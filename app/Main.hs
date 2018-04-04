{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Lens hiding (argument, (<.>), (.=))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (Options)
import Data.List (intercalate)
import Data.Maybe (isNothing)
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Network.AWS.Data (fromText)
import Network.AWS.S3 hiding (tag)
import Options.Applicative
import System.Environment (getEnv, lookupEnv)
import System.Exit
import System.FilePath ((<.>), takeExtensions)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T

nightlyFolder, releaseFolder :: String
nightlyFolder = "nightly"
releaseFolder = "releases"

main :: IO ()
main = do
  Options {..} <- execParser optionsParserInfo
  optMode <- detectMode
  buildInfo <-
    case optMode of
      CircleCI -> circleCiBuildInfo
      AppVeyor -> appVeyorBuildInfo
  s3Info <- getS3Info
  case s3Info of
    Nothing -> do
      putStrLn "S3 env vars are not set, so do nothing."
      exitSuccess
    Just S3Info {..} -> do
      let echoUploading path key = putStrLn $
            "Uploading: " ++ path ++ " to " ++ unObjectKey key
          uploadSingleFile path key = do
            body <- chunkedFile defaultChunkSize path
            putObjectPublicly key body
          putObjectPublicly key body = void . send $
            putObject s3BucketName key body
              & poACL .~ Just OPublicRead
      withEnv <- (\e m -> runResourceT $ runAWST ((envRegion .~ s3Region) e) m)
        <$> newEnv (FromKeys s3AccessKey s3SecretKey)
      case optAsTool of
        Nothing ->
          case objectKeyFunction buildInfo of
            Left msg -> do
              putStrLn msg
              exitSuccess
            Right objectKeyFn -> do
              forM_
                [(s, f) | s <- [minBound..maxBound]
                        , f <- [minBound..maxBound]] $
                \(stampFlavor, fileFlavor) -> do
                  let okey = objectKeyFn optPath stampFlavor fileFlavor
                  withEnv $ case fileFlavor of
                    Bindist -> do
                      liftIO (echoUploading optPath okey)
                      uploadSingleFile optPath okey
                    Metadata -> do
                      liftIO . putStrLn $
                        "Uploading metadata to " ++ unObjectKey okey
                      putObjectPublicly okey (toBody $ toJSON buildInfo)
        Just name -> withEnv $ do
          let okey = toolObjectKey name
          liftIO (echoUploading optPath okey)
          uploadSingleFile optPath okey
      putStrLn "All done, have a nice day."

----------------------------------------------------------------------------
-- Logic

-- | What sort of stamp to use in object key.

data StampFlavor
  = WithStamp          -- ^ Use time\/SHA1\/tag stamp
  | Latest             -- ^ Label as latest, preserve file extension
  deriving (Enum, Bounded)

-- | File flavor: bindist or metadata.

data FileFlavor
  = Bindist            -- ^ Bindist
  | Metadata           -- ^ Metadata
  deriving (Enum, Bounded)

-- | Get a function that is to be used for 'ObjectKey' generation from
-- 'FilePath' and 'StampFlavor' or the reason why we prefer to do nothing
-- this time.

objectKeyFunction
  :: BuildInfo
  -> Either String (FilePath -> StampFlavor -> FileFlavor -> ObjectKey)
objectKeyFunction BuildInfo {..}
  | biBranch /= "master" && isNothing biTag =
    -- NOTE Heads-up, CircleCI sets branch to empty string when a build is
    -- triggered by a tag push. Strange!
    Left "Non-master branch and no tag, nothing to do."
  | otherwise = Right $ \path stampFlavor fileFlavor ->
      let biDate = formatTime defaultTimeLocale "%d-%m-%Y-" biTime
          fileName =
            case fileFlavor of
              Bindist -> "bindist" <.> takeExtensions path
              Metadata -> "metadata.json"
          typeFolder =
            if isNothing biTag
              then nightlyFolder
              else releaseFolder
          stampFolder =
            case stampFlavor of
              WithStamp ->
                case biTag of
                  Nothing -> biDate ++ biSha1
                  Just tag -> tag
              Latest -> "latest"
      in ObjectKey . T.pack . intercalate "/" $
         [ typeFolder
         , biJob
         , stampFolder
         , fileName
         ]

-- | Generate 'ObjectKey' for tool given its name.

toolObjectKey :: FilePath -> ObjectKey
toolObjectKey name = ObjectKey . T.pack $ "tools/" ++ name

----------------------------------------------------------------------------
-- Command line interface

-- | Command line options.

data Options = Options
  { optAsTool :: Maybe String -- ^ Upload as tool named this
  , optPath   :: FilePath     -- ^ Path to the artifact to save
  }

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser) . mconcat $
  [ fullDesc
  , progDesc "GHC artifactor collector"
  , header "ghc-artifact-collector - collect GHC build artifacts"
  ]

optionsParser :: Parser Options
optionsParser = Options
  <$> (optional . strOption . mconcat)
    [ metavar "TOOL-NAME"
    , long "as-tool"
    , short 't'
    , help "Upload file as a tool with this name."
    ]
  <*> (argument str . mconcat)
    [ metavar "FILE"
    , help "File to store."
    ]

----------------------------------------------------------------------------
-- Mode of operation

-- | Mode of operation.

data Mode = CircleCI | AppVeyor

detectMode :: IO Mode
detectMode = do
  circleCi <- lookupEnv "CIRCLECI"
  appVeyor <- lookupEnv "APPVEYOR"
  case (circleCi, appVeyor) of
    (Just "true", _) -> return CircleCI
    (_, Just "True") -> return AppVeyor
    _                -> do
      putStrLn "Could not detect mode of operation"
      exitFailure

----------------------------------------------------------------------------
-- CI build info

-- | CI build info.

data BuildInfo = BuildInfo
  { biBranch :: !String
  , biJob :: !String
  , biTag :: !(Maybe String)
  , biSha1 :: !String
  , biTime :: !UTCTime
  }

instance ToJSON BuildInfo where
  toJSON BuildInfo {..} = object
    [ "branch" .= biBranch
    , "job"    .= biJob
    , "tag"    .= biTag
    , "sha1"   .= biSha1
    , "time"   .= biTime
    ]

-- | Obtain 'BuildInfo' on Circle CI.

circleCiBuildInfo :: IO BuildInfo
circleCiBuildInfo = do
  -- https://circleci.com/docs/2.0/env-vars/
  biBranch <- getEnv    "CIRCLE_BRANCH"
  biJob    <- getEnv    "CIRCLE_JOB"
  biTag    <- lookupEnv "CIRCLE_TAG"
  biSha1   <- getEnv    "CIRCLE_SHA1"
  biTime   <- getCurrentTime
  return BuildInfo {..}

-- | Obtain 'BuildInfo' on AppVeyor.

appVeyorBuildInfo :: IO BuildInfo
appVeyorBuildInfo = do
  -- https://www.appveyor.com/docs/environment-variables/
  biBranch <- getEnv "APPVEYOR_REPO_BRANCH"
  let biJob = "mingw64"
  -- biJob    <- getEnv "APPVEYOR_JOB_NAME"
  -- XXX For some reason APPVEYOR_JOB_NAME is not actually set.
  hasTag   <- getEnv "APPVEYOR_REPO_TAG"
  biTag    <- if hasTag == "true"
    then Just <$> getEnv "APPVEYOR_REPO_TAG_NAME"
    else return Nothing
  biSha1   <- getEnv "APPVEYOR_REPO_COMMIT"
  biTime   <- getCurrentTime
  return BuildInfo {..}

----------------------------------------------------------------------------
-- S3 info

-- | Information that is necessary to upload a file to S3.

data S3Info = S3Info
  { s3AccessKey :: !AccessKey -- ^ AWS access key
  , s3SecretKey :: !SecretKey -- ^ AWS secret key
  , s3BucketName :: !BucketName -- ^ Bucket name
  , s3Region :: !Region -- ^ Bucket Region
  }

-- | Obtain 'S3Info' from environment variables. If at least one such
-- variable is not set, return 'Nothing'.

getS3Info :: IO (Maybe S3Info)
getS3Info = runMaybeT $ do
  let grabEnv = MaybeT . lookupEnv
  s3AccessKey <- AccessKey . B8.pack <$>
    grabEnv "GHC_ARTIFACT_COLLECTOR_ACCESS_KEY"
  s3SecretKey <- SecretKey . B8.pack <$>
    grabEnv "GHC_ARTIFACT_COLLECTOR_SECRET_KEY"
  s3BucketName <- BucketName . T.pack <$>
    grabEnv "GHC_ARTIFACT_COLLECTOR_BUCKET_NAME"
  s3Region <- do
    rawRegion <- T.pack <$> grabEnv "GHC_ARTIFACT_COLLECTOR_REGION"
    MaybeT . return $ case fromText rawRegion of
      Left  _ -> Nothing
      Right x -> Just x
  return S3Info {..}

----------------------------------------------------------------------------
-- Misc

unObjectKey :: ObjectKey -> String
unObjectKey = T.unpack . view _ObjectKey
