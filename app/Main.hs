{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Maybe
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import Network.AWS.Data (fromText)
import Network.AWS.S3 (BucketName (..), ObjectKey (..), putObject)
import Options.Applicative
import System.Environment (getEnv, lookupEnv)
import System.Exit
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
  case (objectKeyFunction buildInfo, s3Info) of
    (Left msg, _) -> do
      putStrLn msg
      exitSuccess
    (_, Nothing) -> do
      putStrLn "S3 env vars are not set, so do nothing."
      exitSuccess
    (Right objectKeyForPath, Just S3Info {..}) -> do
      env <- (envRegion .~ s3Region) <$>
        newEnv (FromKeys s3AccessKey s3SecretKey)
      forM_ optPaths $ \path -> do
        putStrLn ("Uploading: " ++ path)
        runResourceT . runAWST env $ do
          body <- chunkedFile defaultChunkSize path
          send (putObject s3BucketName (objectKeyForPath path) body)
      putStrLn "All done, have a nice day."

----------------------------------------------------------------------------
-- Logic

-- | Get a function that is to be used for 'ObjectKey' generation from
-- 'FilePath's or the reason why we prefer to do nothing this time.

objectKeyFunction
  :: BuildInfo
  -> Either String (FilePath -> ObjectKey)
objectKeyFunction BuildInfo {..}
  | biBranch /= "master" = Left "Non-master branch, nothing to do."
  | otherwise = Right $ \path -> ObjectKey . T.pack . intercalate "/" $
      case biTag of
        -- No tag, just a nightly build. Identified by SHA1 of commit.
        Nothing -> [nightlyFolder, biJob, biDate ++ biSha1, path]
        -- This commit has a tag on it, so it's a release and we should
        -- store it in a long-term folder.
        Just tag -> [releaseFolder, biJob, tag, path]
  where
    biDate = formatTime defaultTimeLocale "%d-%m-%Y-" biTime

----------------------------------------------------------------------------
-- Command line interface

-- | Command line options.

data Options = Options
  { optPaths :: [FilePath] -- ^ Path to the artifact to save
  }

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser) . mconcat $
  [ fullDesc
  , progDesc "GHC artifactor collector"
  , header "ghc-artifact-collector - collect GHC build artifacts"
  ]

optionsParser :: Parser Options
optionsParser = Options
  <$> some (argument str (metavar "FILES" <> help "Files to store."))

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
  biJob    <- getEnv "APPVEYOR_JOB_NAME"
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
