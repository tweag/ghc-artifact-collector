{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.Trans.AWS
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Network.AWS.Data (fromText)
import Network.AWS.S3 (BucketName (..), ObjectKey (..), putObject)
import Options.Applicative
import System.Environment (getEnv, lookupEnv)
import System.Exit
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T

nightlyFolder, ltsFolder :: String
nightlyFolder = "nightly"
ltsFolder     = "lts"

main :: IO ()
main = do
  Options {..} <- execParser optionsParserInfo
  buildInfo <-
    case optMode of
      CircleCI -> circleCiBuildInfo
      AppVeyor -> appVeyorBuildInfo
  case objectKeyFunction buildInfo of
    Left msg -> do
      putStrLn msg
      exitSuccess
    Right objectKeyForPath -> do
      S3Info {..} <- getS3Info
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
        Nothing -> [nightlyFolder, biSha1, path]
        -- This commit has a tag on it, so it's a release and we should
        -- store it in a long-term folder.
        Just tag -> [ltsFolder, tag, path]

----------------------------------------------------------------------------
-- Command line interface

-- | Command line options.

data Options = Options
  { optMode :: Mode     -- ^ Mode of operation
  , optPaths :: [FilePath] -- ^ Path to the artifact to save
  }

-- | Mode of operation.

data Mode = CircleCI | AppVeyor

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (helper <*> optionsParser) . mconcat $
  [ fullDesc
  , progDesc "GHC artifactor collector"
  , header "ghc-artifact-collector - collect GHC build artifacts"
  ]

optionsParser :: Parser Options
optionsParser = Options
  <$> argument parseMode
  ( metavar "MODE"
  <> help "Mode of operation: circleci or appveyor" )
  <*> some (argument str (metavar "FILES" <> help "Files to store."))

parseMode :: ReadM Mode
parseMode = eitherReader $ \s ->
  case s of
    "circleci" -> Right CircleCI
    "appveyor" -> Right AppVeyor
    other      -> Left ("unknown mode: \"" ++ other ++ "\"")

----------------------------------------------------------------------------
-- CI build info

-- | CI build info.

data BuildInfo = BuildInfo
  { biBranch :: !String
  , biJob :: !String
  , biTag :: !(Maybe String)
  , biSha1 :: !String
  }

-- | Obtain 'BuildInfo' on Circle CI.

circleCiBuildInfo :: IO BuildInfo
circleCiBuildInfo = do
  -- https://circleci.com/docs/2.0/env-vars/
  biBranch <- getEnv    "CIRCLE_BRANCH"
  biJob    <- getEnv    "CIRCLE_JOB"
  biTag    <- lookupEnv "CIRCLE_TAG"
  biSha1   <- getEnv    "CIRCLE_SHA1"
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

-- | Obtain 'S3Info' from environment variables.

getS3Info :: IO S3Info
getS3Info = do
  s3AccessKey <- AccessKey . B8.pack <$>
    getEnv "GHC_ARTIFACT_COLLECTOR_ACCESS_KEY"
  s3SecretKey <- SecretKey . B8.pack <$>
    getEnv "GHC_ARTIFACT_COLLECTOR_SECRET_KEY"
  s3BucketName <- BucketName . T.pack <$>
    getEnv "GHC_ARTIFACT_COLLECTOR_BUCKET_NAME"
  s3Region <- do
    rawRegion <- T.pack <$> getEnv "GHC_ARTIFACT_COLLECTOR_REGION"
    case fromText rawRegion of
      Left err -> do
        putStrLn err
        exitFailure
      Right x -> return x
  return S3Info {..}
