{-# LANGUAGE OverloadedStrings #-}

import qualified Amazonka
import qualified Amazonka.Auth
import qualified Amazonka.S3 as S3
import Control.Lens ((.~))
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as BC8
import Data.String (fromString)
import qualified Data.Text as T
import qualified Network.URL as URL
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  [fp, endpointStr, bucketNameStr, objectKeyStr] <- getArgs
  rqBody <- Amazonka.chunkedFile (64 * 1024) fp
  defEnv <- Amazonka.newEnv Amazonka.Auth.fromKeysEnv
  let Just endpoint = parseEndpoint endpointStr

      env = Amazonka.override (Amazonka.serviceEndpoint .~ endpoint) defEnv

      bucketName :: S3.BucketName
      bucketName = fromString bucketNameStr

      objectKey :: S3.ObjectKey
      objectKey = fromString objectKeyStr

      putObject :: S3.PutObject
      putObject = S3.newPutObject bucketName objectKey rqBody

  pPrint endpoint
  pPrint putObject

  _resp <- Amazonka.runResourceT $ Amazonka.send env putObject
  return ()

parseEndpoint :: String -> Maybe Amazonka.Endpoint
parseEndpoint s = do
  url <- URL.importURL s
  host <- case URL.url_type url of
    URL.Absolute host -> pure host
    _ -> mzero
  secure <- case URL.protocol host of
    URL.HTTP secure -> pure secure
    _ -> mzero
  pure
    Amazonka.Endpoint
      { Amazonka._endpointHost = BC8.pack $ URL.host host,
        Amazonka._endpointSecure = secure,
        Amazonka._endpointPort =
          case URL.port host of
            Just port -> fromInteger port
            Nothing
              | secure -> 443
              | otherwise -> 80,
        Amazonka._endpointScope = "us-east-1"
      }
