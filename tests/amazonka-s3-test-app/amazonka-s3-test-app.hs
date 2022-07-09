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

main :: IO ()
main = do
  [fp, endpointStr, bucket, key] <- getArgs
  defEnv <- Amazonka.newEnv Amazonka.Auth.fromKeysEnv
  let Just endpoint = parseEndpoint endpointStr
  let env = Amazonka.override (Amazonka.serviceEndpoint .~ endpoint) defEnv
  rqBody <- Amazonka.chunkedFile (64 * 1024) fp
  _resp <-
    Amazonka.runResourceT $
      Amazonka.send env $
        S3.newPutObject (fromString bucket) (S3.ObjectKey $ T.pack key) rqBody
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
