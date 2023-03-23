module Gen.JSON where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Aeson.Types (Object, Value (..))
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Gen.IO
import Gen.Prelude
import qualified UnliftIO
import qualified UnliftIO.Directory as UnliftIO

required :: MonadIO m => FilePath -> m Object
required path = do
  bytes <- readBSFile path
  case decode bytes of
    Left er -> UnliftIO.throwString (show er)
    Right ok -> pure ok

optional :: MonadIO m => FilePath -> m Object
optional path = do
  exists <- UnliftIO.doesFileExist path
  if exists
    then required path
    else pure mempty

objectErr :: ToJSON a => String -> a -> Either String Object
objectErr n x =
  note ("Failed to extract JSON object from value " ++ n) $
    case Aeson.toJSON x of
      Object m -> Just m
      _other -> Nothing

decode :: ByteString -> Either String Object
decode = Aeson.eitherDecode' . ByteString.Lazy.fromStrict

parse :: FromJSON a => Object -> Either String a
parse = Aeson.Types.parseEither Aeson.parseJSON . Object

merge :: [Object] -> Object
merge = foldr go mempty
  where
    go :: Object -> Object -> Object
    go = Aeson.KeyMap.unionWith value

    value :: Value -> Value -> Value
    value l r =
      case (l, r) of
        (Object x, Object y) -> Object (x `go` y)
        (_, _) -> l
