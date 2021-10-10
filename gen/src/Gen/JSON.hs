-- Module      : Gen.JSON
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.JSON where

import Control.Error
import Control.Monad.Except
import Data.Aeson hiding (decode)
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as Map
import Data.List
import Gen.IO
import qualified Text.EDE as EDE
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
    EDE.fromValue (toJSON x)

decode :: ByteString -> Either String Object
decode = eitherDecode' . LBS.fromStrict

parse :: FromJSON a => Object -> Either String a
parse = parseEither parseJSON . Object

merge :: [Object] -> Object
merge = foldl' go mempty
  where
    go :: Object -> Object -> Object
    go = Map.unionWith value

    value :: Value -> Value -> Value
    value l r =
      case (l, r) of
        (Object x, Object y) -> Object (x `go` y)
        (_, _) -> l
