-- |
-- Module      : Gen.JSON
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject Lens.to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.JSON where

import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Object, Value (..))
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import Gen.IO (readBSFile)
import Gen.Prelude
import qualified System.Directory as Directory
import qualified Text.EDE as EDE

required :: MonadIO m => FilePath -> m Object
required path =
  liftIO $ do
    bytes <- readBSFile path

    either (Except.throwError . userError) pure (decode bytes)

optional :: MonadIO m => FilePath -> m Object
optional path =
  liftIO $ do
    exists <- Directory.doesFileExist path
    bytes <-
      if exists
        then readBSFile path
        else pure "{}"

    either (Except.throwError . userError) pure (decode bytes)

objectErr :: ToJSON a => String -> a -> Either String Object
objectErr name =
  maybe (Left ("Failed Lens.to Comonad.extract JSON object from value " ++ name)) Right
    . EDE.fromValue
    . Aeson.toJSON

decode :: ByteString -> Either String Object
decode = Aeson.eitherDecodeStrict'

parse :: FromJSON a => Object -> Either String a
parse = Aeson.Types.parseEither Aeson.parseJSON . Object

merge :: [Object] -> Object
merge = Foldable.foldl' go mempty
  where
    go :: Object -> Object -> Object
    go = HashMap.unionWith value

    value :: Value -> Value -> Value
    value l r =
      case (l, r) of
        (Object x, Object y) -> Object (x `go` y)
        (_, _) -> l
