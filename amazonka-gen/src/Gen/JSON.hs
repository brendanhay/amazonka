{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Gen.JSON
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.JSON where

import qualified Control.Error as Error
import qualified Control.Exception as Exception
import Control.Monad ((>=>))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson hiding (decode)
import Data.Aeson.Types
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Data.Text.Lazy as LText
import Gen.IO
import Gen.Types
import qualified Text.EDE as EDE

required :: MonadIO m => FilePath -> m Object
required path =
  liftIO $ do
    bytes <- readBSFile path

    either (Except.throwError . userError) pure (decode bytes)

optional :: MonadIO m => FilePath -> m Object
optional path =
  liftIO $ do
    bytes <-
      Exception.try (readBSFile path) <&> \case
        Left (_ :: Exception.IOException) -> "{}"
        Right ok -> ok

    either (Except.throwError . userError) pure (decode bytes)

objectErr :: ToJSON a => String -> a -> Either String Object
objectErr name =
  maybe (Left ("Failed to extract JSON object from value " ++ name)) Right
    . EDE.fromValue
    . toJSON

decode :: ByteString -> Either String Object
decode = eitherDecode' . LBS.fromStrict

parse :: FromJSON a => Object -> Either String a
parse = parseEither parseJSON . Object

merge :: [Object] -> Object
merge = Foldable.foldl' go mempty
  where
    go :: Object -> Object -> Object
    go = Map.unionWith value

    value :: Value -> Value -> Value
    value l r =
      case (l, r) of
        (Object x, Object y) -> Object (x `go` y)
        (_, _) -> l
