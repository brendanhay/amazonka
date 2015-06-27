{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.JSON
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.JSON where

import           Gen.Formatting
import           Gen.IO
import           Gen.Types
import           Control.Error
import           Control.Monad.Except
import           Data.Aeson           hiding (decode)
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as Map
import           Data.List
import qualified Data.Text.Lazy       as LText
import qualified Text.EDE             as EDE

required :: MonadIO m => Path -> ExceptT Error m Object
required = readBSFile >=> hoistEither . decode

optional :: MonadIO m => Path -> ExceptT Error m Object
optional f = readBSFile f `catchError` const (return "{}")
    >>= hoistEither . decode

objectErr :: ToJSON a => String -> a -> Either Error Object
objectErr n =
      note (format ("Failed to extract JSON object from value " % string) n)
    . EDE.fromValue
    . toJSON

decode :: ByteString -> Either Error Object
decode = first LText.pack . eitherDecode' . LBS.fromStrict

parse :: FromJSON a => Object -> Either Error a
parse = first LText.pack . parseEither parseJSON . Object

merge :: [Object] -> Object
merge = foldl' go mempty
  where
    go :: Object -> Object -> Object
    go a b = Map.unionWith value a b

    value :: Value -> Value -> Value
    value l r =
        case (l, r) of
            (Object x, Object y) -> Object (x `go` y)
            (_,        _)        -> l
