{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Compiler.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types where

import           Control.Error
import qualified Data.ByteString.Lazy      as LBS
import           Data.List                 (intersperse)
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as Build
import           Data.Time
import qualified Filesystem.Path.CurrentOS as Path
import           Formatting
import           Formatting.Time

type LazyText       = LText.Text
type LazyByteString = LBS.ByteString

type Path = Path.FilePath

path :: Format a (Path -> a)
path = later (Build.fromText . toTextIgnore)

toTextIgnore :: Path -> Text
toTextIgnore = either id id . Path.toText

data Dir = Dir Path [Path] deriving (Show)

dateDashes :: Format a ([UTCTime] -> a)
dateDashes = later (list . map (bprint dateDash))
  where
    list = ("[" <>) . (<> "]") . mconcat . intersperse ","

failure :: Monad m => Format LText.Text (a -> e) -> a -> EitherT e m b
failure m = Control.Error.left . format m
