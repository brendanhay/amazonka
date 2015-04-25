{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           Compiler.Orphans          ()
import           Control.Error
import           Control.Monad
import qualified Data.HashMap.Strict       as Map
import           Data.List                 (intersperse)
import           Data.Monoid
import qualified Data.SemVer               as SemVer
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as Build
import           Data.Time
import qualified Filesystem.Path.CurrentOS as Path
import           Formatting
import           Formatting.Time
import           Text.EDE                  (Template)

type Compiler = EitherT LazyText
type LazyText = LText.Text

type Map = Map.HashMap

joinMap :: [Text] -> Map Text Text
joinMap = Map.fromList . map (join (,))

type Path = Path.FilePath

path :: Format a (Path -> a)
path = later (Build.fromText . toTextIgnore)

toTextIgnore :: Path -> Text
toTextIgnore = either id id . Path.toText

type SemVer = SemVer.Version

semver :: Format a (SemVer -> a)
semver = later (Build.fromText . SemVer.toText)

data Templates = Templates
    { cabalTemplate           :: Template
    , serviceTemplate         :: Template
    , waitersTemplate         :: Template
    , readmeTemplate          :: Template
    , exampleCabalTemplate    :: Template
    , exampleMakefileTemplate :: Template
    , operationTemplate       :: Template
    , typesTemplate           :: Template
    }

dateDashes :: Format a ([UTCTime] -> a)
dateDashes = later (list . map (bprint dateDash))
  where
    list = ("[" <>) . (<> "]") . mconcat . intersperse ","

failure :: Monad m => Format LText.Text (a -> e) -> a -> EitherT e m b
failure m = Control.Error.left . format m
