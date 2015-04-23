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

import           Control.Error
import           Control.Monad
import qualified Data.Aeson                as A
import qualified Data.HashMap.Strict       as Map
import qualified Data.Jason                as J
import qualified Data.Jason.Types          as J
import           Data.List                 (intersperse)
import           Data.Monoid
import           Data.Scientific           (floatingOrInteger)
import qualified Data.SemVer               as SemVer
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as Build
import           Data.Time
import qualified Filesystem.Path.CurrentOS as Path
import           Formatting
import           Formatting.Time
import           Numeric.Natural
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

dateDashes :: Format a ([UTCTime] -> a)
dateDashes = later (list . map (bprint dateDash))
  where
    list = ("[" <>) . (<> "]") . mconcat . intersperse ","

failure :: Monad m => Format LText.Text (a -> e) -> a -> EitherT e m b
failure m = Control.Error.left . format m

type SemVer = SemVer.Version

instance A.ToJSON SemVer where
    toJSON = A.toJSON . SemVer.toText

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

instance J.FromJSON Natural where
    parseJSON = J.withScientific "natural" (f . floatingOrInteger)
      where
        f :: Either Double Integer -> J.Parser Natural
        f (Left  e)     = fail ("Double when expecting Natural: " ++ show e)
        f (Right x)
            | x < 0     = fail ("Negative when expecting Natural: " ++ show x)
            | otherwise = pure (fromInteger x)

instance A.ToJSON Natural where
    toJSON = A.toJSON . toInteger
