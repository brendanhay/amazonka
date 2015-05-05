{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Compiler.Formatting
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Formatting
    ( module Compiler.Formatting
    , module Formatting
    , module Formatting.Time
    , runFormat
    ) where

import qualified Data.HashMap.Strict    as Map
import           Compiler.Types
import           Control.Monad.Except
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy.Builder as Build
import           Formatting             hiding (left, right)
import           Formatting.Internal    (runFormat)
import           Formatting.Time

scomma :: Format a ([Text] -> a)
scomma = later (Build.fromText . Text.intercalate ",")

path :: Format a (Path -> a)
path = later (Build.fromText . toTextIgnore)

partial :: Show b => Format a ((Text, Map.HashMap Text b) -> a)
partial = later (Build.fromString . show . Map.toList . prefix)
  where
    prefix (Text.take 3 -> p, m) =
        Map.filterWithKey (const . Text.isPrefixOf p) m

failure :: MonadError e m => Format LazyText (a -> e) -> a -> m b
failure m = throwError . format m
