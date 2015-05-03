{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Compiler.Rewrite.Acronym
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Acronym where

import           Compiler.AST
import           Compiler.Text
import           Compiler.Types
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer.Strict
import           Data.Foldable               (traverse_)
import qualified Data.HashMap.Strict         as Map
import qualified Data.HashSet                as Set
import           Data.Maybe
import           Data.Text                   (Text)

recase :: Config -> Map Text (Shape f) -> Config
recase cfg = flip (merge renamedTo) cfg . renamed
  where
    renamed = mapMaybe f . Map.toList
      where
        f (k, v)
            | k /= k'   = Just (k, k')
            | otherwise = Nothing
          where
            k' = upperAcronym k

-- need to add operations to prefixes
