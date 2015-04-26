{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Compiler.Rewrite.Override
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Override
    ( overrides
    ) where

import           Compiler.AST
import           Compiler.Text
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Data.Bifunctor
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Foldable        (foldl')
import qualified Data.HashMap.Strict  as Map
import qualified Data.HashSet         as Set
import           Data.List            (intercalate)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate

-- FIXME: Renaming should additionally operate over
-- the operation input/output.

overrides :: Map Text Override
          -> Map Text (Shape Identity)
          -> Map Text (Shape Identity)
overrides ov = Map.foldlWithKey' go mempty
  where
    go acc k = shape (defaultOverride (Map.lookup k ov)) acc k

    shape :: Override
          -> Map Text (Shape Identity)
          -> Text
          -> Shape Identity
          -> Map Text (Shape Identity)
    shape o@Override{..} acc k s
        | Map.member k replaced          = acc             -- Replace the type.
        | Just x <- Map.lookup k renamed = shape o acc x s -- Rename the type.
        | otherwise                      = Map.insert k (rules s) acc
      where
        rules = require . optional . rename . retype . prefix

        require  = _Struct . _3 %~ (<> _requiredFields)
        optional = _Struct . _3 %~ (`Set.difference` _optionalFields)

        rename = fields %~ first f
          where
            f k = fromMaybe k $
                Map.lookup (CI.mk k) _renamedFields

        retype = references %~ f replaced . f renamed
          where
            f m v = maybe v (flip (set refShape) v) $
                Map.lookup (v ^. refShape) m

        prefix
            | Just p <- _enumPrefix = values %~ first (mappend p)
            | otherwise             = id

    renamed, replaced :: Map Text Text
    renamed  = maybeMap _renamedTo  ov
    replaced = maybeMap _replacedBy ov

defaultOverride :: Maybe Override -> Override
defaultOverride = fromMaybe $ Override
    { _renamedTo      = Nothing
    , _replacedBy     = Nothing
    , _enumPrefix     = Nothing
    , _requiredFields = mempty
    , _optionalFields = mempty
    , _renamedFields  = mempty
    }
