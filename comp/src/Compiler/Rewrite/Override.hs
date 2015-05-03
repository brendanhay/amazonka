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
    ( override
    ) where

import           Compiler.Rewrite.Config
import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.CaseInsensitive    as CI
import qualified Data.HashMap.Strict     as Map
import qualified Data.HashSet            as Set
import           Data.Monoid
import           Data.Text               (Text)

-- FIXME: Renaming should additionally operate over
-- the operation input/output.

-- | Apply the override rules to shapes and their respective fields.
override :: Service f Shape Shape -> State Config (Service f Shape Shape)
override svc = do
    cfg <- get
    return $! svc & shapes %~ Map.foldlWithKey' (go cfg) mempty
  where
    go Config{..} acc n = shape renamed replaced o acc n
      where
        o = defaultOverride `fromMaybe` Map.lookup n _typeOverrides

        renamed, replaced :: Map Text Text
        renamed  = vMapMaybe _renamedTo  _typeOverrides
        replaced = vMapMaybe _replacedBy _typeOverrides

    shape renamed replaced o@Override{..} acc n s
        | Map.member n replaced          = acc             -- Replace the type.
        | Just x <- Map.lookup n renamed = shape renamed replaced o acc x s -- Rename the type.
        | otherwise                      = Map.insert n (rules s) acc
      where
        rules = require . optional . rename . retype . prefix

        require  = _Struct . _2 . required %~ (<> _requiredFields)
        optional = _Struct . _2 . required %~ (`Set.difference` _optionalFields)

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

defaultOverride :: Override
defaultOverride = Override
    { _renamedTo      = Nothing
    , _replacedBy     = Nothing
    , _enumPrefix     = Nothing
    , _requiredFields = mempty
    , _optionalFields = mempty
    , _renamedFields  = mempty
    }
