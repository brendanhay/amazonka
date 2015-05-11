{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Compiler.Override
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Override
    ( override
    ) where

import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Data.Bifunctor
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import           Data.Monoid

-- | Apply the override rules to shapes and their respective fields.
override :: Functor f
         => Config
         -> Service f (RefF a) (ShapeF b)
         -> Service f (RefF a) (ShapeF b)
override Config{..} svc@Service{..} = svc
    & operations . each %~ operation
    & shapes            %~ Map.foldlWithKey' shape mempty
  where
    operation :: Functor f
              => Operation f (RefF a)
              -> Operation f (RefF a)
    operation o = o
        { _opInput  = ref <$> _opInput  o
        , _opOutput = ref <$> _opOutput o
        }
      where
        ref :: RefF a -> RefF a
        ref r
            | Just x <- ptr renamed  = r & refShape .~ x
            | Just x <- ptr replaced = r & refShape .~ x ^. replaceName
            | otherwise              = r
          where
            ptr = Map.lookup (r ^. refShape)

    shape :: Map Id (ShapeF a) -> Id -> ShapeF a -> Map Id (ShapeF a)
    shape acc n s
        | Map.member n replaced          = acc           -- Replace the type.
        | Just x <- Map.lookup n renamed = shape acc x s -- Rename the type.
        | otherwise                      = Map.insert n (rules s) acc
      where
        Override{..} = fromMaybe defaultOverride $
             Map.lookup n _typeOverrides

        rules :: ShapeF a -> ShapeF a
        rules = require . optional . rename . retype . prefix

        require, optional :: ShapeF a -> ShapeF a
        require  = _Struct._2.required %~ (<> _requiredFields)
        optional = _Struct._2.required %~ (`Set.difference` _optionalFields)

        rename :: ShapeF a -> ShapeF a
        rename = _Struct._2.members.kvTraversal %~ first f
          where
            f k = fromMaybe k (Map.lookup k _renamedFields)

        retype :: ShapeF a -> ShapeF a
        retype = references %~ f _replaceName replaced . f id renamed
          where
            f g m v = maybe v (flip (set refShape) v . g) $
                Map.lookup (v ^. refShape) m

        prefix :: ShapeF a -> ShapeF a
        prefix
            | Just p <- _enumPrefix = _Enum._2.kvTraversal %~ first (prependId p)
            | otherwise             = id

    renamed :: Map Id Id
    renamed = vMapMaybe _renamedTo  _typeOverrides

    replaced :: Map Id Replace
    replaced = vMapMaybe _replacedBy _typeOverrides

defaultOverride :: Override
defaultOverride = Override
    { _renamedTo      = Nothing
    , _replacedBy     = Nothing
    , _enumPrefix     = Nothing
    , _requiredFields = mempty
    , _optionalFields = mempty
    , _renamedFields  = mempty
    }
