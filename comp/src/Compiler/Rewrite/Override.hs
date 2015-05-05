{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

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

import           Compiler.Types
import           Control.Error
import           Control.Lens
import           Data.Bifunctor
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import           Data.Monoid

-- | Apply the override rules to shapes and their respective fields.
override :: Functor f => Config -> Service f Ref Shape -> Service f Ref Shape
override Config{..} svc@Service{..} = svc
    { _operations = Map.map oper _operations
    , _shapes     = Map.foldlWithKey' go mempty _shapes
    }
  where
    oper o = o
        { _opInput  = ref <$> _opInput  o
        , _opOutput = ref <$> _opOutput o
        }

    ref r | Just x <- Map.lookup (r ^. refShape) renamed  = r & refShape .~ x
          | Just x <- Map.lookup (r ^. refShape) replaced = r & refShape .~ _replaceName x
          | otherwise = r

    go :: Map Id (Shape f) -> Id -> Shape f -> Map Id (Shape f)
    go acc n s
        | Map.member n replaced          = acc        -- Replace the type.
        | Just x <- Map.lookup n renamed = go acc x s -- Rename the type.
        | otherwise                      = Map.insert n (rules s) acc
      where
        Override{..} = defaultOverride `fromMaybe` Map.lookup n _typeOverrides

        rules = require . optional . rename . retype . prefix

        require  = _Struct._2.required %~ (<> _requiredFields)
        optional = _Struct._2.required %~ (`Set.difference` _optionalFields)

        rename = _Struct._2.members.kvTraversal %~ first f
          where
            f k = fromMaybe k (Map.lookup k _renamedFields)

        retype = references %~ f _replaceName replaced . f id renamed
          where
            f g m v = maybe v (flip (set refShape) v . g) $
                Map.lookup (v ^. refShape) m

        prefix
            | Just p <- _enumPrefix = _Enum._2.kvTraversal %~ first (p <>)
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
