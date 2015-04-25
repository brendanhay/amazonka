{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- Module      : Compiler.Rewrite.Override
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Override where

import           Compiler.Model             hiding (Name, State)
import           Compiler.OrdMap            (OrdMap)
import qualified Compiler.OrdMap            as OrdMap
import           Compiler.Text              (safeHead)
import           Compiler.Types             hiding (override)
import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Bifunctor
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.Default.Class
import           Data.Foldable              (foldl')
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
import           Data.List                  (intercalate)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Manipulate
import           Debug.Trace

-- FIXME: Renaming should additionally operate over
-- the operation input/output.

applyOverrides :: Rules
               -> Map Text (Shape Identity)
               -> Map Text (Shape Identity)
applyOverrides api = Map.foldlWithKey' go mempty
  where

-- overrideShapes

-- overrideFields

    go acc n = shape (fromMaybe def (Map.lookup n o)) acc n

    shape :: Rules
          -> Map Text (Untyped Shape)
          -> Text
          -> (Untyped Shape)
          -> Map Text (Untyped Shape)
    shape rs acc n s
        | Map.member n replacedBy          = acc
        | Just x <- Map.lookup n renamedTo = shape rs acc x s
        | otherwise                        = Map.insert n (rules s) acc
      where
        rules = requireFields
              . optionalFields
              . renameFields
              . retypeFields
              . prefixEnum
              . appendEnum

        requireFields :: Untyped Shape -> Untyped Shape
        requireFields = _SStruct . structRequired
            %~ (<> _ruleRequired rs)

        optionalFields = _SStruct . structRequired
            %~ (`Set.difference` _ruleOptional rs)

        renameFields :: Untyped Shape -> Untyped Shape
        renameFields = _SStruct . structMembers %~ first f
          where
            f k = fromMaybe k $ do
                k' <- Map.lookup (CI.mk (k ^. memOriginal)) (_ruleRenamed rs)
                return (k & memName .~ k')

        retypeFields :: Untyped Shape -> Untyped Shape
        retypeFields = references %~ f replacedBy . f renamedTo
          where
            f m v = maybe v (\x -> v & refShape .~ x)
                $ Map.lookup (v ^. refShape) m

        prefixEnum :: Untyped Shape -> Untyped Shape
        prefixEnum = _SEnum . enumValues %~ f
          where
            f vs = fromMaybe vs $ do
                p <- _ruleEnumPrefix rs
                return $! first (memPrefix ?~ p) vs

        appendEnum :: Untyped Shape -> Untyped Shape
        appendEnum = _SEnum . enumValues <>~ _ruleEnumValues rs

    renamed, replaced :: Map Text Text
    renamed  = buildMapping _renameTo
    replaced = buildMapping _replacedBy

buildMapping :: (Rules -> Maybe Text) -> Map Text Text
buildMapping f = Map.fromList $
    mapMaybe (\(k, v) -> (k,) <$> f v) (Map.toList o)
