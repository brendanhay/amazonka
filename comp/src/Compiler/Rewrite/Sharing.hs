{-# LANGUAGE ViewPatterns #-}

-- Module      : Compiler.Rewrite.Sharing
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Rewrite.Sharing where

import           Compiler.AST
import           Compiler.Types
import           Control.Lens
import           Control.Monad.State
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import           Data.Text           (Text)

type Count = State (Map Text Int)

-- | Determine the usage of operation input/output shapes.
--
-- A shape is considered 'shared' if it is used as a field of another shape,
-- as opposed to only being referenced by the operation itself.
sharing :: Map Text (Operation Identity Ref)
        -> Map Text (Shape Identity)
        -> Set Text
sharing os ss = Set.fromList . Map.keys . Map.filter (> 1) $ execState go mempty
  where
    -- FIXME: Need to correctly count a shape being used as a ref as shared.
    go :: Count ()
    go = forM_ (Map.elems os) $ \o -> do
        ref (o ^. opInput  . _Identity . refShape)
        ref (o ^. opOutput . _Identity . refShape)

    ref :: Text -> Count ()
    ref n = incr n >> maybe (pure ()) shape (Map.lookup n ss)

    shape :: Shape Identity -> Count ()
    shape = mapM_ (incr . view refShape) . toListOf references

    incr :: Text -> Count ()
    incr n  = modify (Map.insertWith (+) n 1)
