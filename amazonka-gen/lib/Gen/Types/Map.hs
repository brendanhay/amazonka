{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Gen.Types.Map
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Types.Map where

import qualified Control.Lens as Lens
import Data.Functor.Classes (Show1 (liftShowsPrec))
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Hashable as Hashable
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import Gen.Prelude

instance Show k => Show1 (InsOrdHashMap k) where
  liftShowsPrec sv slv d = liftShowsPrec sv slv d . HashMap.toHashMap

vMapMaybe ::
  (Eq k, Hashable k) =>
  (a -> Maybe b) ->
  InsOrdHashMap k a ->
  InsOrdHashMap k b
vMapMaybe f = runIdentity . kvTraverseMaybe (const (pure . f))

-- kvInvert :: (Eq v, Hashable v) => InsOrdHashMap k v -> InsOrdHashMap v k
-- kvInvert = kvTraversal %~ Tuple.swap

kvTraverseMaybe ::
  (Applicative f, Eq k, Hashable k) =>
  (k -> a -> f (Maybe b)) ->
  InsOrdHashMap k a ->
  f (InsOrdHashMap k b)
kvTraverseMaybe f =
  fmap (HashMap.map fromJust . HashMap.filter isJust)
    . HashMap.traverseWithKey f

kvTraversal ::
  (Eq k', Hashable k') =>
  Traversal (InsOrdHashMap k v) (InsOrdHashMap k' v') (k, v) (k', v')
kvTraversal f = fmap HashMap.fromList . traverse f . HashMap.toList
