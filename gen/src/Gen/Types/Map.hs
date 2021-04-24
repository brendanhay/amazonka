-- Module      : Gen.Types.Map
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Types.Map where

import Control.Lens
import Data.HashMap.Strict qualified as Map
import Data.Hashable
import Data.Maybe
import Data.Tuple

type Map = Map.HashMap

vMapMaybe ::
  (Eq k, Hashable k) =>
  (a -> Maybe b) ->
  Map k a ->
  Map k b
vMapMaybe f = runIdentity . kvTraverseMaybe (const (pure . f))

kvInvert :: (Eq v, Hashable v) => Map k v -> Map v k
kvInvert = kvTraversal %~ swap

kvTraverseMaybe ::
  (Applicative f, Eq k, Hashable k) =>
  (k -> a -> f (Maybe b)) ->
  Map k a ->
  f (Map k b)
kvTraverseMaybe f =
  fmap (Map.map fromJust . Map.filter isJust)
    . Map.traverseWithKey f

kvTraversal ::
  (Eq k', Hashable k') =>
  Traversal (Map k v) (Map k' v') (k, v) (k', v')
kvTraversal f = fmap Map.fromList . traverse f . Map.toList
