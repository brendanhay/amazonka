-- |
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

import qualified Control.Lens as Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Tuple as Tuple
import Gen.Prelude

vMapMaybe ::
  (Eq k, Hashable k) =>
  (a -> Maybe b) ->
  HashMap k a ->
  HashMap k b
vMapMaybe f =
  runIdentity
    . kvTraverseMaybe (const (pure . f))

kvInvert :: (Eq v, Hashable v) => HashMap k v -> HashMap v k
kvInvert = kvTraversal %~ Tuple.swap

kvTraverseMaybe ::
  (Applicative f, Eq k, Hashable k) =>
  (k -> a -> f (Maybe b)) ->
  HashMap k a ->
  f (HashMap k b)
kvTraverseMaybe f =
  fmap (HashMap.map fromJust . HashMap.filter isJust)
    . HashMap.traverseWithKey f

kvTraversal ::
  (Eq k', Hashable k') =>
  Lens.Traversal (HashMap k v) (HashMap k' v') (k, v) (k', v')
kvTraversal f =
  fmap HashMap.fromList
    . traverse f
    . HashMap.toList
