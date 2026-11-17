module Gen.Types.Map where

import qualified Control.Lens as Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
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

kvInvert :: (Ord k, Eq v, Hashable v) => HashMap k v -> HashMap v k
kvInvert = kvTraversal %~ Tuple.swap

kvTraverseMaybe ::
  (Applicative f, Eq k, Hashable k) =>
  (k -> a -> f (Maybe b)) ->
  HashMap k a ->
  f (HashMap k b)
kvTraverseMaybe f =
  fmap (HashMap.map fromJust . HashMap.filter isJust)
    . HashMap.traverseWithKey f

-- | Traverses in key order, so that keys which collide after mapping resolve
-- to the same entry regardless of the hash order.
kvTraversal ::
  (Ord k, Eq k', Hashable k') =>
  Lens.Traversal (HashMap k v) (HashMap k' v') (k, v) (k', v')
kvTraversal f =
  fmap HashMap.fromList
    . traverse f
    . List.sortOn fst
    . HashMap.toList
