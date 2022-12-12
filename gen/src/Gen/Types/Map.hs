module Gen.Types.Map where

import qualified Control.Lens as Lens
import qualified Data.Map.Strict as Map
import qualified Data.Tuple as Tuple
import Gen.Prelude

vMapMaybe ::
  (Ord k) =>
  (a -> Maybe b) ->
  Map k a ->
  Map k b
vMapMaybe f =
  runIdentity
    . kvTraverseMaybe (const (pure . f))

kvInvert :: (Ord v) => Map k v -> Map v k
kvInvert = kvTraversal %~ Tuple.swap

kvTraverseMaybe ::
  (Applicative f, Ord k) =>
  (k -> a -> f (Maybe b)) ->
  Map k a ->
  f (Map k b)
kvTraverseMaybe f =
  fmap (Map.map fromJust . Map.filter isJust)
    . Map.traverseWithKey f

kvTraversal ::
  (Ord k') =>
  Lens.Traversal (Map k v) (Map k' v') (k, v) (k', v')
kvTraversal f = fmap Map.fromList . traverse f . Map.toList
