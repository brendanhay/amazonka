-- Module      : Compiler.Types.Map
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Types.Map where

import           Control.Lens
import           Control.Monad
import           Data.Hashable
import qualified Data.HashMap.Strict as Map
import           Data.Maybe

type Map = Map.HashMap

kvJoin :: (Eq a, Hashable a) => [a] -> Map a a
kvJoin = Map.fromList . map (join (,))

vMapMaybe :: (Eq k, Hashable k)
          => (a -> Maybe b)
          -> Map k a
          -> Map k b
vMapMaybe f = runIdentity . kvTraverseMaybe (const (pure . f))

kvTraverseMaybe :: (Applicative f, Eq k, Hashable k)
                => (k -> a -> f (Maybe b))
                -> Map k a
                -> f (Map k b)
kvTraverseMaybe f = fmap (Map.map fromJust . Map.filter isJust)
    . Map.traverseWithKey f

kvTraversal :: (Eq k', Hashable k')
           => Traversal (Map k v) (Map k' v') (k, v) (k', v')
kvTraversal f = fmap Map.fromList . traverse f . Map.toList
