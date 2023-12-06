-- |
-- Module      : Data.Functor.Barbie.Extended
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.Functor.Barbie.Extended (module Data.Functor.Barbie, bfor) where

import Data.Functor.Barbie

bfor ::
  (TraversableB b, Applicative e) =>
  b f ->
  (forall a. f a -> (e (g a))) ->
  e (b g)
bfor b f = btraverse f b
