-- |
-- Module      : Amazonka.Core.Lens.Internal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Re-export a number of lens types and combinators for use in service
-- bindings.
module Amazonka.Core.Lens.Internal
  ( concatOf,
    folding,
    to,
    module Export,
  )
where

import Lens.Micro as Export
  ( ASetter,
    ASetter',
    Getting,
    Lens,
    Lens',
    SimpleFold,
    SimpleGetter,
    filtered,
    has,
    lens,
    non,
    sets,
    toListOf,
    traversed,
    (%~),
    (.~),
    (<&>),
    (<>~),
    (?~),
    (^.),
    (^..),
    (^?),
    _1,
    _2,
    _Just,
    _last,
  )
import qualified Lens.Micro as Lens
import Lens.Micro.Contra as Export
  ( Fold,
    Getter,
    fromSimpleFold,
    fromSimpleGetter,
  )
import Lens.Micro.Extras as Export
  ( view,
  )
import Lens.Micro.Internal as Export
  ( foldMapOf,
    (#.),
  )
import Lens.Micro.Pro as Export
  ( AReview,
    Iso',
    LensLike',
    Prism',
    Traversal',
    coerced,
    iso,
    mapping,
    preview,
    prism,
    prism',
    review,
    (#),
  )
import Prelude (Foldable, ($))

-- | 'concatOf' is 'view', with a type signature to make it obvious
-- that it concatenates all focused lists.
concatOf :: Getting [r] s [r] -> s -> [r]
concatOf = view
{-# INLINE concatOf #-}

folding :: (Foldable f) => (s -> f a) -> Fold s a
folding f = fromSimpleFold $ Lens.folding f
{-# INLINE folding #-}

to :: (s -> a) -> Getter s a
to f = fromSimpleGetter $ Lens.to f
{-# INLINE to #-}
