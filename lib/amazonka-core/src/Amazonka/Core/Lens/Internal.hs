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
    folding,
    has,
    lens,
    non,
    sets,
    to,
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
import Lens.Micro.Contra as Export
  ( Fold,
    Getter,
    fromSimpleFold,
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

-- | 'concatOf' is 'view', with a type signature to make it obvious
-- that it concatenates all focused lists.
concatOf :: Getting [r] s [r] -> s -> [r]
concatOf = view
{-# INLINE concatOf #-}
