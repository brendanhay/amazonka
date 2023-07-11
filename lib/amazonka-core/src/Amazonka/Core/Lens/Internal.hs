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
module Amazonka.Core.Lens.Internal (module Export) where

import Control.Exception.Lens as Export
  ( catching,
    catching_,
    exception,
    throwingM,
    trying,
    _IOException,
  )
import Control.Lens as Export
  ( AReview,
    Choice,
    Fold,
    Getter,
    Getting,
    IndexedTraversal',
    Iso',
    Lens,
    Lens',
    Optic',
    Prism',
    Setter',
    Traversal',
    allOf,
    anyOf,
    coerced,
    concatOf,
    filtered,
    folding,
    has,
    iso,
    lens,
    mapping,
    non,
    prism,
    sets,
    to,
    traversed,
    un,
    view,
    (#),
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
