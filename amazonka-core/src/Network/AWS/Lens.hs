-- |
-- Module      : Network.AWS.Lens
-- Copyright   : (c) 2013-2020 Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lens
  ( module Export,
  )
where

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
    Lens',
    Optic',
    Prism',
    Setter',
    Traversal',
    allOf,
    anyOf,
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
    (%~),
    (&),
    (.~),
    (<&>),
    (<>~),
    (?~),
    (^.),
    (^?),
    _1,
    _2,
    _Just,
    _last,
  )
import Data.Generics.Product.Fields as Export
  ( HasField (field),
  )
