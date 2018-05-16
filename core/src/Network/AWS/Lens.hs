-- |
-- Module      : Network.AWS.Lens
-- Copyright   : (c) 2013-2018 Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lens
    ( module Ex
    , module Ty
    , module Fn
    , module Op
    ) where

import           Control.Exception.Lens as Ex (catching, catching_, exception,
                                               throwingM, trying, _IOException)
import           Control.Lens           as Ty (AReview, Choice, Fold, Getter,
                                               Getting, IndexedTraversal', Iso',
                                               Lens', Optic', Prism', Setter',
                                               Traversal')
import           Control.Lens           as Fn (allOf, anyOf, concatOf, filtered,
                                               folding, has, iso, lens, mapping,
                                               prism, sets, to, traversed, un,
                                               view, _1, _2, _Just, _last)
import           Control.Lens           as Op (( # ), (%~), (&), (.~), (<&>),
                                               (<>~), (?~), (^.), (^?))
