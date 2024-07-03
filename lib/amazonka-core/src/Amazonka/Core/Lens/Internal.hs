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
  ( exception,
    anyOf,
    allOf,
    concatOf,
    module Export,
  )
where

import Control.Exception (Exception, SomeException, fromException, toException)
import Data.Functor.Const (Const (..))
import Data.Monoid (All (..), Any (..))
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
import Prelude (Bool)

-- TODO: Remaining items to find and fix based on export list
-- import Control.Lens as Export
--   ( concatOf,
--     folding,
--     mapping,
--     un,
--   )

-- | The following are all lifted from Control.Lens

-- | Traverse the strongly typed 'Exception' contained in 'SomeException' where the type of your function matches
-- the desired 'Exception'.
--
-- @
-- 'exception' :: ('Applicative' f, 'Exception' a)
--           => (a -> f a) -> 'SomeException' -> f 'SomeException'
-- @
exception :: (Exception a) => Prism' SomeException a
exception = prism' toException fromException
{-# INLINE exception #-}

-- | Returns 'True' if any target of a 'Fold' satisfies a predicate.
--
-- >>> anyOf both (=='x') ('x','y')
-- True
-- >>> import Data.Data.Lens
-- >>> anyOf biplate (== "world") (((),2::Int),"hello",("world",11::Int))
-- True
--
-- @
-- 'Data.Foldable.any' ≡ 'anyOf' 'folded'
-- @
--
-- @
-- 'ianyOf' l ≡ 'anyOf' l '.' 'Indexed'
-- @
--
-- @
-- 'anyOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny #. foldMapOf l (Any #. f)
{-# INLINE anyOf #-}

-- | Returns 'True' if every target of a 'Fold' satisfies a predicate.
--
-- >>> allOf both (>=3) (4,5)
-- True
-- >>> allOf folded (>=2) [1..10]
-- False
--
-- @
-- 'Data.Foldable.all' ≡ 'allOf' 'folded'
-- @
--
-- @
-- 'iallOf' l = 'allOf' l '.' 'Indexed'
-- @
--
-- @
-- 'allOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll #. foldMapOf l (All #. f)
{-# INLINE allOf #-}

-- | Concatenate all of the lists targeted by a 'Fold' into a longer list.
--
-- >>> concatOf both ("pan","ama")
-- "panama"
--
-- @
-- 'concat' ≡ 'concatOf' 'folded'
-- 'concatOf' ≡ 'view'
-- @
--
-- @
-- 'concatOf' :: 'Getter' s [r]     -> s -> [r]
-- 'concatOf' :: 'Fold' s [r]       -> s -> [r]
-- 'concatOf' :: 'Iso'' s [r]       -> s -> [r]
-- 'concatOf' :: 'Lens'' s [r]      -> s -> [r]
-- 'concatOf' :: 'Traversal'' s [r] -> s -> [r]
-- @
concatOf :: Getting [r] s [r] -> s -> [r]
concatOf l = getConst #. l Const
{-# INLINE concatOf #-}
