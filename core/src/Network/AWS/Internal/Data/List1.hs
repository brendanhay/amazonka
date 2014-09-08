{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- Module      : Network.AWS.Internal.Data.List1
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Data.List1
    ( List1
    , list1
    , (<|)
    , toList
    , toNonEmpty
    ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Foldable       (Foldable)
import           Data.List.NonEmpty  (NonEmpty(..))
import qualified Data.List.NonEmpty  as NonEmpty
import           Data.Semigroup      (Semigroup)
import           Data.Traversable
import qualified Data.Vector         as Vector

newtype List1 a = List1 { toNonEmpty :: NonEmpty a }
    deriving
        ( Eq
        , Ord
        , Show
        , Read
        , Functor
        , Applicative
        , Monad
        , Foldable
        , Traversable
        , Semigroup
        )

list1 :: a -> [a] -> List1 a
list1 a = List1 . (:|) a

infixr 5 <|

(<|) :: a -> List1 a -> List1 a
(<|) x = List1 . (NonEmpty.<|) x . toNonEmpty

toList :: List1 a -> [a]
toList = NonEmpty.toList . toNonEmpty

instance ToJSON a => ToJSON (List1 a) where
    toJSON = toJSON . toList

instance FromJSON a => FromJSON (List1 a) where
    parseJSON = withArray "List1" $ \case
        v | Vector.null v ->
                fail "Empty array, expected at least 1 element."
        v -> traverse parseJSON $
                 Vector.unsafeHead v
                     `list1` Vector.toList (Vector.unsafeTail v)
