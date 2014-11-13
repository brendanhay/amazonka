{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Module      : Network.AWS.Data.Internal.List1
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.List1
    ( List1
    , _List1
    , list1
    , head
    , length
    , (<|)
    , map
    , toList
    , fromList
    , toNonEmpty
    ) where

import           Control.Applicative
import           Control.Lens                    (Iso', iso)
import           Data.Aeson
import           Data.Foldable                   (Foldable)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Semigroup                  (Semigroup)
import           Data.Tagged
import           Data.Traversable
import qualified Data.Vector                     as Vector
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.XML
import           Prelude                         hiding (map, head, length)

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

_List1 :: Iso' (List1 a) (NonEmpty a)
_List1 = iso toNonEmpty List1

list1 :: a -> [a] -> List1 a
list1 a = List1 . (:|) a

infixr 5 <|

head :: List1 a -> a
head = NonEmpty.head . toNonEmpty

length :: List1 a -> Int
length = NonEmpty.length . toNonEmpty

(<|) :: a -> List1 a -> List1 a
(<|) x = List1 . (NonEmpty.<|) x . toNonEmpty

map :: (a -> b) -> List1 a -> List1 b
map f = List1 . NonEmpty.map f . toNonEmpty

toList :: List1 a -> [a]
toList = NonEmpty.toList . toNonEmpty

fromList :: [a] -> Maybe (List1 a)
fromList []     = Nothing
fromList (x:xs) = Just (list1 x xs)

instance ToQuery a => ToQuery (List1 a) where
    toQuery = toQuery . toList

instance FromJSON a => FromJSON (List1 a) where
    parseJSON = withArray "List1" $ \case
        v | Vector.null v ->
                fail "Empty array, expected at least 1 element."
        v -> traverse parseJSON $
                 Vector.unsafeHead v
                     `list1` Vector.toList (Vector.unsafeTail v)

instance ToJSON a => ToJSON (List1 a) where
    toJSON = toJSON . toList

instance FromXML a => FromXML (List1 a) where
    fromXMLRoot = fromRoot "List1"
    fromXML o   = either Left f . fromXML (retag o)
      where
        f []     = Left  "Empty list, expected at least 1 element."
        f (x:xs) = Right (list1 x xs)

instance ToXML a => ToXML (List1 a) where
    toXMLRoot = toRoot "List1"
    toXML o   = toXML (retag o) . toList
