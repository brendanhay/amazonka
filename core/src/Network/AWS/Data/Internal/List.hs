{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.Data.Internal.List
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.List where

import           Control.Applicative
import           Control.Lens                      hiding (coerce, element)
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Foldable                     (Foldable)
import           Data.List.NonEmpty                (NonEmpty(..))
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Semigroup                    (Semigroup)
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Traversable
import qualified Data.Vector                       as Vector
import           GHC.Exts
import           GHC.TypeLits
import           Network.AWS.Data.Internal.Flatten
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.XML
import           Text.XML

newtype List (k :: Symbol) a = List { list :: [a] }
    deriving (Eq, Ord, Show, Semigroup, Monoid)

newtype List1 (k :: Symbol) a = List1 { list1 :: NonEmpty a }
    deriving (Eq, Ord, Show, Semigroup)

deriving instance Functor     (List1 k)
deriving instance Foldable    (List1 k)
deriving instance Traversable (List1 k)

type role List  phantom representational
type role List1 phantom representational

_List :: (Coercible a b, Coercible b a) => Iso' (List k a) [b]
_List = iso (coerce . list) (List . coerce)

_List1 :: (Coercible a b, Coercible b a) => Iso' (List1 k a) (NonEmpty b)
_List1 = iso (coerce . list1) (List1 . coerce)

listItem :: forall k a. KnownSymbol k => List k a -> Text
listItem _ = Text.pack (symbolVal (Proxy :: Proxy k))

list1Item :: forall k a. KnownSymbol k => List1 k a -> Text
list1Item _ = Text.pack (symbolVal (Proxy :: Proxy k))

instance IsList (List k a) where
    type Item (List k a) = a

    fromList = List
    toList   = list

fromList1 :: List1 k a -> List k a
fromList1 = List . toList . list1

toList1 :: List k a -> Either String (List1 k a)
toList1 (List (x:xs))
          = Right $ List1 (x :| xs)
toList1 _ = Left  $ "Unexpected empty list, expected at least 1 element."

instance ToQuery a => ToQuery (List k a) where
    toQuery = toQuery . list

instance ToQuery a => ToQuery (List1 k a) where
    toQuery = toQuery . toList . list1

instance FromJSON a => FromJSON (List k a) where
    parseJSON = fmap List . parseJSON

instance FromJSON a => FromJSON (List1 k a) where
    parseJSON = withArray "List1" $ \case
        v | Vector.null v -> fail "Empty array, expected at least 1 element."
        v                 -> traverse parseJSON
            . List1 $ Vector.unsafeHead v :| Vector.toList (Vector.unsafeTail v)

instance ToJSON a => ToJSON (List k a) where
    toJSON = toJSON . list

instance ToJSON a => ToJSON (List1 k a) where
    toJSON = toJSON . toList . list1

instance (KnownSymbol k, FromXML a) => FromXML (List k a) where
    parseXML = fmap List . traverse parseXML . mapMaybe (childNodes n)
      where
        n = listItem (undefined :: List k a)

instance (KnownSymbol k, FromXML a) => FromXML (List1 k a) where
    parseXML = parseXML >=> toList1

instance (KnownSymbol k, ToXML a) => ToXML (List k a) where
    toXML = map (NodeElement . element n . toXML) . list
      where
        n = fromString . Text.unpack $ listItem (undefined :: List k a)

instance (KnownSymbol k, ToXML a) => ToXML (List1 k a) where
    toXML = toXML . fromList1
