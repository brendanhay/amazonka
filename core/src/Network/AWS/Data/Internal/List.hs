{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- Module      : Network.AWS.Data.Internal.List
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.List where

import           Control.Lens                    hiding (coerce, element)
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Foldable                   (Foldable)
import           Data.List.NonEmpty              (NonEmpty(..))
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Semigroup                  (Semigroup)
import           Data.String
import qualified Data.Text                       as Text
import qualified Data.Vector                     as Vector
import           GHC.Exts
import           GHC.TypeLits
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.XML
import           Text.XML

newtype List (e :: Symbol) a = List { list :: [a] }
    deriving (Eq, Ord, Read, Show, Semigroup, Monoid)

newtype List1 (e :: Symbol) a = List1 { list1 :: NonEmpty a }
    deriving (Eq, Ord, Read, Show, Semigroup)

deriving instance Functor     (List1 e)
deriving instance Foldable    (List1 e)
deriving instance Traversable (List1 e)

type role List  phantom representational
type role List1 phantom representational

_List :: (Coercible a b, Coercible b a) => Iso' (List e a) [b]
_List = iso (coerce . list) (List . coerce)

_List1 :: (Coercible a b, Coercible b a) => Iso' (List1 e a) (NonEmpty b)
_List1 = iso (coerce . list1) (List1 . coerce)

instance IsList (List e a) where
    type Item (List e a) = a

    fromList = List
    toList   = list

fromList1 :: List1 e a -> List e a
fromList1 = List . toList . list1

toList1 :: List e a -> Either String (List1 e a)
toList1 (List (x:xs))
          = Right $ List1 (x :| xs)
toList1 _ = Left  $ "Unexpected empty list, expected at least 1 element."

instance (KnownSymbol e, ToQuery a) => ToQuery (List e a) where
    toQuery = toQueryList n . toList . list
      where
        n = fromString $ symbolVal (Proxy :: Proxy e)

instance (KnownSymbol e, ToQuery a) => ToQuery (List1 e a) where
    toQuery = toQueryList n . toList . list1
      where
        n = fromString $ symbolVal (Proxy :: Proxy e)

instance FromJSON a => FromJSON (List e a) where
    parseJSON = fmap List . parseJSON

instance FromJSON a => FromJSON (List1 e a) where
    parseJSON = withArray "List1" $ \case
        v | Vector.null v -> fail "Empty array, expected at least 1 element."
        v                 -> traverse parseJSON
            . List1 $ Vector.unsafeHead v :| Vector.toList (Vector.unsafeTail v)

instance ToJSON a => ToJSON (List e a) where
    toJSON = toJSON . list

instance ToJSON a => ToJSON (List1 e a) where
    toJSON = toJSON . toList . list1

instance (KnownSymbol e, FromXML a) => FromXML (List e a) where
    parseXML = fmap List . traverse parseXML . mapMaybe (childNodes n)
      where
        n = Text.pack $ symbolVal (Proxy :: Proxy e)

instance (KnownSymbol e, FromXML a) => FromXML (List1 e a) where
    parseXML = parseXML >=> toList1

instance (KnownSymbol e, ToXML a) => ToXML (List e a) where
    toXML = map (NodeElement . element n . toXML) . list
      where
        n = fromString $ symbolVal (Proxy :: Proxy e)

instance (KnownSymbol e, ToXML a) => ToXML (List1 e a) where
    toXML = toXML . fromList1
