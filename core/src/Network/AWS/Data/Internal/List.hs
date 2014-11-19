{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RoleAnnotations            #-}

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
import           Control.Lens                      hiding (coerce)
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
import           Data.Foldable                     (Foldable)
import           Data.List.NonEmpty                (NonEmpty(..))
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Monoid
import           Data.Semigroup                    (Semigroup)
import           Data.Traversable
import qualified Data.Vector                       as Vector
import           GHC.Exts
import           GHC.TypeLits
import           Network.AWS.Data.Internal.Flatten
import           Network.AWS.Data.Internal.Query
import           Network.AWS.Data.Internal.XML

newtype List (e :: Symbol) a = List { list :: [a] }
    deriving (Eq, Ord, Show, Semigroup, Monoid)

newtype List1 (e :: Symbol) a = List1 { list1 :: NonEmpty a }
    deriving (Eq, Ord, Show, Semigroup)

type role List  phantom representational
type role List1 phantom representational

_List :: (Coercible a b, Coercible b a) => Iso' (List e a) [b]
_List = iso (coerce . list) (List . coerce)

_List1 :: (Coercible a b, Coercible b a) => Iso' (List1 e a) (NonEmpty b)
_List1 = iso (coerce . list1) (List1 . coerce)

-- fromList :: a -> [a] -> List1 a
-- fromList a = List1 . (:|) a

-- toList :: List1 a -> [a]
-- toList = NonEmpty.toList . toNonEmpty

-- instance ToQuery a => ToQuery (List1 a) where
--     toQuery = toQuery . toList

-- instance ToQuery a => ToQuery (List1 a) where
--     toQuery = toQuery . toList

-- instance ToQuery a => ToQuery (List1 a) where
--     toQuery = toQuery . toList

-- instance FromJSON a => FromJSON (List1 a) where
--     parseJSON = withArray "List1" $ \case
--         v | Vector.null v ->
--                 fail "Empty array, expected at least 1 element."
--         v -> traverse parseJSON $
--                  Vector.unsafeHead v
--                      `fromList` Vector.toList (Vector.unsafeTail v)

-- instance ToJSON a => ToJSON (List1 a) where
--     toJSON = toJSON . toList


-- instance FromXML a => FromXML (List1 a) where
--     parseXML = fmap List1 . parseXML

-- instance ToXML a => ToXML (List1 a) where
--     toXML = toXML . toList
