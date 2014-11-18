{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RoleAnnotations            #-}

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
    ( List1 (..)
    , _List1
    ) where

import           Control.Applicative
import           Control.Lens                    (Iso', iso)
import           Control.Monad
import           Data.Aeson
import           Data.Coerce
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

type role List1 representational

_List1 :: (Coercible a b, Coercible b a) => Iso' (List1 a) (NonEmpty b)
_List1 = iso (coerce . toNonEmpty) (List1 . coerce)

fromList :: a -> [a] -> List1 a
fromList a = List1 . (:|) a

toList :: List1 a -> [a]
toList = NonEmpty.toList . toNonEmpty

instance ToQuery a => ToQuery (List1 a) where
    toQuery = toQuery . toList

instance FromJSON a => FromJSON (List1 a) where
    parseJSON = withArray "List1" $ \case
        v | Vector.null v ->
                fail "Empty array, expected at least 1 element."
        v -> traverse parseJSON $
                 Vector.unsafeHead v
                     `fromList` Vector.toList (Vector.unsafeTail v)

instance ToJSON a => ToJSON (List1 a) where
    toJSON = toJSON . toList

instance FromXML a => FromXML (List1 a) where
    parseXML = fmap List1 . parseXML

instance ToXML a => ToXML (List1 a) where
    toXML = toXML . toList
