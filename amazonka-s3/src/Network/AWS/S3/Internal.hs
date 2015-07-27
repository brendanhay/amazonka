{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

-- Module      : Network.AWS.S3.Internal
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.S3.Internal
    ( Region
    , BucketName      (..)
    , ETag            (..)
    , ObjectVersionId (..)
    , ObjectKey       (..)
    , Delimiter
    , _Key
    , segments
    , _KeyHead
    , _KeyLast
    ) where

import           Control.Lens
import           Data.String
import qualified Data.Text            as Text
import           Network.AWS.Data.XML
import           Network.AWS.Prelude

newtype BucketName = BucketName Text
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

instance ToPath BucketName

-- FIXME: Add the difference between weak + strong ETags and their respective
-- equalities if necessary, see: https://github.com/brendanhay/amazonka/issues/76
newtype ETag = ETag ByteString
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

newtype ObjectVersionId = ObjectVersionId Text
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

instance ToPath ObjectVersionId

newtype ObjectKey = ObjectKey Text
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

instance ToPath ObjectKey

type Delimiter = Char

_Key :: Iso' ObjectKey Text
_Key = iso (\(ObjectKey k) -> k) ObjectKey
{-# INLINE _Key #-}

segments :: Delimiter -> IndexedTraversal' Int ObjectKey Text
segments c f k = joinKey c <$> traversed f (splitKey c k)
{-# INLINE segments #-}

_KeyHead :: Delimiter -> Traversal' ObjectKey Text
_KeyHead c = _KeyCons c . _1
{-# INLINE _KeyHead #-}

_KeyLast :: Delimiter -> Traversal' ObjectKey Text
_KeyLast c = _KeySnoc c . _2
{-# INLINE _KeyLast #-}

-- The following are modelled on the respective _Cons and _Snoc type classes:

_KeyCons :: Delimiter -> Prism' ObjectKey (Text, [Text])
_KeyCons c = prism (joinKey c . uncurry (:)) $ \k ->
    case splitKey c k of
        []     -> Left  k
        (x:xs) -> Right (x, xs)

_KeySnoc :: Delimiter -> Prism' ObjectKey ([Text], Text)
_KeySnoc c = prism (\(xs, x) -> joinKey c (xs ++ [x])) $ \k ->
    case splitKey c k of
        [] -> Left  k
        xs -> Right (init xs, last xs)

joinKey :: Char -> [Text] -> ObjectKey
joinKey !c = ObjectKey . Text.intercalate (Text.singleton c)

splitKey :: Char -> ObjectKey -> [Text]
splitKey !c (ObjectKey k) = Text.split (== c) k
