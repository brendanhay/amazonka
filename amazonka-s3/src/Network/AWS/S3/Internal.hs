{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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
    ( module Network.AWS.S3.Internal
    , Region
    ) where

import           Control.Lens
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Foldable         as Fold
import           Data.Sequence         as Seq
import           Data.Sequence.Lens
import           Data.String
import           Network.AWS.Data.XML
import           Network.AWS.Prelude

default (ByteString)

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
        , FromXML
        , ToXML
        , ToQuery
        )

instance ToByteString BucketName
instance ToPath       BucketName

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
        , FromXML
        , ToXML
        , ToQuery
        )

instance ToByteString ObjectVersionId
instance ToPath       ObjectVersionId

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

type Delimiter = Char

data ObjectKey
    = DecodedKey !Delimiter (Seq ByteString)
    | RawKey     ByteString
      deriving (Ord, Read, Show, Data, Typeable, Generic)

-- FIXME: suboptimal eq complexity due to comparison requiring encoded keys.
instance Eq ObjectKey where
    a == b =
        case (a, b) of
            (DecodedKey x xs, DecodedKey y ys) -> x == y && xs == ys
            (RawKey       x,  RawKey       y)  -> x == y
            _                                  -> toBS a == toBS b

-- FIXME: cause more problems than it's worth?
instance IsString ObjectKey where
    fromString = objectKey '/' . fromString

instance FromText ObjectKey where
    parser = RawKey . toBS <$> takeText

instance FromXML ObjectKey where
    parseXML = parseXMLText "ObjectKey"

instance ToByteString ObjectKey where
    toBS = \case
        RawKey       bs -> bs
        DecodedKey c xs -> BS8.intercalate (BS8.singleton c) (Fold.toList xs)

instance ToPath ObjectKey where
    toPath = toPath . \case
        RawKey       bs -> bs
        DecodedKey c xs ->
            BS8.intercalate (BS8.singleton c) $
                map (urlEncode False) $
                    Fold.toList xs

instance ToText    ObjectKey where toText  = toText  . toBS
instance ToQuery   ObjectKey where toQuery = toQuery . toBS
instance ToBuilder ObjectKey where build   = build   . toBS
instance ToXML     ObjectKey where toXML   = toXMLText

objectKey :: Delimiter -> ByteString -> ObjectKey
objectKey c = DecodedKey c . splitKey c

splitKey :: Delimiter -> ByteString -> Seq ByteString
splitKey c = Seq.fromList . BS8.split c

components :: Delimiter -> IndexedTraversal' Int ObjectKey ByteString
components c f = \case
    DecodedKey _ xs -> DecodedKey c <$> traversed f xs
    RawKey       bs -> DecodedKey c <$> traversed f (splitKey c bs)

_Last :: Delimiter -> Traversal' ObjectKey ByteString
_Last c f = \case
    DecodedKey _ xs -> go xs
    RawKey       bs -> go (splitKey c bs)
  where
    go = fmap (DecodedKey c) . viewR right

    right (xs :> x) = (xs :>) <$> f x
    right EmptyR    = pure EmptyR
