{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import           Data.Data            (Data, Typeable)
import           Data.String
import           GHC.Generics         (Generic)
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

-- objectKey :: Delimiter -> ByteString -> ObjectKey
-- objectKey c = DecodedKey c . splitKey c

-- splitKey :: Delimiter -> ByteString -> Seq ByteString
-- splitKey c = Seq.fromList . BS8.split c

-- components :: Delimiter -> IndexedTraversal' Int ObjectKey ByteString
-- components c f = \case
--     DecodedKey _ xs -> DecodedKey c <$> traversed f xs
--     RawKey       bs -> DecodedKey c <$> traversed f (splitKey c bs)

-- _Last :: Delimiter -> Traversal' ObjectKey ByteString
-- _Last c f = \case
--     DecodedKey _ xs -> go xs
--     RawKey       bs -> go (splitKey c bs)
--   where
--     go = fmap (DecodedKey c) . viewR right

--     right (xs :> x) = (xs :>) <$> f x
--     right EmptyR    = pure EmptyR


-- Provide a traversal for components, last etc.

-- Reintroduce some of the changes
