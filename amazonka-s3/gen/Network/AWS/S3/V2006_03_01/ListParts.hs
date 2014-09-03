{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.ListParts
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the parts that have been uploaded for a specific multipart upload.
module Network.AWS.S3.V2006_03_01.ListParts
    (
    -- * Request
      ListParts
    -- ** Request constructor
    , listParts
    -- ** Request lenses
    , lprBucket
    , lprUploadId
    , lprKey
    , lprMaxParts
    , lprPartNumberMarker

    -- * Response
    , ListPartsResponse
    -- ** Response lenses
    , lpoIsTruncated
    , lpoBucket
    , lpoInitiator
    , lpoMaxParts
    , lpoUploadId
    , lpoNextPartNumberMarker
    , lpoKey
    , lpoOwner
    , lpoPartNumberMarker
    , lpoParts
    , lpoStorageClass
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListParts' request.
listParts :: BucketName -- ^ 'lprBucket'
          -> Text -- ^ 'lprUploadId'
          -> ObjectKey -- ^ 'lprKey'
          -> ListParts
listParts p1 p2 p3 = ListParts
    { _lprBucket = p1
    , _lprUploadId = p2
    , _lprKey = p3
    , _lprMaxParts = Nothing
    , _lprPartNumberMarker = Nothing
    }

data ListParts = ListParts
    { _lprBucket :: BucketName
    , _lprUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose parts are being
      -- listed.
    , _lprKey :: ObjectKey
    , _lprMaxParts :: Maybe Integer
      -- ^ Sets the maximum number of parts to return.
    , _lprPartNumberMarker :: Maybe Integer
      -- ^ Specifies the part after which listing should begin. Only parts
      -- with higher part numbers will be listed.
    } deriving (Show, Generic)

lprBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> ListParts
    -> f ListParts
lprBucket f x =
    (\y -> x { _lprBucket = y })
       <$> f (_lprBucket x)
{-# INLINE lprBucket #-}

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprUploadId
    :: Functor f
    => (Text
    -> f (Text))
    -> ListParts
    -> f ListParts
lprUploadId f x =
    (\y -> x { _lprUploadId = y })
       <$> f (_lprUploadId x)
{-# INLINE lprUploadId #-}

lprKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> ListParts
    -> f ListParts
lprKey f x =
    (\y -> x { _lprKey = y })
       <$> f (_lprKey x)
{-# INLINE lprKey #-}

-- | Sets the maximum number of parts to return.
lprMaxParts
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListParts
    -> f ListParts
lprMaxParts f x =
    (\y -> x { _lprMaxParts = y })
       <$> f (_lprMaxParts x)
{-# INLINE lprMaxParts #-}

-- | Specifies the part after which listing should begin. Only parts with higher
-- part numbers will be listed.
lprPartNumberMarker
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListParts
    -> f ListParts
lprPartNumberMarker f x =
    (\y -> x { _lprPartNumberMarker = y })
       <$> f (_lprPartNumberMarker x)
{-# INLINE lprPartNumberMarker #-}

instance ToPath ListParts where
    toPath ListParts{..} = mconcat
        [ "/"
        , toBS _lprBucket
        , "/"
        , toBS _lprKey
        ]

instance ToQuery ListParts where
    toQuery ListParts{..} = mconcat
        [ "max-parts" =? _lprMaxParts
        , "part-number-marker" =? _lprPartNumberMarker
        , "uploadId" =? _lprUploadId
        ]

instance ToHeaders ListParts

instance ToBody ListParts

data ListPartsResponse = ListPartsResponse
    { _lpoIsTruncated :: Bool
      -- ^ Indicates whether the returned list of parts is truncated.
    , _lpoBucket :: Maybe BucketName
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , _lpoInitiator :: Maybe Initiator
      -- ^ Identifies who initiated the multipart upload.
    , _lpoMaxParts :: Maybe Integer
      -- ^ Maximum number of parts that were allowed in the response.
    , _lpoUploadId :: Maybe Text
      -- ^ Upload ID identifying the multipart upload whose parts are being
      -- listed.
    , _lpoNextPartNumberMarker :: Maybe Integer
      -- ^ When a list is truncated, this element specifies the last part in
      -- the list, as well as the value to use for the part-number-marker
      -- request parameter in a subsequent request.
    , _lpoKey :: Maybe ObjectKey
      -- ^ Object key for which the multipart upload was initiated.
    , _lpoOwner :: Maybe Owner
    , _lpoPartNumberMarker :: Maybe Integer
      -- ^ Part number after which listing begins.
    , _lpoParts :: [Part]
    , _lpoStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Show, Generic)

-- | Indicates whether the returned list of parts is truncated.
lpoIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoIsTruncated f x =
    (\y -> x { _lpoIsTruncated = y })
       <$> f (_lpoIsTruncated x)
{-# INLINE lpoIsTruncated #-}

-- | Name of the bucket to which the multipart upload was initiated.
lpoBucket
    :: Functor f
    => (Maybe BucketName
    -> f (Maybe BucketName))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoBucket f x =
    (\y -> x { _lpoBucket = y })
       <$> f (_lpoBucket x)
{-# INLINE lpoBucket #-}

-- | Identifies who initiated the multipart upload.
lpoInitiator
    :: Functor f
    => (Maybe Initiator
    -> f (Maybe Initiator))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoInitiator f x =
    (\y -> x { _lpoInitiator = y })
       <$> f (_lpoInitiator x)
{-# INLINE lpoInitiator #-}

-- | Maximum number of parts that were allowed in the response.
lpoMaxParts
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoMaxParts f x =
    (\y -> x { _lpoMaxParts = y })
       <$> f (_lpoMaxParts x)
{-# INLINE lpoMaxParts #-}

-- | Upload ID identifying the multipart upload whose parts are being listed.
lpoUploadId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoUploadId f x =
    (\y -> x { _lpoUploadId = y })
       <$> f (_lpoUploadId x)
{-# INLINE lpoUploadId #-}

-- | When a list is truncated, this element specifies the last part in the list,
-- as well as the value to use for the part-number-marker request parameter in
-- a subsequent request.
lpoNextPartNumberMarker
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoNextPartNumberMarker f x =
    (\y -> x { _lpoNextPartNumberMarker = y })
       <$> f (_lpoNextPartNumberMarker x)
{-# INLINE lpoNextPartNumberMarker #-}

-- | Object key for which the multipart upload was initiated.
lpoKey
    :: Functor f
    => (Maybe ObjectKey
    -> f (Maybe ObjectKey))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoKey f x =
    (\y -> x { _lpoKey = y })
       <$> f (_lpoKey x)
{-# INLINE lpoKey #-}

lpoOwner
    :: Functor f
    => (Maybe Owner
    -> f (Maybe Owner))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoOwner f x =
    (\y -> x { _lpoOwner = y })
       <$> f (_lpoOwner x)
{-# INLINE lpoOwner #-}

-- | Part number after which listing begins.
lpoPartNumberMarker
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoPartNumberMarker f x =
    (\y -> x { _lpoPartNumberMarker = y })
       <$> f (_lpoPartNumberMarker x)
{-# INLINE lpoPartNumberMarker #-}

lpoParts
    :: Functor f
    => ([Part]
    -> f ([Part]))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoParts f x =
    (\y -> x { _lpoParts = y })
       <$> f (_lpoParts x)
{-# INLINE lpoParts #-}

-- | The class of storage used to store the object.
lpoStorageClass
    :: Functor f
    => (Maybe StorageClass
    -> f (Maybe StorageClass))
    -> ListPartsResponse
    -> f ListPartsResponse
lpoStorageClass f x =
    (\y -> x { _lpoStorageClass = y })
       <$> f (_lpoStorageClass x)
{-# INLINE lpoStorageClass #-}

instance FromXML ListPartsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListParts where
    type Sv ListParts = S3
    type Rs ListParts = ListPartsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListParts where
    next rq rs
        | not (_lpoIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lprPartNumberMarker = _lpoNextPartNumberMarker rs
            }
