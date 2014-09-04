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
    , mkListPartsRequest
    -- ** Request lenses
    , lprBucket
    , lprKey
    , lprMaxParts
    , lprPartNumberMarker
    , lprUploadId

    -- * Response
    , ListPartsResponse
    -- ** Response lenses
    , lpoBucket
    , lpoKey
    , lpoUploadId
    , lpoPartNumberMarker
    , lpoNextPartNumberMarker
    , lpoMaxParts
    , lpoIsTruncated
    , lpoParts
    , lpoInitiator
    , lpoOwner
    , lpoStorageClass
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListParts' request.
mkListPartsRequest :: BucketName -- ^ 'lprBucket'
                   -> ObjectKey -- ^ 'lprKey'
                   -> Text -- ^ 'lprUploadId'
                   -> ListParts
mkListPartsRequest p1 p2 p3 = ListParts
    { _lprBucket = p1
    , _lprKey = p2
    , _lprMaxParts = Nothing
    , _lprPartNumberMarker = Nothing
    , _lprUploadId = p5
    }
{-# INLINE mkListPartsRequest #-}

data ListParts = ListParts
    { _lprBucket :: BucketName
    , _lprKey :: ObjectKey
    , _lprMaxParts :: Maybe Integer
      -- ^ Sets the maximum number of parts to return.
    , _lprPartNumberMarker :: Maybe Integer
      -- ^ Specifies the part after which listing should begin. Only parts
      -- with higher part numbers will be listed.
    , _lprUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose parts are being
      -- listed.
    } deriving (Show, Generic)

lprBucket :: Lens' ListParts (BucketName)
lprBucket = lens _lprBucket (\s a -> s { _lprBucket = a })
{-# INLINE lprBucket #-}

lprKey :: Lens' ListParts (ObjectKey)
lprKey = lens _lprKey (\s a -> s { _lprKey = a })
{-# INLINE lprKey #-}

-- | Sets the maximum number of parts to return.
lprMaxParts :: Lens' ListParts (Maybe Integer)
lprMaxParts = lens _lprMaxParts (\s a -> s { _lprMaxParts = a })
{-# INLINE lprMaxParts #-}

-- | Specifies the part after which listing should begin. Only parts with higher
-- part numbers will be listed.
lprPartNumberMarker :: Lens' ListParts (Maybe Integer)
lprPartNumberMarker = lens _lprPartNumberMarker (\s a -> s { _lprPartNumberMarker = a })
{-# INLINE lprPartNumberMarker #-}

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprUploadId :: Lens' ListParts (Text)
lprUploadId = lens _lprUploadId (\s a -> s { _lprUploadId = a })
{-# INLINE lprUploadId #-}

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
    { _lpoBucket :: Maybe BucketName
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , _lpoKey :: Maybe ObjectKey
      -- ^ Object key for which the multipart upload was initiated.
    , _lpoUploadId :: Maybe Text
      -- ^ Upload ID identifying the multipart upload whose parts are being
      -- listed.
    , _lpoPartNumberMarker :: Maybe Integer
      -- ^ Part number after which listing begins.
    , _lpoNextPartNumberMarker :: Maybe Integer
      -- ^ When a list is truncated, this element specifies the last part in
      -- the list, as well as the value to use for the part-number-marker
      -- request parameter in a subsequent request.
    , _lpoMaxParts :: Maybe Integer
      -- ^ Maximum number of parts that were allowed in the response.
    , _lpoIsTruncated :: Bool
      -- ^ Indicates whether the returned list of parts is truncated.
    , _lpoParts :: [Part]
    , _lpoInitiator :: Maybe Initiator
      -- ^ Identifies who initiated the multipart upload.
    , _lpoOwner :: Maybe Owner
    , _lpoStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Show, Generic)

-- | Name of the bucket to which the multipart upload was initiated.
lpoBucket :: Lens' ListPartsResponse (Maybe BucketName)
lpoBucket = lens _lpoBucket (\s a -> s { _lpoBucket = a })
{-# INLINE lpoBucket #-}

-- | Object key for which the multipart upload was initiated.
lpoKey :: Lens' ListPartsResponse (Maybe ObjectKey)
lpoKey = lens _lpoKey (\s a -> s { _lpoKey = a })
{-# INLINE lpoKey #-}

-- | Upload ID identifying the multipart upload whose parts are being listed.
lpoUploadId :: Lens' ListPartsResponse (Maybe Text)
lpoUploadId = lens _lpoUploadId (\s a -> s { _lpoUploadId = a })
{-# INLINE lpoUploadId #-}

-- | Part number after which listing begins.
lpoPartNumberMarker :: Lens' ListPartsResponse (Maybe Integer)
lpoPartNumberMarker = lens _lpoPartNumberMarker (\s a -> s { _lpoPartNumberMarker = a })
{-# INLINE lpoPartNumberMarker #-}

-- | When a list is truncated, this element specifies the last part in the list,
-- as well as the value to use for the part-number-marker request parameter in
-- a subsequent request.
lpoNextPartNumberMarker :: Lens' ListPartsResponse (Maybe Integer)
lpoNextPartNumberMarker = lens _lpoNextPartNumberMarker (\s a -> s { _lpoNextPartNumberMarker = a })
{-# INLINE lpoNextPartNumberMarker #-}

-- | Maximum number of parts that were allowed in the response.
lpoMaxParts :: Lens' ListPartsResponse (Maybe Integer)
lpoMaxParts = lens _lpoMaxParts (\s a -> s { _lpoMaxParts = a })
{-# INLINE lpoMaxParts #-}

-- | Indicates whether the returned list of parts is truncated.
lpoIsTruncated :: Lens' ListPartsResponse (Bool)
lpoIsTruncated = lens _lpoIsTruncated (\s a -> s { _lpoIsTruncated = a })
{-# INLINE lpoIsTruncated #-}

lpoParts :: Lens' ListPartsResponse ([Part])
lpoParts = lens _lpoParts (\s a -> s { _lpoParts = a })
{-# INLINE lpoParts #-}

-- | Identifies who initiated the multipart upload.
lpoInitiator :: Lens' ListPartsResponse (Maybe Initiator)
lpoInitiator = lens _lpoInitiator (\s a -> s { _lpoInitiator = a })
{-# INLINE lpoInitiator #-}

lpoOwner :: Lens' ListPartsResponse (Maybe Owner)
lpoOwner = lens _lpoOwner (\s a -> s { _lpoOwner = a })
{-# INLINE lpoOwner #-}

-- | The class of storage used to store the object.
lpoStorageClass :: Lens' ListPartsResponse (Maybe StorageClass)
lpoStorageClass = lens _lpoStorageClass (\s a -> s { _lpoStorageClass = a })
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
