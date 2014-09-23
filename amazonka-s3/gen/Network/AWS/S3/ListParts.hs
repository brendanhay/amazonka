{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the parts that have been uploaded for a specific multipart upload.
module Network.AWS.S3.ListParts
    (
    -- * Request
      ListParts
    -- ** Request constructor
    , listParts
    -- ** Request lenses
    , lpBucket
    , lpKey
    , lpMaxParts
    , lpPartNumberMarker
    , lpUploadId

    -- * Response
    , ListPartsResponse
    -- ** Response constructor
    , listPartsResponse
    -- ** Response lenses
    , lprBucket
    , lprKey
    , lprUploadId
    , lprPartNumberMarker
    , lprNextPartNumberMarker
    , lprMaxParts
    , lprIsTruncated
    , lprParts
    , lprInitiator
    , lprOwner
    , lprStorageClass
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data ListParts = ListParts
    { _lpBucket :: BucketName
    , _lpKey :: ObjectKey
    , _lpMaxParts :: Maybe Integer
    , _lpPartNumberMarker :: Maybe Integer
    , _lpUploadId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListParts' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Key ::@ @ObjectKey@
--
-- * @MaxParts ::@ @Maybe Integer@
--
-- * @PartNumberMarker ::@ @Maybe Integer@
--
-- * @UploadId ::@ @Text@
--
listParts :: BucketName -- ^ 'lpBucket'
          -> ObjectKey -- ^ 'lpKey'
          -> Text -- ^ 'lpUploadId'
          -> ListParts
listParts p1 p2 p5 = ListParts
    { _lpBucket = p1
    , _lpKey = p2
    , _lpMaxParts = Nothing
    , _lpPartNumberMarker = Nothing
    , _lpUploadId = p5
    }

lpBucket :: Lens' ListParts BucketName
lpBucket = lens _lpBucket (\s a -> s { _lpBucket = a })

lpKey :: Lens' ListParts ObjectKey
lpKey = lens _lpKey (\s a -> s { _lpKey = a })

-- | Sets the maximum number of parts to return.
lpMaxParts :: Lens' ListParts (Maybe Integer)
lpMaxParts = lens _lpMaxParts (\s a -> s { _lpMaxParts = a })

-- | Specifies the part after which listing should begin. Only parts with higher
-- part numbers will be listed.
lpPartNumberMarker :: Lens' ListParts (Maybe Integer)
lpPartNumberMarker =
    lens _lpPartNumberMarker (\s a -> s { _lpPartNumberMarker = a })

-- | Upload ID identifying the multipart upload whose parts are being listed.
lpUploadId :: Lens' ListParts Text
lpUploadId = lens _lpUploadId (\s a -> s { _lpUploadId = a })

instance ToPath ListParts

instance ToQuery ListParts

instance ToHeaders ListParts

instance ToBody ListParts

data ListPartsResponse = ListPartsResponse
    { _lprBucket :: Maybe BucketName
    , _lprKey :: Maybe ObjectKey
    , _lprUploadId :: Maybe Text
    , _lprPartNumberMarker :: Maybe Integer
    , _lprNextPartNumberMarker :: Maybe Integer
    , _lprMaxParts :: Maybe Integer
    , _lprIsTruncated :: Bool
    , _lprParts :: [Part]
    , _lprInitiator :: Maybe Initiator
    , _lprOwner :: Maybe Owner
    , _lprStorageClass :: Maybe StorageClass
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListPartsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Maybe BucketName@
--
-- * @Key ::@ @Maybe ObjectKey@
--
-- * @UploadId ::@ @Maybe Text@
--
-- * @PartNumberMarker ::@ @Maybe Integer@
--
-- * @NextPartNumberMarker ::@ @Maybe Integer@
--
-- * @MaxParts ::@ @Maybe Integer@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Parts ::@ @[Part]@
--
-- * @Initiator ::@ @Maybe Initiator@
--
-- * @Owner ::@ @Maybe Owner@
--
-- * @StorageClass ::@ @Maybe StorageClass@
--
listPartsResponse :: Bool -- ^ 'lprIsTruncated'
                  -> ListPartsResponse
listPartsResponse p7 = ListPartsResponse
    { _lprBucket = Nothing
    , _lprKey = Nothing
    , _lprUploadId = Nothing
    , _lprPartNumberMarker = Nothing
    , _lprNextPartNumberMarker = Nothing
    , _lprMaxParts = Nothing
    , _lprIsTruncated = p7
    , _lprParts = mempty
    , _lprInitiator = Nothing
    , _lprOwner = Nothing
    , _lprStorageClass = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
lprBucket :: Lens' ListPartsResponse (Maybe BucketName)
lprBucket = lens _lprBucket (\s a -> s { _lprBucket = a })

-- | Object key for which the multipart upload was initiated.
lprKey :: Lens' ListPartsResponse (Maybe ObjectKey)
lprKey = lens _lprKey (\s a -> s { _lprKey = a })

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprUploadId :: Lens' ListPartsResponse (Maybe Text)
lprUploadId = lens _lprUploadId (\s a -> s { _lprUploadId = a })

-- | Part number after which listing begins.
lprPartNumberMarker :: Lens' ListPartsResponse (Maybe Integer)
lprPartNumberMarker =
    lens _lprPartNumberMarker (\s a -> s { _lprPartNumberMarker = a })

-- | When a list is truncated, this element specifies the last part in the list,
-- as well as the value to use for the part-number-marker request parameter in
-- a subsequent request.
lprNextPartNumberMarker :: Lens' ListPartsResponse (Maybe Integer)
lprNextPartNumberMarker =
    lens _lprNextPartNumberMarker
         (\s a -> s { _lprNextPartNumberMarker = a })

-- | Maximum number of parts that were allowed in the response.
lprMaxParts :: Lens' ListPartsResponse (Maybe Integer)
lprMaxParts = lens _lprMaxParts (\s a -> s { _lprMaxParts = a })

-- | Indicates whether the returned list of parts is truncated.
lprIsTruncated :: Lens' ListPartsResponse Bool
lprIsTruncated = lens _lprIsTruncated (\s a -> s { _lprIsTruncated = a })

lprParts :: Lens' ListPartsResponse [Part]
lprParts = lens _lprParts (\s a -> s { _lprParts = a })

-- | Identifies who initiated the multipart upload.
lprInitiator :: Lens' ListPartsResponse (Maybe Initiator)
lprInitiator = lens _lprInitiator (\s a -> s { _lprInitiator = a })

lprOwner :: Lens' ListPartsResponse (Maybe Owner)
lprOwner = lens _lprOwner (\s a -> s { _lprOwner = a })

-- | The class of storage used to store the object.
lprStorageClass :: Lens' ListPartsResponse (Maybe StorageClass)
lprStorageClass = lens _lprStorageClass (\s a -> s { _lprStorageClass = a })

instance FromXML ListPartsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListParts where
    type Sv ListParts = S3
    type Rs ListParts = ListPartsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListParts where
    next rq rs
        | not (rs ^. lprIsTruncated) = Nothing
        | otherwise = Just $
            rq & lpPartNumberMarker .~ rs ^. lprNextPartNumberMarker
