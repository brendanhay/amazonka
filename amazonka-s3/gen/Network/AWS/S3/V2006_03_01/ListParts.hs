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
    , mkListParts
    -- ** Request lenses
    , lpBucket
    , lpKey
    , lpMaxParts
    , lpPartNumberMarker
    , lpUploadId

    -- * Response
    , ListPartsResponse
    -- ** Response lenses
    , lprsBucket
    , lprsKey
    , lprsUploadId
    , lprsPartNumberMarker
    , lprsNextPartNumberMarker
    , lprsMaxParts
    , lprsIsTruncated
    , lprsParts
    , lprsInitiator
    , lprsOwner
    , lprsStorageClass
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data ListParts = ListParts
    { _lpBucket :: BucketName
    , _lpKey :: ObjectKey
    , _lpMaxParts :: Maybe Integer
    , _lpPartNumberMarker :: Maybe Integer
    , _lpUploadId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListParts' request.
mkListParts :: BucketName -- ^ 'lpBucket'
            -> ObjectKey -- ^ 'lpKey'
            -> Text -- ^ 'lpUploadId'
            -> ListParts
mkListParts p1 p2 p5 = ListParts
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

instance ToPath ListParts where
    toPath ListParts{..} = mconcat
        [ "/"
        , toBS _lpBucket
        , "/"
        , toBS _lpKey
        ]

instance ToQuery ListParts where
    toQuery ListParts{..} = mconcat
        [ "max-parts" =? _lpMaxParts
        , "part-number-marker" =? _lpPartNumberMarker
        , "uploadId" =? _lpUploadId
        ]

instance ToHeaders ListParts

instance ToBody ListParts

data ListPartsResponse = ListPartsResponse
    { _lprsBucket :: Maybe BucketName
    , _lprsKey :: Maybe ObjectKey
    , _lprsUploadId :: Maybe Text
    , _lprsPartNumberMarker :: Maybe Integer
    , _lprsNextPartNumberMarker :: Maybe Integer
    , _lprsMaxParts :: Maybe Integer
    , _lprsIsTruncated :: Bool
    , _lprsParts :: [Part]
    , _lprsInitiator :: Maybe Initiator
    , _lprsOwner :: Maybe Owner
    , _lprsStorageClass :: Maybe StorageClass
    } deriving (Show, Generic)

-- | Name of the bucket to which the multipart upload was initiated.
lprsBucket :: Lens' ListPartsResponse (Maybe BucketName)
lprsBucket = lens _lprsBucket (\s a -> s { _lprsBucket = a })

-- | Object key for which the multipart upload was initiated.
lprsKey :: Lens' ListPartsResponse (Maybe ObjectKey)
lprsKey = lens _lprsKey (\s a -> s { _lprsKey = a })

-- | Upload ID identifying the multipart upload whose parts are being listed.
lprsUploadId :: Lens' ListPartsResponse (Maybe Text)
lprsUploadId = lens _lprsUploadId (\s a -> s { _lprsUploadId = a })

-- | Part number after which listing begins.
lprsPartNumberMarker :: Lens' ListPartsResponse (Maybe Integer)
lprsPartNumberMarker =
    lens _lprsPartNumberMarker (\s a -> s { _lprsPartNumberMarker = a })

-- | When a list is truncated, this element specifies the last part in the list,
-- as well as the value to use for the part-number-marker request parameter in
-- a subsequent request.
lprsNextPartNumberMarker :: Lens' ListPartsResponse (Maybe Integer)
lprsNextPartNumberMarker =
    lens _lprsNextPartNumberMarker
         (\s a -> s { _lprsNextPartNumberMarker = a })

-- | Maximum number of parts that were allowed in the response.
lprsMaxParts :: Lens' ListPartsResponse (Maybe Integer)
lprsMaxParts = lens _lprsMaxParts (\s a -> s { _lprsMaxParts = a })

-- | Indicates whether the returned list of parts is truncated.
lprsIsTruncated :: Lens' ListPartsResponse Bool
lprsIsTruncated = lens _lprsIsTruncated (\s a -> s { _lprsIsTruncated = a })

lprsParts :: Lens' ListPartsResponse [Part]
lprsParts = lens _lprsParts (\s a -> s { _lprsParts = a })

-- | Identifies who initiated the multipart upload.
lprsInitiator :: Lens' ListPartsResponse (Maybe Initiator)
lprsInitiator = lens _lprsInitiator (\s a -> s { _lprsInitiator = a })

lprsOwner :: Lens' ListPartsResponse (Maybe Owner)
lprsOwner = lens _lprsOwner (\s a -> s { _lprsOwner = a })

-- | The class of storage used to store the object.
lprsStorageClass :: Lens' ListPartsResponse (Maybe StorageClass)
lprsStorageClass =
    lens _lprsStorageClass (\s a -> s { _lprsStorageClass = a })

instance FromXML ListPartsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListParts where
    type Sv ListParts = S3
    type Rs ListParts = ListPartsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListParts where
    next rq rs
        | not (rs ^. lprsIsTruncated) = Nothing
        | otherwise = Just $
            rq & lpPartNumberMarker .~ rs ^. lprsNextPartNumberMarker
