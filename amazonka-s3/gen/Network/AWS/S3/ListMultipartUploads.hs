{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListMultipartUploads
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation lists in-progress multipart uploads.
module Network.AWS.S3.ListMultipartUploads
    (
    -- * Request
      ListMultipartUploads
    -- ** Request constructor
    , mkListMultipartUploads
    -- ** Request lenses
    , lmuBucket
    , lmuDelimiter
    , lmuEncodingType
    , lmuKeyMarker
    , lmuMaxUploads
    , lmuPrefix
    , lmuUploadIdMarker

    -- * Response
    , ListMultipartUploadsResponse
    -- ** Response constructor
    , mkListMultipartUploadsResponse
    -- ** Response lenses
    , lmurBucket
    , lmurKeyMarker
    , lmurUploadIdMarker
    , lmurNextKeyMarker
    , lmurPrefix
    , lmurNextUploadIdMarker
    , lmurMaxUploads
    , lmurIsTruncated
    , lmurUploads
    , lmurCommonPrefixes
    , lmurEncodingType
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data ListMultipartUploads = ListMultipartUploads
    { _lmuBucket :: !BucketName
    , _lmuDelimiter :: !(Maybe Char)
    , _lmuEncodingType :: Maybe EncodingType
    , _lmuKeyMarker :: !(Maybe Text)
    , _lmuMaxUploads :: !(Maybe Integer)
    , _lmuPrefix :: !(Maybe Text)
    , _lmuUploadIdMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListMultipartUploads' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Delimiter ::@ @Maybe Char@
--
-- * @EncodingType ::@ @Maybe EncodingType@
--
-- * @KeyMarker ::@ @Maybe Text@
--
-- * @MaxUploads ::@ @Maybe Integer@
--
-- * @Prefix ::@ @Maybe Text@
--
-- * @UploadIdMarker ::@ @Maybe Text@
--
mkListMultipartUploads :: BucketName -- ^ 'lmuBucket'
                       -> ListMultipartUploads
mkListMultipartUploads p1 = ListMultipartUploads
    { _lmuBucket = p1
    , _lmuDelimiter = Nothing
    , _lmuEncodingType = Nothing
    , _lmuKeyMarker = Nothing
    , _lmuMaxUploads = Nothing
    , _lmuPrefix = Nothing
    , _lmuUploadIdMarker = Nothing
    }

lmuBucket :: Lens' ListMultipartUploads BucketName
lmuBucket = lens _lmuBucket (\s a -> s { _lmuBucket = a })

-- | Character you use to group keys.
lmuDelimiter :: Lens' ListMultipartUploads (Maybe Char)
lmuDelimiter = lens _lmuDelimiter (\s a -> s { _lmuDelimiter = a })

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
lmuEncodingType :: Lens' ListMultipartUploads (Maybe EncodingType)
lmuEncodingType = lens _lmuEncodingType (\s a -> s { _lmuEncodingType = a })

-- | Together with upload-id-marker, this parameter specifies the multipart
-- upload after which listing should begin.
lmuKeyMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuKeyMarker = lens _lmuKeyMarker (\s a -> s { _lmuKeyMarker = a })

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return in
-- the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
lmuMaxUploads :: Lens' ListMultipartUploads (Maybe Integer)
lmuMaxUploads = lens _lmuMaxUploads (\s a -> s { _lmuMaxUploads = a })

-- | Lists in-progress uploads only for those keys that begin with the specified
-- prefix.
lmuPrefix :: Lens' ListMultipartUploads (Maybe Text)
lmuPrefix = lens _lmuPrefix (\s a -> s { _lmuPrefix = a })

-- | Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the upload-id-marker
-- parameter is ignored.
lmuUploadIdMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuUploadIdMarker =
    lens _lmuUploadIdMarker (\s a -> s { _lmuUploadIdMarker = a })

instance ToPath ListMultipartUploads

instance ToQuery ListMultipartUploads

instance ToHeaders ListMultipartUploads

instance ToBody ListMultipartUploads

data ListMultipartUploadsResponse = ListMultipartUploadsResponse
    { _lmurBucket :: !(Maybe BucketName)
    , _lmurKeyMarker :: !(Maybe Text)
    , _lmurUploadIdMarker :: !(Maybe Text)
    , _lmurNextKeyMarker :: !(Maybe Text)
    , _lmurPrefix :: !(Maybe Text)
    , _lmurNextUploadIdMarker :: !(Maybe Text)
    , _lmurMaxUploads :: !(Maybe Integer)
    , _lmurIsTruncated :: !Bool
    , _lmurUploads :: [MultipartUpload]
    , _lmurCommonPrefixes :: [CommonPrefix]
    , _lmurEncodingType :: Maybe EncodingType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListMultipartUploadsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Maybe BucketName@
--
-- * @KeyMarker ::@ @Maybe Text@
--
-- * @UploadIdMarker ::@ @Maybe Text@
--
-- * @NextKeyMarker ::@ @Maybe Text@
--
-- * @Prefix ::@ @Maybe Text@
--
-- * @NextUploadIdMarker ::@ @Maybe Text@
--
-- * @MaxUploads ::@ @Maybe Integer@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @Uploads ::@ @[MultipartUpload]@
--
-- * @CommonPrefixes ::@ @[CommonPrefix]@
--
-- * @EncodingType ::@ @Maybe EncodingType@
--
mkListMultipartUploadsResponse :: Bool -- ^ 'lmurIsTruncated'
                               -> ListMultipartUploadsResponse
mkListMultipartUploadsResponse p8 = ListMultipartUploadsResponse
    { _lmurBucket = Nothing
    , _lmurKeyMarker = Nothing
    , _lmurUploadIdMarker = Nothing
    , _lmurNextKeyMarker = Nothing
    , _lmurPrefix = Nothing
    , _lmurNextUploadIdMarker = Nothing
    , _lmurMaxUploads = Nothing
    , _lmurIsTruncated = p8
    , _lmurUploads = mempty
    , _lmurCommonPrefixes = mempty
    , _lmurEncodingType = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
lmurBucket :: Lens' ListMultipartUploadsResponse (Maybe BucketName)
lmurBucket = lens _lmurBucket (\s a -> s { _lmurBucket = a })

-- | The key at or after which the listing began.
lmurKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurKeyMarker = lens _lmurKeyMarker (\s a -> s { _lmurKeyMarker = a })

-- | Upload ID after which listing began.
lmurUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurUploadIdMarker =
    lens _lmurUploadIdMarker (\s a -> s { _lmurUploadIdMarker = a })

-- | When a list is truncated, this element specifies the value that should be
-- used for the key-marker request parameter in a subsequent request.
lmurNextKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurNextKeyMarker =
    lens _lmurNextKeyMarker (\s a -> s { _lmurNextKeyMarker = a })

-- | When a prefix is provided in the request, this field contains the specified
-- prefix. The result contains only keys starting with the specified prefix.
lmurPrefix :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurPrefix = lens _lmurPrefix (\s a -> s { _lmurPrefix = a })

-- | When a list is truncated, this element specifies the value that should be
-- used for the upload-id-marker request parameter in a subsequent request.
lmurNextUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurNextUploadIdMarker =
    lens _lmurNextUploadIdMarker (\s a -> s { _lmurNextUploadIdMarker = a })

-- | Maximum number of multipart uploads that could have been included in the
-- response.
lmurMaxUploads :: Lens' ListMultipartUploadsResponse (Maybe Integer)
lmurMaxUploads = lens _lmurMaxUploads (\s a -> s { _lmurMaxUploads = a })

-- | Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed or
-- specified by max uploads.
lmurIsTruncated :: Lens' ListMultipartUploadsResponse Bool
lmurIsTruncated = lens _lmurIsTruncated (\s a -> s { _lmurIsTruncated = a })

lmurUploads :: Lens' ListMultipartUploadsResponse [MultipartUpload]
lmurUploads = lens _lmurUploads (\s a -> s { _lmurUploads = a })

lmurCommonPrefixes :: Lens' ListMultipartUploadsResponse [CommonPrefix]
lmurCommonPrefixes =
    lens _lmurCommonPrefixes (\s a -> s { _lmurCommonPrefixes = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lmurEncodingType :: Lens' ListMultipartUploadsResponse (Maybe EncodingType)
lmurEncodingType =
    lens _lmurEncodingType (\s a -> s { _lmurEncodingType = a })

instance FromXML ListMultipartUploadsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListMultipartUploads where
    type Sv ListMultipartUploads = S3
    type Rs ListMultipartUploads = ListMultipartUploadsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListMultipartUploads where
    next rq rs
        | not (rs ^. lmurIsTruncated) = Nothing
        | isNothing p1 && isNothing p2 = Nothing
        | otherwise = Just $ rq
            & lmuKeyMarker .~ p1
            & lmuUploadIdMarker .~ p2
      where
        p1 = rs ^. lmurNextKeyMarker
        p2 = rs ^. lmurNextUploadIdMarker
