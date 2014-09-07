{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.ListMultipartUploads
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation lists in-progress multipart uploads.
module Network.AWS.S3.V2006_03_01.ListMultipartUploads
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
    -- ** Response lenses
    , lmursBucket
    , lmursKeyMarker
    , lmursUploadIdMarker
    , lmursNextKeyMarker
    , lmursPrefix
    , lmursNextUploadIdMarker
    , lmursMaxUploads
    , lmursIsTruncated
    , lmursUploads
    , lmursCommonPrefixes
    , lmursEncodingType
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data ListMultipartUploads = ListMultipartUploads
    { _lmuBucket :: BucketName
    , _lmuDelimiter :: Maybe Char
    , _lmuEncodingType :: Maybe EncodingType
    , _lmuKeyMarker :: Maybe Text
    , _lmuMaxUploads :: Maybe Integer
    , _lmuPrefix :: Maybe Text
    , _lmuUploadIdMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListMultipartUploads' request.
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

instance ToPath ListMultipartUploads where
    toPath ListMultipartUploads{..} = mconcat
        [ "/"
        , toBS _lmuBucket
        ]

instance ToQuery ListMultipartUploads where
    toQuery ListMultipartUploads{..} = mconcat
        [ "delimiter" =? _lmuDelimiter
        , "encoding-type" =? _lmuEncodingType
        , "key-marker" =? _lmuKeyMarker
        , "max-uploads" =? _lmuMaxUploads
        , "upload-id-marker" =? _lmuUploadIdMarker
        , "uploads&prefix" =? _lmuPrefix
        ]

instance ToHeaders ListMultipartUploads

instance ToBody ListMultipartUploads

data ListMultipartUploadsResponse = ListMultipartUploadsResponse
    { _lmursBucket :: Maybe BucketName
    , _lmursKeyMarker :: Maybe Text
    , _lmursUploadIdMarker :: Maybe Text
    , _lmursNextKeyMarker :: Maybe Text
    , _lmursPrefix :: Maybe Text
    , _lmursNextUploadIdMarker :: Maybe Text
    , _lmursMaxUploads :: Maybe Integer
    , _lmursIsTruncated :: Bool
    , _lmursUploads :: [MultipartUpload]
    , _lmursCommonPrefixes :: [CommonPrefix]
    , _lmursEncodingType :: Maybe EncodingType
    } deriving (Show, Generic)

-- | Name of the bucket to which the multipart upload was initiated.
lmursBucket :: Lens' ListMultipartUploadsResponse (Maybe BucketName)
lmursBucket = lens _lmursBucket (\s a -> s { _lmursBucket = a })

-- | The key at or after which the listing began.
lmursKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursKeyMarker = lens _lmursKeyMarker (\s a -> s { _lmursKeyMarker = a })

-- | Upload ID after which listing began.
lmursUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursUploadIdMarker =
    lens _lmursUploadIdMarker (\s a -> s { _lmursUploadIdMarker = a })

-- | When a list is truncated, this element specifies the value that should be
-- used for the key-marker request parameter in a subsequent request.
lmursNextKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursNextKeyMarker =
    lens _lmursNextKeyMarker (\s a -> s { _lmursNextKeyMarker = a })

-- | When a prefix is provided in the request, this field contains the specified
-- prefix. The result contains only keys starting with the specified prefix.
lmursPrefix :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursPrefix = lens _lmursPrefix (\s a -> s { _lmursPrefix = a })

-- | When a list is truncated, this element specifies the value that should be
-- used for the upload-id-marker request parameter in a subsequent request.
lmursNextUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursNextUploadIdMarker =
    lens _lmursNextUploadIdMarker
         (\s a -> s { _lmursNextUploadIdMarker = a })

-- | Maximum number of multipart uploads that could have been included in the
-- response.
lmursMaxUploads :: Lens' ListMultipartUploadsResponse (Maybe Integer)
lmursMaxUploads = lens _lmursMaxUploads (\s a -> s { _lmursMaxUploads = a })

-- | Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed or
-- specified by max uploads.
lmursIsTruncated :: Lens' ListMultipartUploadsResponse Bool
lmursIsTruncated =
    lens _lmursIsTruncated (\s a -> s { _lmursIsTruncated = a })

lmursUploads :: Lens' ListMultipartUploadsResponse [MultipartUpload]
lmursUploads = lens _lmursUploads (\s a -> s { _lmursUploads = a })

lmursCommonPrefixes :: Lens' ListMultipartUploadsResponse [CommonPrefix]
lmursCommonPrefixes =
    lens _lmursCommonPrefixes (\s a -> s { _lmursCommonPrefixes = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lmursEncodingType :: Lens' ListMultipartUploadsResponse (Maybe EncodingType)
lmursEncodingType =
    lens _lmursEncodingType (\s a -> s { _lmursEncodingType = a })

instance FromXML ListMultipartUploadsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListMultipartUploads where
    type Sv ListMultipartUploads = S3
    type Rs ListMultipartUploads = ListMultipartUploadsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListMultipartUploads where
    next rq rs
        | not (rs ^. lmursIsTruncated) = Nothing
        | isNothing p1 && isNothing p2 = Nothing
        | otherwise = Just $ rq
            & lmuKeyMarker .~ p1
            & lmuUploadIdMarker .~ p2
      where
        p1 = lmursNextKeyMarker rs
        p2 = lmursNextUploadIdMarker rs
