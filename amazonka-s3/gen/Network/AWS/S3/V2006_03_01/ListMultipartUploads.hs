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
    , listMultipartUploads
    -- ** Request lenses
    , lmurBucket
    , lmurDelimiter
    , lmurEncodingType
    , lmurKeyMarker
    , lmurMaxUploads
    , lmurPrefix
    , lmurUploadIdMarker

    -- * Response
    , ListMultipartUploadsResponse
    -- ** Response lenses
    , lmuoIsTruncated
    , lmuoBucket
    , lmuoCommonPrefixes
    , lmuoEncodingType
    , lmuoKeyMarker
    , lmuoMaxUploads
    , lmuoUploads
    , lmuoNextKeyMarker
    , lmuoNextUploadIdMarker
    , lmuoPrefix
    , lmuoUploadIdMarker
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListMultipartUploads' request.
listMultipartUploads :: BucketName -- ^ 'lmurBucket'
                     -> ListMultipartUploads
listMultipartUploads p1 = ListMultipartUploads
    { _lmurBucket = p1
    , _lmurDelimiter = Nothing
    , _lmurEncodingType = Nothing
    , _lmurKeyMarker = Nothing
    , _lmurMaxUploads = Nothing
    , _lmurPrefix = Nothing
    , _lmurUploadIdMarker = Nothing
    }
{-# INLINE listMultipartUploads #-}

data ListMultipartUploads = ListMultipartUploads
    { _lmurBucket :: BucketName
    , _lmurDelimiter :: Maybe Char
      -- ^ Character you use to group keys.
    , _lmurEncodingType :: Maybe EncodingType
      -- ^ Requests Amazon S3 to encode the object keys in the response and
      -- specifies the encoding method to use. An object key may contain
      -- any Unicode character; however, XML 1.0 parser cannot parse some
      -- characters, such as characters with an ASCII value from 0 to 10.
      -- For characters that are not supported in XML 1.0, you can add
      -- this parameter to request that Amazon S3 encode the keys in the
      -- response.
    , _lmurKeyMarker :: Maybe Text
      -- ^ Together with upload-id-marker, this parameter specifies the
      -- multipart upload after which listing should begin.
    , _lmurMaxUploads :: Maybe Integer
      -- ^ Sets the maximum number of multipart uploads, from 1 to 1,000, to
      -- return in the response body. 1,000 is the maximum number of
      -- uploads that can be returned in a response.
    , _lmurPrefix :: Maybe Text
      -- ^ Lists in-progress uploads only for those keys that begin with the
      -- specified prefix.
    , _lmurUploadIdMarker :: Maybe Text
      -- ^ Together with key-marker, specifies the multipart upload after
      -- which listing should begin. If key-marker is not specified, the
      -- upload-id-marker parameter is ignored.
    } deriving (Show, Generic)

lmurBucket :: Lens' ListMultipartUploads (BucketName)
lmurBucket f x =
    f (_lmurBucket x)
        <&> \y -> x { _lmurBucket = y }
{-# INLINE lmurBucket #-}

-- | Character you use to group keys.
lmurDelimiter :: Lens' ListMultipartUploads (Maybe Char)
lmurDelimiter f x =
    f (_lmurDelimiter x)
        <&> \y -> x { _lmurDelimiter = y }
{-# INLINE lmurDelimiter #-}

-- | Requests Amazon S3 to encode the object keys in the response and specifies
-- the encoding method to use. An object key may contain any Unicode
-- character; however, XML 1.0 parser cannot parse some characters, such as
-- characters with an ASCII value from 0 to 10. For characters that are not
-- supported in XML 1.0, you can add this parameter to request that Amazon S3
-- encode the keys in the response.
lmurEncodingType :: Lens' ListMultipartUploads (Maybe EncodingType)
lmurEncodingType f x =
    f (_lmurEncodingType x)
        <&> \y -> x { _lmurEncodingType = y }
{-# INLINE lmurEncodingType #-}

-- | Together with upload-id-marker, this parameter specifies the multipart
-- upload after which listing should begin.
lmurKeyMarker :: Lens' ListMultipartUploads (Maybe Text)
lmurKeyMarker f x =
    f (_lmurKeyMarker x)
        <&> \y -> x { _lmurKeyMarker = y }
{-# INLINE lmurKeyMarker #-}

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return in
-- the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
lmurMaxUploads :: Lens' ListMultipartUploads (Maybe Integer)
lmurMaxUploads f x =
    f (_lmurMaxUploads x)
        <&> \y -> x { _lmurMaxUploads = y }
{-# INLINE lmurMaxUploads #-}

-- | Lists in-progress uploads only for those keys that begin with the specified
-- prefix.
lmurPrefix :: Lens' ListMultipartUploads (Maybe Text)
lmurPrefix f x =
    f (_lmurPrefix x)
        <&> \y -> x { _lmurPrefix = y }
{-# INLINE lmurPrefix #-}

-- | Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the upload-id-marker
-- parameter is ignored.
lmurUploadIdMarker :: Lens' ListMultipartUploads (Maybe Text)
lmurUploadIdMarker f x =
    f (_lmurUploadIdMarker x)
        <&> \y -> x { _lmurUploadIdMarker = y }
{-# INLINE lmurUploadIdMarker #-}

instance ToPath ListMultipartUploads where
    toPath ListMultipartUploads{..} = mconcat
        [ "/"
        , toBS _lmurBucket
        ]

instance ToQuery ListMultipartUploads where
    toQuery ListMultipartUploads{..} = mconcat
        [ "delimiter" =? _lmurDelimiter
        , "encoding-type" =? _lmurEncodingType
        , "key-marker" =? _lmurKeyMarker
        , "max-uploads" =? _lmurMaxUploads
        , "upload-id-marker" =? _lmurUploadIdMarker
        , "uploads&prefix" =? _lmurPrefix
        ]

instance ToHeaders ListMultipartUploads

instance ToBody ListMultipartUploads

data ListMultipartUploadsResponse = ListMultipartUploadsResponse
    { _lmuoIsTruncated :: Bool
      -- ^ Indicates whether the returned list of multipart uploads is
      -- truncated. A value of true indicates that the list was truncated.
      -- The list can be truncated if the number of multipart uploads
      -- exceeds the limit allowed or specified by max uploads.
    , _lmuoBucket :: Maybe BucketName
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , _lmuoCommonPrefixes :: [CommonPrefix]
    , _lmuoEncodingType :: Maybe EncodingType
      -- ^ Encoding type used by Amazon S3 to encode object keys in the
      -- response.
    , _lmuoKeyMarker :: Maybe Text
      -- ^ The key at or after which the listing began.
    , _lmuoMaxUploads :: Maybe Integer
      -- ^ Maximum number of multipart uploads that could have been included
      -- in the response.
    , _lmuoUploads :: [MultipartUpload]
    , _lmuoNextKeyMarker :: Maybe Text
      -- ^ When a list is truncated, this element specifies the value that
      -- should be used for the key-marker request parameter in a
      -- subsequent request.
    , _lmuoNextUploadIdMarker :: Maybe Text
      -- ^ When a list is truncated, this element specifies the value that
      -- should be used for the upload-id-marker request parameter in a
      -- subsequent request.
    , _lmuoPrefix :: Maybe Text
      -- ^ When a prefix is provided in the request, this field contains the
      -- specified prefix. The result contains only keys starting with the
      -- specified prefix.
    , _lmuoUploadIdMarker :: Maybe Text
      -- ^ Upload ID after which listing began.
    } deriving (Show, Generic)

-- | Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed or
-- specified by max uploads.
lmuoIsTruncated :: Lens' ListMultipartUploadsResponse (Bool)
lmuoIsTruncated f x =
    f (_lmuoIsTruncated x)
        <&> \y -> x { _lmuoIsTruncated = y }
{-# INLINE lmuoIsTruncated #-}

-- | Name of the bucket to which the multipart upload was initiated.
lmuoBucket :: Lens' ListMultipartUploadsResponse (Maybe BucketName)
lmuoBucket f x =
    f (_lmuoBucket x)
        <&> \y -> x { _lmuoBucket = y }
{-# INLINE lmuoBucket #-}

lmuoCommonPrefixes :: Lens' ListMultipartUploadsResponse ([CommonPrefix])
lmuoCommonPrefixes f x =
    f (_lmuoCommonPrefixes x)
        <&> \y -> x { _lmuoCommonPrefixes = y }
{-# INLINE lmuoCommonPrefixes #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lmuoEncodingType :: Lens' ListMultipartUploadsResponse (Maybe EncodingType)
lmuoEncodingType f x =
    f (_lmuoEncodingType x)
        <&> \y -> x { _lmuoEncodingType = y }
{-# INLINE lmuoEncodingType #-}

-- | The key at or after which the listing began.
lmuoKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmuoKeyMarker f x =
    f (_lmuoKeyMarker x)
        <&> \y -> x { _lmuoKeyMarker = y }
{-# INLINE lmuoKeyMarker #-}

-- | Maximum number of multipart uploads that could have been included in the
-- response.
lmuoMaxUploads :: Lens' ListMultipartUploadsResponse (Maybe Integer)
lmuoMaxUploads f x =
    f (_lmuoMaxUploads x)
        <&> \y -> x { _lmuoMaxUploads = y }
{-# INLINE lmuoMaxUploads #-}

lmuoUploads :: Lens' ListMultipartUploadsResponse ([MultipartUpload])
lmuoUploads f x =
    f (_lmuoUploads x)
        <&> \y -> x { _lmuoUploads = y }
{-# INLINE lmuoUploads #-}

-- | When a list is truncated, this element specifies the value that should be
-- used for the key-marker request parameter in a subsequent request.
lmuoNextKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmuoNextKeyMarker f x =
    f (_lmuoNextKeyMarker x)
        <&> \y -> x { _lmuoNextKeyMarker = y }
{-# INLINE lmuoNextKeyMarker #-}

-- | When a list is truncated, this element specifies the value that should be
-- used for the upload-id-marker request parameter in a subsequent request.
lmuoNextUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmuoNextUploadIdMarker f x =
    f (_lmuoNextUploadIdMarker x)
        <&> \y -> x { _lmuoNextUploadIdMarker = y }
{-# INLINE lmuoNextUploadIdMarker #-}

-- | When a prefix is provided in the request, this field contains the specified
-- prefix. The result contains only keys starting with the specified prefix.
lmuoPrefix :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmuoPrefix f x =
    f (_lmuoPrefix x)
        <&> \y -> x { _lmuoPrefix = y }
{-# INLINE lmuoPrefix #-}

-- | Upload ID after which listing began.
lmuoUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmuoUploadIdMarker f x =
    f (_lmuoUploadIdMarker x)
        <&> \y -> x { _lmuoUploadIdMarker = y }
{-# INLINE lmuoUploadIdMarker #-}

instance FromXML ListMultipartUploadsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListMultipartUploads where
    type Sv ListMultipartUploads = S3
    type Rs ListMultipartUploads = ListMultipartUploadsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListMultipartUploads where
    next rq rs
        | not (_lmuoIsTruncated rs) = Nothing
        | and [isNothing p1, isNothing p2] = Nothing
        | otherwise = Just $ rq
            { _lmurKeyMarker = p1
            , _lmurUploadIdMarker = p2
            }
      where
        p1 = _lmuoNextKeyMarker rs
        p2 = _lmuoNextUploadIdMarker rs
