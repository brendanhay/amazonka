{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.S3.V2006_03_01.ListMultipartUploads where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ListMultipartUploads' request.
listMultipartUploads :: BucketName -- ^ '_lmurBucket'
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
    } deriving (Generic)

instance ToPath ListMultipartUploads where
    toPath ListMultipartUploads{..} = mconcat
        [ "/"
        , toBS _lmurBucket
        ]

instance ToQuery ListMultipartUploads

instance ToHeaders ListMultipartUploads

instance ToBody ListMultipartUploads

instance AWSRequest ListMultipartUploads where
    type Sv ListMultipartUploads = S3
    type Rs ListMultipartUploads = ListMultipartUploadsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListMultipartUploads where
    next rq rs
        | not (_lmuoIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lmurKeyMarker = _lmuoNextKeyMarker rs
            , _lmurUploadIdMarker = _lmuoNextUploadIdMarker rs
            }

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
    } deriving (Generic)

instance FromXML ListMultipartUploadsResponse where
    fromXMLOptions = xmlOptions
