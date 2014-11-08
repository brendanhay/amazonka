{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , ListMultipartUploadsOutput
    -- ** Response constructor
    , listMultipartUploadsOutput
    -- ** Response lenses
    , lmuoBucket
    , lmuoCommonPrefixes
    , lmuoEncodingType
    , lmuoIsTruncated
    , lmuoKeyMarker
    , lmuoMaxUploads
    , lmuoNextKeyMarker
    , lmuoNextUploadIdMarker
    , lmuoPrefix
    , lmuoUploadIdMarker
    , lmuoUploads
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data ListMultipartUploads = ListMultipartUploads
    { _lmurBucket         :: Text
    , _lmurDelimiter      :: Maybe Text
    , _lmurEncodingType   :: Maybe Text
    , _lmurKeyMarker      :: Maybe Text
    , _lmurMaxUploads     :: Maybe Int
    , _lmurPrefix         :: Maybe Text
    , _lmurUploadIdMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListMultipartUploads' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmurBucket' @::@ 'Text'
--
-- * 'lmurDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'lmurEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lmurKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmurMaxUploads' @::@ 'Maybe' 'Int'
--
-- * 'lmurPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lmurUploadIdMarker' @::@ 'Maybe' 'Text'
--
listMultipartUploads :: Text -- ^ 'lmurBucket'
                     -> ListMultipartUploads
listMultipartUploads p1 = ListMultipartUploads
    { _lmurBucket         = p1
    , _lmurDelimiter      = Nothing
    , _lmurEncodingType   = Nothing
    , _lmurKeyMarker      = Nothing
    , _lmurMaxUploads     = Nothing
    , _lmurPrefix         = Nothing
    , _lmurUploadIdMarker = Nothing
    }

lmurBucket :: Lens' ListMultipartUploads Text
lmurBucket = lens _lmurBucket (\s a -> s { _lmurBucket = a })

-- | Character you use to group keys.
lmurDelimiter :: Lens' ListMultipartUploads (Maybe Text)
lmurDelimiter = lens _lmurDelimiter (\s a -> s { _lmurDelimiter = a })

lmurEncodingType :: Lens' ListMultipartUploads (Maybe Text)
lmurEncodingType = lens _lmurEncodingType (\s a -> s { _lmurEncodingType = a })

-- | Together with upload-id-marker, this parameter specifies the multipart
-- upload after which listing should begin.
lmurKeyMarker :: Lens' ListMultipartUploads (Maybe Text)
lmurKeyMarker = lens _lmurKeyMarker (\s a -> s { _lmurKeyMarker = a })

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return
-- in the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
lmurMaxUploads :: Lens' ListMultipartUploads (Maybe Int)
lmurMaxUploads = lens _lmurMaxUploads (\s a -> s { _lmurMaxUploads = a })

-- | Lists in-progress uploads only for those keys that begin with the
-- specified prefix.
lmurPrefix :: Lens' ListMultipartUploads (Maybe Text)
lmurPrefix = lens _lmurPrefix (\s a -> s { _lmurPrefix = a })

-- | Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the
-- upload-id-marker parameter is ignored.
lmurUploadIdMarker :: Lens' ListMultipartUploads (Maybe Text)
lmurUploadIdMarker =
    lens _lmurUploadIdMarker (\s a -> s { _lmurUploadIdMarker = a })

instance ToPath ListMultipartUploads where
    toPath ListMultipartUploads{..} = mconcat
        [ "/"
        , toText _lmurBucket
        ]

instance ToQuery ListMultipartUploads where
    toQuery ListMultipartUploads{..} = mconcat
        [ "uploads"
        , "delimiter"        =? _lmurDelimiter
        , "encoding-type"    =? _lmurEncodingType
        , "key-marker"       =? _lmurKeyMarker
        , "max-uploads"      =? _lmurMaxUploads
        , "prefix"           =? _lmurPrefix
        , "upload-id-marker" =? _lmurUploadIdMarker
        ]

instance ToHeaders ListMultipartUploads

data ListMultipartUploadsOutput = ListMultipartUploadsOutput
    { _lmuoBucket             :: Maybe Text
    , _lmuoCommonPrefixes     :: [CommonPrefix]
    , _lmuoEncodingType       :: Maybe Text
    , _lmuoIsTruncated        :: Maybe Bool
    , _lmuoKeyMarker          :: Maybe Text
    , _lmuoMaxUploads         :: Maybe Int
    , _lmuoNextKeyMarker      :: Maybe Text
    , _lmuoNextUploadIdMarker :: Maybe Text
    , _lmuoPrefix             :: Maybe Text
    , _lmuoUploadIdMarker     :: Maybe Text
    , _lmuoUploads            :: [MultipartUpload]
    } deriving (Eq, Show, Generic)

-- | 'ListMultipartUploadsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmuoBucket' @::@ 'Maybe' 'Text'
--
-- * 'lmuoCommonPrefixes' @::@ ['CommonPrefix']
--
-- * 'lmuoEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lmuoIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lmuoKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmuoMaxUploads' @::@ 'Maybe' 'Int'
--
-- * 'lmuoNextKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmuoNextUploadIdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmuoPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lmuoUploadIdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmuoUploads' @::@ ['MultipartUpload']
--
listMultipartUploadsOutput :: ListMultipartUploadsOutput
listMultipartUploadsOutput = ListMultipartUploadsOutput
    { _lmuoBucket             = Nothing
    , _lmuoKeyMarker          = Nothing
    , _lmuoUploadIdMarker     = Nothing
    , _lmuoNextKeyMarker      = Nothing
    , _lmuoPrefix             = Nothing
    , _lmuoNextUploadIdMarker = Nothing
    , _lmuoMaxUploads         = Nothing
    , _lmuoIsTruncated        = Nothing
    , _lmuoUploads            = mempty
    , _lmuoCommonPrefixes     = mempty
    , _lmuoEncodingType       = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
lmuoBucket :: Lens' ListMultipartUploadsOutput (Maybe Text)
lmuoBucket = lens _lmuoBucket (\s a -> s { _lmuoBucket = a })

lmuoCommonPrefixes :: Lens' ListMultipartUploadsOutput [CommonPrefix]
lmuoCommonPrefixes =
    lens _lmuoCommonPrefixes (\s a -> s { _lmuoCommonPrefixes = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lmuoEncodingType :: Lens' ListMultipartUploadsOutput (Maybe Text)
lmuoEncodingType = lens _lmuoEncodingType (\s a -> s { _lmuoEncodingType = a })

-- | Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed or
-- specified by max uploads.
lmuoIsTruncated :: Lens' ListMultipartUploadsOutput (Maybe Bool)
lmuoIsTruncated = lens _lmuoIsTruncated (\s a -> s { _lmuoIsTruncated = a })

-- | The key at or after which the listing began.
lmuoKeyMarker :: Lens' ListMultipartUploadsOutput (Maybe Text)
lmuoKeyMarker = lens _lmuoKeyMarker (\s a -> s { _lmuoKeyMarker = a })

-- | Maximum number of multipart uploads that could have been included in the
-- response.
lmuoMaxUploads :: Lens' ListMultipartUploadsOutput (Maybe Int)
lmuoMaxUploads = lens _lmuoMaxUploads (\s a -> s { _lmuoMaxUploads = a })

-- | When a list is truncated, this element specifies the value that should be
-- used for the key-marker request parameter in a subsequent request.
lmuoNextKeyMarker :: Lens' ListMultipartUploadsOutput (Maybe Text)
lmuoNextKeyMarker =
    lens _lmuoNextKeyMarker (\s a -> s { _lmuoNextKeyMarker = a })

-- | When a list is truncated, this element specifies the value that should be
-- used for the upload-id-marker request parameter in a subsequent request.
lmuoNextUploadIdMarker :: Lens' ListMultipartUploadsOutput (Maybe Text)
lmuoNextUploadIdMarker =
    lens _lmuoNextUploadIdMarker (\s a -> s { _lmuoNextUploadIdMarker = a })

-- | When a prefix is provided in the request, this field contains the
-- specified prefix. The result contains only keys starting with the
-- specified prefix.
lmuoPrefix :: Lens' ListMultipartUploadsOutput (Maybe Text)
lmuoPrefix = lens _lmuoPrefix (\s a -> s { _lmuoPrefix = a })

-- | Upload ID after which listing began.
lmuoUploadIdMarker :: Lens' ListMultipartUploadsOutput (Maybe Text)
lmuoUploadIdMarker =
    lens _lmuoUploadIdMarker (\s a -> s { _lmuoUploadIdMarker = a })

lmuoUploads :: Lens' ListMultipartUploadsOutput [MultipartUpload]
lmuoUploads = lens _lmuoUploads (\s a -> s { _lmuoUploads = a })

instance AWSRequest ListMultipartUploads where
    type Sv ListMultipartUploads = S3
    type Rs ListMultipartUploads = ListMultipartUploadsOutput

    request  = get'
    response = const . xmlResponse $ \h x -> ListMultipartUploadsOutput
        <$> x %| "Bucket"
        <*> x %| "CommonPrefixes"
        <*> x %| "EncodingType"
        <*> x %| "IsTruncated"
        <*> x %| "KeyMarker"
        <*> x %| "MaxUploads"
        <*> x %| "NextKeyMarker"
        <*> x %| "NextUploadIdMarker"
        <*> x %| "Prefix"
        <*> x %| "UploadIdMarker"
        <*> x %| "Upload"
