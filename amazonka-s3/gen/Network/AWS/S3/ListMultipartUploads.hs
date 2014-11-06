{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

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
    { _lmurBucket         :: BucketName
    , _lmurDelimiter      :: Maybe Char
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
-- * 'lmurBucket' @::@ 'BucketName'
--
-- * 'lmurDelimiter' @::@ 'Maybe' 'Char'
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
listMultipartUploads :: BucketName -- ^ 'lmurBucket'
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

lmurBucket :: Lens' ListMultipartUploads BucketName
lmurBucket = lens _lmurBucket (\s a -> s { _lmurBucket = a })

-- | Character you use to group keys.
lmurDelimiter :: Lens' ListMultipartUploads (Maybe Char)
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
    { _lmuoBucket             :: Maybe BucketName
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
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest ListMultipartUploads where
    type Sv ListMultipartUploads = S3
    type Rs ListMultipartUploads = ListMultipartUploadsOutput

    request  = get
    response = const . xmlResponse $ \h x ->
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
