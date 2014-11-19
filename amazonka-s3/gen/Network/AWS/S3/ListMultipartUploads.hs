{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListMultipartUploads.html>
module Network.AWS.S3.ListMultipartUploads
    (
    -- * Request
      ListMultipartUploads
    -- ** Request constructor
    , listMultipartUploads
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
    , listMultipartUploadsResponse
    -- ** Response lenses
    , lmurBucket
    , lmurCommonPrefixes
    , lmurDelimiter
    , lmurEncodingType
    , lmurIsTruncated
    , lmurKeyMarker
    , lmurMaxUploads
    , lmurNextKeyMarker
    , lmurNextUploadIdMarker
    , lmurPrefix
    , lmurUploadIdMarker
    , lmurUploads
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data ListMultipartUploads = ListMultipartUploads
    { _lmuBucket         :: Text
    , _lmuDelimiter      :: Maybe Text
    , _lmuEncodingType   :: Maybe Text
    , _lmuKeyMarker      :: Maybe Text
    , _lmuMaxUploads     :: Maybe Int
    , _lmuPrefix         :: Maybe Text
    , _lmuUploadIdMarker :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListMultipartUploads' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmuBucket' @::@ 'Text'
--
-- * 'lmuDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'lmuEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lmuKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmuMaxUploads' @::@ 'Maybe' 'Int'
--
-- * 'lmuPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lmuUploadIdMarker' @::@ 'Maybe' 'Text'
--
listMultipartUploads :: Text -- ^ 'lmuBucket'
                     -> ListMultipartUploads
listMultipartUploads p1 = ListMultipartUploads
    { _lmuBucket         = p1
    , _lmuDelimiter      = Nothing
    , _lmuEncodingType   = Nothing
    , _lmuKeyMarker      = Nothing
    , _lmuMaxUploads     = Nothing
    , _lmuPrefix         = Nothing
    , _lmuUploadIdMarker = Nothing
    }

lmuBucket :: Lens' ListMultipartUploads Text
lmuBucket = lens _lmuBucket (\s a -> s { _lmuBucket = a })

-- | Character you use to group keys.
lmuDelimiter :: Lens' ListMultipartUploads (Maybe Text)
lmuDelimiter = lens _lmuDelimiter (\s a -> s { _lmuDelimiter = a })

lmuEncodingType :: Lens' ListMultipartUploads (Maybe Text)
lmuEncodingType = lens _lmuEncodingType (\s a -> s { _lmuEncodingType = a })

-- | Together with upload-id-marker, this parameter specifies the multipart
-- upload after which listing should begin.
lmuKeyMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuKeyMarker = lens _lmuKeyMarker (\s a -> s { _lmuKeyMarker = a })

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return
-- in the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
lmuMaxUploads :: Lens' ListMultipartUploads (Maybe Int)
lmuMaxUploads = lens _lmuMaxUploads (\s a -> s { _lmuMaxUploads = a })

-- | Lists in-progress uploads only for those keys that begin with the
-- specified prefix.
lmuPrefix :: Lens' ListMultipartUploads (Maybe Text)
lmuPrefix = lens _lmuPrefix (\s a -> s { _lmuPrefix = a })

-- | Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the
-- upload-id-marker parameter is ignored.
lmuUploadIdMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuUploadIdMarker =
    lens _lmuUploadIdMarker (\s a -> s { _lmuUploadIdMarker = a })

data ListMultipartUploadsResponse = ListMultipartUploadsResponse
    { _lmurBucket             :: Maybe Text
    , _lmurCommonPrefixes     :: Flatten [CommonPrefix]
    , _lmurDelimiter          :: Maybe Text
    , _lmurEncodingType       :: Maybe Text
    , _lmurIsTruncated        :: Maybe Bool
    , _lmurKeyMarker          :: Maybe Text
    , _lmurMaxUploads         :: Maybe Int
    , _lmurNextKeyMarker      :: Maybe Text
    , _lmurNextUploadIdMarker :: Maybe Text
    , _lmurPrefix             :: Maybe Text
    , _lmurUploadIdMarker     :: Maybe Text
    , _lmurUploads            :: Flatten [MultipartUpload]
    } deriving (Eq, Show, Generic)

-- | 'ListMultipartUploadsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmurBucket' @::@ 'Maybe' 'Text'
--
-- * 'lmurCommonPrefixes' @::@ ['CommonPrefix']
--
-- * 'lmurDelimiter' @::@ 'Maybe' 'Text'
--
-- * 'lmurEncodingType' @::@ 'Maybe' 'Text'
--
-- * 'lmurIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'lmurKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmurMaxUploads' @::@ 'Maybe' 'Int'
--
-- * 'lmurNextKeyMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmurNextUploadIdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmurPrefix' @::@ 'Maybe' 'Text'
--
-- * 'lmurUploadIdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lmurUploads' @::@ ['MultipartUpload']
--
listMultipartUploadsResponse :: [MultipartUpload] -- ^ 'lmurUploads'
                             -> [CommonPrefix] -- ^ 'lmurCommonPrefixes'
                             -> ListMultipartUploadsResponse
listMultipartUploadsResponse p1 p2 = ListMultipartUploadsResponse
    { _lmurUploads            = withIso _Flatten (const id) p1
    , _lmurCommonPrefixes     = withIso _Flatten (const id) p2
    , _lmurBucket             = Nothing
    , _lmurKeyMarker          = Nothing
    , _lmurUploadIdMarker     = Nothing
    , _lmurNextKeyMarker      = Nothing
    , _lmurPrefix             = Nothing
    , _lmurDelimiter          = Nothing
    , _lmurNextUploadIdMarker = Nothing
    , _lmurMaxUploads         = Nothing
    , _lmurIsTruncated        = Nothing
    , _lmurEncodingType       = Nothing
    }

-- | Name of the bucket to which the multipart upload was initiated.
lmurBucket :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurBucket = lens _lmurBucket (\s a -> s { _lmurBucket = a })

lmurCommonPrefixes :: Lens' ListMultipartUploadsResponse [CommonPrefix]
lmurCommonPrefixes =
    lens _lmurCommonPrefixes (\s a -> s { _lmurCommonPrefixes = a })
        . _Flatten

lmurDelimiter :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurDelimiter = lens _lmurDelimiter (\s a -> s { _lmurDelimiter = a })

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lmurEncodingType :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurEncodingType = lens _lmurEncodingType (\s a -> s { _lmurEncodingType = a })

-- | Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed or
-- specified by max uploads.
lmurIsTruncated :: Lens' ListMultipartUploadsResponse (Maybe Bool)
lmurIsTruncated = lens _lmurIsTruncated (\s a -> s { _lmurIsTruncated = a })

-- | The key at or after which the listing began.
lmurKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurKeyMarker = lens _lmurKeyMarker (\s a -> s { _lmurKeyMarker = a })

-- | Maximum number of multipart uploads that could have been included in the
-- response.
lmurMaxUploads :: Lens' ListMultipartUploadsResponse (Maybe Int)
lmurMaxUploads = lens _lmurMaxUploads (\s a -> s { _lmurMaxUploads = a })

-- | When a list is truncated, this element specifies the value that should be
-- used for the key-marker request parameter in a subsequent request.
lmurNextKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurNextKeyMarker =
    lens _lmurNextKeyMarker (\s a -> s { _lmurNextKeyMarker = a })

-- | When a list is truncated, this element specifies the value that should be
-- used for the upload-id-marker request parameter in a subsequent request.
lmurNextUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurNextUploadIdMarker =
    lens _lmurNextUploadIdMarker (\s a -> s { _lmurNextUploadIdMarker = a })

-- | When a prefix is provided in the request, this field contains the
-- specified prefix. The result contains only keys starting with the
-- specified prefix.
lmurPrefix :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurPrefix = lens _lmurPrefix (\s a -> s { _lmurPrefix = a })

-- | Upload ID after which listing began.
lmurUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurUploadIdMarker =
    lens _lmurUploadIdMarker (\s a -> s { _lmurUploadIdMarker = a })

lmurUploads :: Lens' ListMultipartUploadsResponse [MultipartUpload]
lmurUploads = lens _lmurUploads (\s a -> s { _lmurUploads = a })
    . _Flatten

instance ToPath ListMultipartUploads where
    toPath ListMultipartUploads{..} = mconcat
        [ "/"
        , toText _lmuBucket
        ]

instance ToQuery ListMultipartUploads where
    toQuery ListMultipartUploads{..} = mconcat
        [ "uploads"
        , "delimiter"        =? _lmuDelimiter
        , "encoding-type"    =? _lmuEncodingType
        , "key-marker"       =? _lmuKeyMarker
        , "max-uploads"      =? _lmuMaxUploads
        , "prefix"           =? _lmuPrefix
        , "upload-id-marker" =? _lmuUploadIdMarker
        ]

instance ToHeaders ListMultipartUploads

instance ToXMLRoot ListMultipartUploads where
    toXMLRoot = const (element "ListMultipartUploads" [])

instance ToXML ListMultipartUploads

instance AWSRequest ListMultipartUploads where
    type Sv ListMultipartUploads = S3
    type Rs ListMultipartUploads = ListMultipartUploadsResponse

    request  = get
    response = xmlResponse

instance FromXML ListMultipartUploadsResponse where
    parseXML x = ListMultipartUploadsResponse
        <$> x .@? "Bucket"
        <*> x .@ "CommonPrefixes"
        <*> x .@? "Delimiter"
        <*> x .@? "EncodingType"
        <*> x .@? "IsTruncated"
        <*> x .@? "KeyMarker"
        <*> x .@? "MaxUploads"
        <*> x .@? "NextKeyMarker"
        <*> x .@? "NextUploadIdMarker"
        <*> x .@? "Prefix"
        <*> x .@? "UploadIdMarker"
        <*> x .@ "Upload"

instance AWSPager ListMultipartUploads where
    next rq rs
        | not (more (rs ^. lmurIsTruncated)) = Nothing
        | isNothing p1 && isNothing p2 = Nothing
        | otherwise = Just $ rq
            & lmuKeyMarker .~ p1
            & lmuUploadIdMarker .~ p2
      where
        p1 = rs ^. lmurNextKeyMarker
        p2 = rs ^. lmurNextUploadIdMarker
