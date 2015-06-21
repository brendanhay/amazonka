{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.ListMultipartUploads
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , lmuKeyMarker
    , lmuPrefix
    , lmuEncodingType
    , lmuMaxUploads
    , lmuUploadIdMarker
    , lmuDelimiter
    , lmuBucket

    -- * Response
    , ListMultipartUploadsResponse
    -- ** Response constructor
    , listMultipartUploadsResponse
    -- ** Response lenses
    , lmurKeyMarker
    , lmurPrefix
    , lmurEncodingType
    , lmurCommonPrefixes
    , lmurBucket
    , lmurMaxUploads
    , lmurUploadIdMarker
    , lmurNextKeyMarker
    , lmurUploads
    , lmurIsTruncated
    , lmurNextUploadIdMarker
    , lmurDelimiter
    ) where

import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'listMultipartUploads' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmuKeyMarker'
--
-- * 'lmuPrefix'
--
-- * 'lmuEncodingType'
--
-- * 'lmuMaxUploads'
--
-- * 'lmuUploadIdMarker'
--
-- * 'lmuDelimiter'
--
-- * 'lmuBucket'
data ListMultipartUploads = ListMultipartUploads'{_lmuKeyMarker :: Maybe Text, _lmuPrefix :: Maybe Text, _lmuEncodingType :: Maybe EncodingType, _lmuMaxUploads :: Maybe Int, _lmuUploadIdMarker :: Maybe Text, _lmuDelimiter :: Maybe Char, _lmuBucket :: BucketName} deriving (Eq, Read, Show)

-- | 'ListMultipartUploads' smart constructor.
listMultipartUploads :: BucketName -> ListMultipartUploads
listMultipartUploads pBucket = ListMultipartUploads'{_lmuKeyMarker = Nothing, _lmuPrefix = Nothing, _lmuEncodingType = Nothing, _lmuMaxUploads = Nothing, _lmuUploadIdMarker = Nothing, _lmuDelimiter = Nothing, _lmuBucket = pBucket};

-- | Together with upload-id-marker, this parameter specifies the multipart
-- upload after which listing should begin.
lmuKeyMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuKeyMarker = lens _lmuKeyMarker (\ s a -> s{_lmuKeyMarker = a});

-- | Lists in-progress uploads only for those keys that begin with the
-- specified prefix.
lmuPrefix :: Lens' ListMultipartUploads (Maybe Text)
lmuPrefix = lens _lmuPrefix (\ s a -> s{_lmuPrefix = a});

-- | FIXME: Undocumented member.
lmuEncodingType :: Lens' ListMultipartUploads (Maybe EncodingType)
lmuEncodingType = lens _lmuEncodingType (\ s a -> s{_lmuEncodingType = a});

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return
-- in the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
lmuMaxUploads :: Lens' ListMultipartUploads (Maybe Int)
lmuMaxUploads = lens _lmuMaxUploads (\ s a -> s{_lmuMaxUploads = a});

-- | Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the
-- upload-id-marker parameter is ignored.
lmuUploadIdMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuUploadIdMarker = lens _lmuUploadIdMarker (\ s a -> s{_lmuUploadIdMarker = a});

-- | Character you use to group keys.
lmuDelimiter :: Lens' ListMultipartUploads (Maybe Char)
lmuDelimiter = lens _lmuDelimiter (\ s a -> s{_lmuDelimiter = a});

-- | FIXME: Undocumented member.
lmuBucket :: Lens' ListMultipartUploads BucketName
lmuBucket = lens _lmuBucket (\ s a -> s{_lmuBucket = a});

instance AWSPager ListMultipartUploads where
        page rq rs
          | stop (rs ^. lmurIsTruncated) = Nothing
          | isNothing (rs ^. lmurNextKeyMarker) &&
              isNothing (rs ^. lmurNextUploadIdMarker)
            = Nothing
          | otherwise =
            Just $ rq & lmuKeyMarker .~ rs ^. lmurNextKeyMarker &
              lmuUploadIdMarker .~ rs ^. lmurNextUploadIdMarker

instance AWSRequest ListMultipartUploads where
        type Sv ListMultipartUploads = S3
        type Rs ListMultipartUploads =
             ListMultipartUploadsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListMultipartUploadsResponse' <$>
                   (x .@? "KeyMarker") <*> (x .@? "Prefix") <*>
                     (x .@? "EncodingType")
                     <*> (may (parseXMLList "CommonPrefixes") x)
                     <*> (x .@? "Bucket")
                     <*> (x .@? "MaxUploads")
                     <*> (x .@? "UploadIdMarker")
                     <*> (x .@? "NextKeyMarker")
                     <*> (may (parseXMLList "Upload") x)
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "NextUploadIdMarker")
                     <*> (x .@? "Delimiter"))

instance ToHeaders ListMultipartUploads where
        toHeaders = const mempty

instance ToPath ListMultipartUploads where
        toPath ListMultipartUploads'{..}
          = mconcat ["/", toText _lmuBucket]

instance ToQuery ListMultipartUploads where
        toQuery ListMultipartUploads'{..}
          = mconcat
              ["key-marker" =: _lmuKeyMarker,
               "prefix" =: _lmuPrefix,
               "encoding-type" =: _lmuEncodingType,
               "max-uploads" =: _lmuMaxUploads,
               "upload-id-marker" =: _lmuUploadIdMarker,
               "delimiter" =: _lmuDelimiter, "uploads"]

-- | /See:/ 'listMultipartUploadsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmurKeyMarker'
--
-- * 'lmurPrefix'
--
-- * 'lmurEncodingType'
--
-- * 'lmurCommonPrefixes'
--
-- * 'lmurBucket'
--
-- * 'lmurMaxUploads'
--
-- * 'lmurUploadIdMarker'
--
-- * 'lmurNextKeyMarker'
--
-- * 'lmurUploads'
--
-- * 'lmurIsTruncated'
--
-- * 'lmurNextUploadIdMarker'
--
-- * 'lmurDelimiter'
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'{_lmurKeyMarker :: Maybe Text, _lmurPrefix :: Maybe Text, _lmurEncodingType :: Maybe EncodingType, _lmurCommonPrefixes :: Maybe [CommonPrefix], _lmurBucket :: Maybe BucketName, _lmurMaxUploads :: Maybe Int, _lmurUploadIdMarker :: Maybe Text, _lmurNextKeyMarker :: Maybe Text, _lmurUploads :: Maybe [MultipartUpload], _lmurIsTruncated :: Maybe Bool, _lmurNextUploadIdMarker :: Maybe Text, _lmurDelimiter :: Maybe Char} deriving (Eq, Read, Show)

-- | 'ListMultipartUploadsResponse' smart constructor.
listMultipartUploadsResponse :: ListMultipartUploadsResponse
listMultipartUploadsResponse = ListMultipartUploadsResponse'{_lmurKeyMarker = Nothing, _lmurPrefix = Nothing, _lmurEncodingType = Nothing, _lmurCommonPrefixes = Nothing, _lmurBucket = Nothing, _lmurMaxUploads = Nothing, _lmurUploadIdMarker = Nothing, _lmurNextKeyMarker = Nothing, _lmurUploads = Nothing, _lmurIsTruncated = Nothing, _lmurNextUploadIdMarker = Nothing, _lmurDelimiter = Nothing};

-- | The key at or after which the listing began.
lmurKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurKeyMarker = lens _lmurKeyMarker (\ s a -> s{_lmurKeyMarker = a});

-- | When a prefix is provided in the request, this field contains the
-- specified prefix. The result contains only keys starting with the
-- specified prefix.
lmurPrefix :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurPrefix = lens _lmurPrefix (\ s a -> s{_lmurPrefix = a});

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lmurEncodingType :: Lens' ListMultipartUploadsResponse (Maybe EncodingType)
lmurEncodingType = lens _lmurEncodingType (\ s a -> s{_lmurEncodingType = a});

-- | FIXME: Undocumented member.
lmurCommonPrefixes :: Lens' ListMultipartUploadsResponse [CommonPrefix]
lmurCommonPrefixes = lens _lmurCommonPrefixes (\ s a -> s{_lmurCommonPrefixes = a}) . _Default;

-- | Name of the bucket to which the multipart upload was initiated.
lmurBucket :: Lens' ListMultipartUploadsResponse (Maybe BucketName)
lmurBucket = lens _lmurBucket (\ s a -> s{_lmurBucket = a});

-- | Maximum number of multipart uploads that could have been included in the
-- response.
lmurMaxUploads :: Lens' ListMultipartUploadsResponse (Maybe Int)
lmurMaxUploads = lens _lmurMaxUploads (\ s a -> s{_lmurMaxUploads = a});

-- | Upload ID after which listing began.
lmurUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurUploadIdMarker = lens _lmurUploadIdMarker (\ s a -> s{_lmurUploadIdMarker = a});

-- | When a list is truncated, this element specifies the value that should
-- be used for the key-marker request parameter in a subsequent request.
lmurNextKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurNextKeyMarker = lens _lmurNextKeyMarker (\ s a -> s{_lmurNextKeyMarker = a});

-- | FIXME: Undocumented member.
lmurUploads :: Lens' ListMultipartUploadsResponse [MultipartUpload]
lmurUploads = lens _lmurUploads (\ s a -> s{_lmurUploads = a}) . _Default;

-- | Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed
-- or specified by max uploads.
lmurIsTruncated :: Lens' ListMultipartUploadsResponse (Maybe Bool)
lmurIsTruncated = lens _lmurIsTruncated (\ s a -> s{_lmurIsTruncated = a});

-- | When a list is truncated, this element specifies the value that should
-- be used for the upload-id-marker request parameter in a subsequent
-- request.
lmurNextUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmurNextUploadIdMarker = lens _lmurNextUploadIdMarker (\ s a -> s{_lmurNextUploadIdMarker = a});

-- | FIXME: Undocumented member.
lmurDelimiter :: Lens' ListMultipartUploadsResponse (Maybe Char)
lmurDelimiter = lens _lmurDelimiter (\ s a -> s{_lmurDelimiter = a});
