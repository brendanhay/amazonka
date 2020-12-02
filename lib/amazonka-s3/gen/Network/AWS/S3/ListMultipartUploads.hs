{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListMultipartUploads
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists in-progress multipart uploads.
--
-- This operation returns paginated results.
module Network.AWS.S3.ListMultipartUploads
    (
    -- * Creating a Request
      listMultipartUploads
    , ListMultipartUploads
    -- * Request Lenses
    , lmuKeyMarker
    , lmuPrefix
    , lmuEncodingType
    , lmuUploadIdMarker
    , lmuMaxUploads
    , lmuDelimiter
    , lmuBucket

    -- * Destructuring the Response
    , listMultipartUploadsResponse
    , ListMultipartUploadsResponse
    -- * Response Lenses
    , lmursKeyMarker
    , lmursPrefix
    , lmursCommonPrefixes
    , lmursEncodingType
    , lmursBucket
    , lmursUploadIdMarker
    , lmursMaxUploads
    , lmursNextKeyMarker
    , lmursUploads
    , lmursIsTruncated
    , lmursNextUploadIdMarker
    , lmursDelimiter
    , lmursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'listMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { _lmuKeyMarker      :: !(Maybe Text)
  , _lmuPrefix         :: !(Maybe Text)
  , _lmuEncodingType   :: !(Maybe EncodingType)
  , _lmuUploadIdMarker :: !(Maybe Text)
  , _lmuMaxUploads     :: !(Maybe Int)
  , _lmuDelimiter      :: !(Maybe Delimiter)
  , _lmuBucket         :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMultipartUploads' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmuKeyMarker' - Together with upload-id-marker, this parameter specifies the multipart upload after which listing should begin.
--
-- * 'lmuPrefix' - Lists in-progress uploads only for those keys that begin with the specified prefix.
--
-- * 'lmuEncodingType' - Undocumented member.
--
-- * 'lmuUploadIdMarker' - Together with key-marker, specifies the multipart upload after which listing should begin. If key-marker is not specified, the upload-id-marker parameter is ignored.
--
-- * 'lmuMaxUploads' - Sets the maximum number of multipart uploads, from 1 to 1,000, to return in the response body. 1,000 is the maximum number of uploads that can be returned in a response.
--
-- * 'lmuDelimiter' - Character you use to group keys.
--
-- * 'lmuBucket' - Undocumented member.
listMultipartUploads
    :: BucketName -- ^ 'lmuBucket'
    -> ListMultipartUploads
listMultipartUploads pBucket_ =
  ListMultipartUploads'
    { _lmuKeyMarker = Nothing
    , _lmuPrefix = Nothing
    , _lmuEncodingType = Nothing
    , _lmuUploadIdMarker = Nothing
    , _lmuMaxUploads = Nothing
    , _lmuDelimiter = Nothing
    , _lmuBucket = pBucket_
    }


-- | Together with upload-id-marker, this parameter specifies the multipart upload after which listing should begin.
lmuKeyMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuKeyMarker = lens _lmuKeyMarker (\ s a -> s{_lmuKeyMarker = a})

-- | Lists in-progress uploads only for those keys that begin with the specified prefix.
lmuPrefix :: Lens' ListMultipartUploads (Maybe Text)
lmuPrefix = lens _lmuPrefix (\ s a -> s{_lmuPrefix = a})

-- | Undocumented member.
lmuEncodingType :: Lens' ListMultipartUploads (Maybe EncodingType)
lmuEncodingType = lens _lmuEncodingType (\ s a -> s{_lmuEncodingType = a})

-- | Together with key-marker, specifies the multipart upload after which listing should begin. If key-marker is not specified, the upload-id-marker parameter is ignored.
lmuUploadIdMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuUploadIdMarker = lens _lmuUploadIdMarker (\ s a -> s{_lmuUploadIdMarker = a})

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return in the response body. 1,000 is the maximum number of uploads that can be returned in a response.
lmuMaxUploads :: Lens' ListMultipartUploads (Maybe Int)
lmuMaxUploads = lens _lmuMaxUploads (\ s a -> s{_lmuMaxUploads = a})

-- | Character you use to group keys.
lmuDelimiter :: Lens' ListMultipartUploads (Maybe Delimiter)
lmuDelimiter = lens _lmuDelimiter (\ s a -> s{_lmuDelimiter = a})

-- | Undocumented member.
lmuBucket :: Lens' ListMultipartUploads BucketName
lmuBucket = lens _lmuBucket (\ s a -> s{_lmuBucket = a})

instance AWSPager ListMultipartUploads where
        page rq rs
          | stop (rs ^. lmursIsTruncated) = Nothing
          | isNothing (rs ^. lmursNextKeyMarker) &&
              isNothing (rs ^. lmursNextUploadIdMarker)
            = Nothing
          | otherwise =
            Just $ rq & lmuKeyMarker .~ rs ^. lmursNextKeyMarker
              & lmuUploadIdMarker .~ rs ^. lmursNextUploadIdMarker

instance AWSRequest ListMultipartUploads where
        type Rs ListMultipartUploads =
             ListMultipartUploadsResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 ListMultipartUploadsResponse' <$>
                   (x .@? "KeyMarker") <*> (x .@? "Prefix") <*>
                     (may (parseXMLList "CommonPrefixes") x)
                     <*> (x .@? "EncodingType")
                     <*> (x .@? "Bucket")
                     <*> (x .@? "UploadIdMarker")
                     <*> (x .@? "MaxUploads")
                     <*> (x .@? "NextKeyMarker")
                     <*> (may (parseXMLList "Upload") x)
                     <*> (x .@? "IsTruncated")
                     <*> (x .@? "NextUploadIdMarker")
                     <*> (x .@? "Delimiter")
                     <*> (pure (fromEnum s)))

instance Hashable ListMultipartUploads where

instance NFData ListMultipartUploads where

instance ToHeaders ListMultipartUploads where
        toHeaders = const mempty

instance ToPath ListMultipartUploads where
        toPath ListMultipartUploads'{..}
          = mconcat ["/", toBS _lmuBucket]

instance ToQuery ListMultipartUploads where
        toQuery ListMultipartUploads'{..}
          = mconcat
              ["key-marker" =: _lmuKeyMarker,
               "prefix" =: _lmuPrefix,
               "encoding-type" =: _lmuEncodingType,
               "upload-id-marker" =: _lmuUploadIdMarker,
               "max-uploads" =: _lmuMaxUploads,
               "delimiter" =: _lmuDelimiter, "uploads"]

-- | /See:/ 'listMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { _lmursKeyMarker          :: !(Maybe Text)
  , _lmursPrefix             :: !(Maybe Text)
  , _lmursCommonPrefixes     :: !(Maybe [CommonPrefix])
  , _lmursEncodingType       :: !(Maybe EncodingType)
  , _lmursBucket             :: !(Maybe BucketName)
  , _lmursUploadIdMarker     :: !(Maybe Text)
  , _lmursMaxUploads         :: !(Maybe Int)
  , _lmursNextKeyMarker      :: !(Maybe Text)
  , _lmursUploads            :: !(Maybe [MultipartUpload])
  , _lmursIsTruncated        :: !(Maybe Bool)
  , _lmursNextUploadIdMarker :: !(Maybe Text)
  , _lmursDelimiter          :: !(Maybe Delimiter)
  , _lmursResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMultipartUploadsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmursKeyMarker' - The key at or after which the listing began.
--
-- * 'lmursPrefix' - When a prefix is provided in the request, this field contains the specified prefix. The result contains only keys starting with the specified prefix.
--
-- * 'lmursCommonPrefixes' - Undocumented member.
--
-- * 'lmursEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lmursBucket' - Name of the bucket to which the multipart upload was initiated.
--
-- * 'lmursUploadIdMarker' - Upload ID after which listing began.
--
-- * 'lmursMaxUploads' - Maximum number of multipart uploads that could have been included in the response.
--
-- * 'lmursNextKeyMarker' - When a list is truncated, this element specifies the value that should be used for the key-marker request parameter in a subsequent request.
--
-- * 'lmursUploads' - Undocumented member.
--
-- * 'lmursIsTruncated' - Indicates whether the returned list of multipart uploads is truncated. A value of true indicates that the list was truncated. The list can be truncated if the number of multipart uploads exceeds the limit allowed or specified by max uploads.
--
-- * 'lmursNextUploadIdMarker' - When a list is truncated, this element specifies the value that should be used for the upload-id-marker request parameter in a subsequent request.
--
-- * 'lmursDelimiter' - Undocumented member.
--
-- * 'lmursResponseStatus' - -- | The response status code.
listMultipartUploadsResponse
    :: Int -- ^ 'lmursResponseStatus'
    -> ListMultipartUploadsResponse
listMultipartUploadsResponse pResponseStatus_ =
  ListMultipartUploadsResponse'
    { _lmursKeyMarker = Nothing
    , _lmursPrefix = Nothing
    , _lmursCommonPrefixes = Nothing
    , _lmursEncodingType = Nothing
    , _lmursBucket = Nothing
    , _lmursUploadIdMarker = Nothing
    , _lmursMaxUploads = Nothing
    , _lmursNextKeyMarker = Nothing
    , _lmursUploads = Nothing
    , _lmursIsTruncated = Nothing
    , _lmursNextUploadIdMarker = Nothing
    , _lmursDelimiter = Nothing
    , _lmursResponseStatus = pResponseStatus_
    }


-- | The key at or after which the listing began.
lmursKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursKeyMarker = lens _lmursKeyMarker (\ s a -> s{_lmursKeyMarker = a})

-- | When a prefix is provided in the request, this field contains the specified prefix. The result contains only keys starting with the specified prefix.
lmursPrefix :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursPrefix = lens _lmursPrefix (\ s a -> s{_lmursPrefix = a})

-- | Undocumented member.
lmursCommonPrefixes :: Lens' ListMultipartUploadsResponse [CommonPrefix]
lmursCommonPrefixes = lens _lmursCommonPrefixes (\ s a -> s{_lmursCommonPrefixes = a}) . _Default . _Coerce

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lmursEncodingType :: Lens' ListMultipartUploadsResponse (Maybe EncodingType)
lmursEncodingType = lens _lmursEncodingType (\ s a -> s{_lmursEncodingType = a})

-- | Name of the bucket to which the multipart upload was initiated.
lmursBucket :: Lens' ListMultipartUploadsResponse (Maybe BucketName)
lmursBucket = lens _lmursBucket (\ s a -> s{_lmursBucket = a})

-- | Upload ID after which listing began.
lmursUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursUploadIdMarker = lens _lmursUploadIdMarker (\ s a -> s{_lmursUploadIdMarker = a})

-- | Maximum number of multipart uploads that could have been included in the response.
lmursMaxUploads :: Lens' ListMultipartUploadsResponse (Maybe Int)
lmursMaxUploads = lens _lmursMaxUploads (\ s a -> s{_lmursMaxUploads = a})

-- | When a list is truncated, this element specifies the value that should be used for the key-marker request parameter in a subsequent request.
lmursNextKeyMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursNextKeyMarker = lens _lmursNextKeyMarker (\ s a -> s{_lmursNextKeyMarker = a})

-- | Undocumented member.
lmursUploads :: Lens' ListMultipartUploadsResponse [MultipartUpload]
lmursUploads = lens _lmursUploads (\ s a -> s{_lmursUploads = a}) . _Default . _Coerce

-- | Indicates whether the returned list of multipart uploads is truncated. A value of true indicates that the list was truncated. The list can be truncated if the number of multipart uploads exceeds the limit allowed or specified by max uploads.
lmursIsTruncated :: Lens' ListMultipartUploadsResponse (Maybe Bool)
lmursIsTruncated = lens _lmursIsTruncated (\ s a -> s{_lmursIsTruncated = a})

-- | When a list is truncated, this element specifies the value that should be used for the upload-id-marker request parameter in a subsequent request.
lmursNextUploadIdMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursNextUploadIdMarker = lens _lmursNextUploadIdMarker (\ s a -> s{_lmursNextUploadIdMarker = a})

-- | Undocumented member.
lmursDelimiter :: Lens' ListMultipartUploadsResponse (Maybe Delimiter)
lmursDelimiter = lens _lmursDelimiter (\ s a -> s{_lmursDelimiter = a})

-- | -- | The response status code.
lmursResponseStatus :: Lens' ListMultipartUploadsResponse Int
lmursResponseStatus = lens _lmursResponseStatus (\ s a -> s{_lmursResponseStatus = a})

instance NFData ListMultipartUploadsResponse where
