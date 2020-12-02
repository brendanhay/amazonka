{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListObjectVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about all versions of the objects in a bucket. You can also use request parameters as selection criteria to return metadata about a subset of all the object versions.
--
--
-- To use this operation, you must have READ access to the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- The following operations are related to @ListObjectVersions@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html ListObjectsV2>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjectVersions
  ( -- * Creating a Request
    listObjectVersions,
    ListObjectVersions,

    -- * Request Lenses
    lKeyMarker,
    lPrefix,
    lEncodingType,
    lVersionIdMarker,
    lMaxKeys,
    lDelimiter,
    lExpectedBucketOwner,
    lBucket,

    -- * Destructuring the Response
    listObjectVersionsResponse,
    ListObjectVersionsResponse,

    -- * Response Lenses
    lrsNextVersionIdMarker,
    lrsKeyMarker,
    lrsDeleteMarkers,
    lrsPrefix,
    lrsCommonPrefixes,
    lrsEncodingType,
    lrsVersions,
    lrsName,
    lrsNextKeyMarker,
    lrsVersionIdMarker,
    lrsMaxKeys,
    lrsIsTruncated,
    lrsDelimiter,
    lrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'listObjectVersions' smart constructor.
data ListObjectVersions = ListObjectVersions'
  { _lKeyMarker ::
      !(Maybe Text),
    _lPrefix :: !(Maybe Text),
    _lEncodingType :: !(Maybe EncodingType),
    _lVersionIdMarker :: !(Maybe Text),
    _lMaxKeys :: !(Maybe Int),
    _lDelimiter :: !(Maybe Delimiter),
    _lExpectedBucketOwner :: !(Maybe Text),
    _lBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListObjectVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lKeyMarker' - Specifies the key to start with when listing objects in a bucket.
--
-- * 'lPrefix' - Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
--
-- * 'lEncodingType' - Undocumented member.
--
-- * 'lVersionIdMarker' - Specifies the object version you want to start listing from.
--
-- * 'lMaxKeys' - Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
--
-- * 'lDelimiter' - A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- * 'lExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'lBucket' - The bucket name that contains the objects.
listObjectVersions ::
  -- | 'lBucket'
  BucketName ->
  ListObjectVersions
listObjectVersions pBucket_ =
  ListObjectVersions'
    { _lKeyMarker = Nothing,
      _lPrefix = Nothing,
      _lEncodingType = Nothing,
      _lVersionIdMarker = Nothing,
      _lMaxKeys = Nothing,
      _lDelimiter = Nothing,
      _lExpectedBucketOwner = Nothing,
      _lBucket = pBucket_
    }

-- | Specifies the key to start with when listing objects in a bucket.
lKeyMarker :: Lens' ListObjectVersions (Maybe Text)
lKeyMarker = lens _lKeyMarker (\s a -> s {_lKeyMarker = a})

-- | Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
lPrefix :: Lens' ListObjectVersions (Maybe Text)
lPrefix = lens _lPrefix (\s a -> s {_lPrefix = a})

-- | Undocumented member.
lEncodingType :: Lens' ListObjectVersions (Maybe EncodingType)
lEncodingType = lens _lEncodingType (\s a -> s {_lEncodingType = a})

-- | Specifies the object version you want to start listing from.
lVersionIdMarker :: Lens' ListObjectVersions (Maybe Text)
lVersionIdMarker = lens _lVersionIdMarker (\s a -> s {_lVersionIdMarker = a})

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
lMaxKeys :: Lens' ListObjectVersions (Maybe Int)
lMaxKeys = lens _lMaxKeys (\s a -> s {_lMaxKeys = a})

-- | A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
lDelimiter :: Lens' ListObjectVersions (Maybe Delimiter)
lDelimiter = lens _lDelimiter (\s a -> s {_lDelimiter = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
lExpectedBucketOwner :: Lens' ListObjectVersions (Maybe Text)
lExpectedBucketOwner = lens _lExpectedBucketOwner (\s a -> s {_lExpectedBucketOwner = a})

-- | The bucket name that contains the objects.
lBucket :: Lens' ListObjectVersions BucketName
lBucket = lens _lBucket (\s a -> s {_lBucket = a})

instance AWSPager ListObjectVersions where
  page rq rs
    | stop (rs ^. lrsIsTruncated) = Nothing
    | isNothing (rs ^. lrsNextKeyMarker)
        && isNothing (rs ^. lrsNextVersionIdMarker) =
      Nothing
    | otherwise =
      Just $
        rq & lKeyMarker .~ rs ^. lrsNextKeyMarker
          & lVersionIdMarker .~ rs ^. lrsNextVersionIdMarker

instance AWSRequest ListObjectVersions where
  type Rs ListObjectVersions = ListObjectVersionsResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          ListObjectVersionsResponse'
            <$> (x .@? "NextVersionIdMarker")
            <*> (x .@? "KeyMarker")
            <*> (may (parseXMLList "DeleteMarker") x)
            <*> (x .@? "Prefix")
            <*> (may (parseXMLList "CommonPrefixes") x)
            <*> (x .@? "EncodingType")
            <*> (may (parseXMLList "Version") x)
            <*> (x .@? "Name")
            <*> (x .@? "NextKeyMarker")
            <*> (x .@? "VersionIdMarker")
            <*> (x .@? "MaxKeys")
            <*> (x .@? "IsTruncated")
            <*> (x .@? "Delimiter")
            <*> (pure (fromEnum s))
      )

instance Hashable ListObjectVersions

instance NFData ListObjectVersions

instance ToHeaders ListObjectVersions where
  toHeaders ListObjectVersions' {..} =
    mconcat ["x-amz-expected-bucket-owner" =# _lExpectedBucketOwner]

instance ToPath ListObjectVersions where
  toPath ListObjectVersions' {..} = mconcat ["/", toBS _lBucket]

instance ToQuery ListObjectVersions where
  toQuery ListObjectVersions' {..} =
    mconcat
      [ "key-marker" =: _lKeyMarker,
        "prefix" =: _lPrefix,
        "encoding-type" =: _lEncodingType,
        "version-id-marker" =: _lVersionIdMarker,
        "max-keys" =: _lMaxKeys,
        "delimiter" =: _lDelimiter,
        "versions"
      ]

-- | /See:/ 'listObjectVersionsResponse' smart constructor.
data ListObjectVersionsResponse = ListObjectVersionsResponse'
  { _lrsNextVersionIdMarker ::
      !(Maybe Text),
    _lrsKeyMarker :: !(Maybe Text),
    _lrsDeleteMarkers ::
      !(Maybe [DeleteMarkerEntry]),
    _lrsPrefix :: !(Maybe Text),
    _lrsCommonPrefixes ::
      !(Maybe [CommonPrefix]),
    _lrsEncodingType ::
      !(Maybe EncodingType),
    _lrsVersions ::
      !(Maybe [ObjectVersion]),
    _lrsName :: !(Maybe BucketName),
    _lrsNextKeyMarker :: !(Maybe Text),
    _lrsVersionIdMarker :: !(Maybe Text),
    _lrsMaxKeys :: !(Maybe Int),
    _lrsIsTruncated :: !(Maybe Bool),
    _lrsDelimiter :: !(Maybe Delimiter),
    _lrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListObjectVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextVersionIdMarker' - When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
--
-- * 'lrsKeyMarker' - Marks the last key returned in a truncated response.
--
-- * 'lrsDeleteMarkers' - Container for an object that is a delete marker.
--
-- * 'lrsPrefix' - Selects objects that start with the value supplied by this parameter.
--
-- * 'lrsCommonPrefixes' - All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
--
-- * 'lrsEncodingType' - Encoding type used by Amazon S3 to encode object key names in the XML response. If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements: @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
--
-- * 'lrsVersions' - Container for version information.
--
-- * 'lrsName' - The bucket name.
--
-- * 'lrsNextKeyMarker' - When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
--
-- * 'lrsVersionIdMarker' - Marks the last version of the key returned in a truncated response.
--
-- * 'lrsMaxKeys' - Specifies the maximum number of objects to return.
--
-- * 'lrsIsTruncated' - A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
--
-- * 'lrsDelimiter' - The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listObjectVersionsResponse ::
  -- | 'lrsResponseStatus'
  Int ->
  ListObjectVersionsResponse
listObjectVersionsResponse pResponseStatus_ =
  ListObjectVersionsResponse'
    { _lrsNextVersionIdMarker = Nothing,
      _lrsKeyMarker = Nothing,
      _lrsDeleteMarkers = Nothing,
      _lrsPrefix = Nothing,
      _lrsCommonPrefixes = Nothing,
      _lrsEncodingType = Nothing,
      _lrsVersions = Nothing,
      _lrsName = Nothing,
      _lrsNextKeyMarker = Nothing,
      _lrsVersionIdMarker = Nothing,
      _lrsMaxKeys = Nothing,
      _lrsIsTruncated = Nothing,
      _lrsDelimiter = Nothing,
      _lrsResponseStatus = pResponseStatus_
    }

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
lrsNextVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsNextVersionIdMarker = lens _lrsNextVersionIdMarker (\s a -> s {_lrsNextVersionIdMarker = a})

-- | Marks the last key returned in a truncated response.
lrsKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsKeyMarker = lens _lrsKeyMarker (\s a -> s {_lrsKeyMarker = a})

-- | Container for an object that is a delete marker.
lrsDeleteMarkers :: Lens' ListObjectVersionsResponse [DeleteMarkerEntry]
lrsDeleteMarkers = lens _lrsDeleteMarkers (\s a -> s {_lrsDeleteMarkers = a}) . _Default . _Coerce

-- | Selects objects that start with the value supplied by this parameter.
lrsPrefix :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsPrefix = lens _lrsPrefix (\s a -> s {_lrsPrefix = a})

-- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
lrsCommonPrefixes :: Lens' ListObjectVersionsResponse [CommonPrefix]
lrsCommonPrefixes = lens _lrsCommonPrefixes (\s a -> s {_lrsCommonPrefixes = a}) . _Default . _Coerce

-- | Encoding type used by Amazon S3 to encode object key names in the XML response. If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements: @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
lrsEncodingType :: Lens' ListObjectVersionsResponse (Maybe EncodingType)
lrsEncodingType = lens _lrsEncodingType (\s a -> s {_lrsEncodingType = a})

-- | Container for version information.
lrsVersions :: Lens' ListObjectVersionsResponse [ObjectVersion]
lrsVersions = lens _lrsVersions (\s a -> s {_lrsVersions = a}) . _Default . _Coerce

-- | The bucket name.
lrsName :: Lens' ListObjectVersionsResponse (Maybe BucketName)
lrsName = lens _lrsName (\s a -> s {_lrsName = a})

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
lrsNextKeyMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsNextKeyMarker = lens _lrsNextKeyMarker (\s a -> s {_lrsNextKeyMarker = a})

-- | Marks the last version of the key returned in a truncated response.
lrsVersionIdMarker :: Lens' ListObjectVersionsResponse (Maybe Text)
lrsVersionIdMarker = lens _lrsVersionIdMarker (\s a -> s {_lrsVersionIdMarker = a})

-- | Specifies the maximum number of objects to return.
lrsMaxKeys :: Lens' ListObjectVersionsResponse (Maybe Int)
lrsMaxKeys = lens _lrsMaxKeys (\s a -> s {_lrsMaxKeys = a})

-- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
lrsIsTruncated :: Lens' ListObjectVersionsResponse (Maybe Bool)
lrsIsTruncated = lens _lrsIsTruncated (\s a -> s {_lrsIsTruncated = a})

-- | The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
lrsDelimiter :: Lens' ListObjectVersionsResponse (Maybe Delimiter)
lrsDelimiter = lens _lrsDelimiter (\s a -> s {_lrsDelimiter = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListObjectVersionsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\s a -> s {_lrsResponseStatus = a})

instance NFData ListObjectVersionsResponse
