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
-- Module      : Network.AWS.S3.ListObjectsV2
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1,000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. A @200 OK@ response can contain valid or invalid XML. Make sure to design your application to parse the contents of the response and handle it appropriately.
--
--
-- To use this operation, you must have READ access to the bucket.
--
-- To use this operation in an AWS Identity and Access Management (IAM) policy, you must have permissions to perform the @s3:ListBucket@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- /Important:/ This section describes the latest revision of the API. We recommend that you use this revised API for application development. For backward compatibility, Amazon S3 continues to support the prior version of this API, <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects> .
--
-- To get a list of your buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets> .
--
-- The following operations are related to @ListObjectsV2@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjectsV2
  ( -- * Creating a Request
    listObjectsV2,
    ListObjectsV2,

    -- * Request Lenses
    lovStartAfter,
    lovContinuationToken,
    lovFetchOwner,
    lovPrefix,
    lovEncodingType,
    lovRequestPayer,
    lovMaxKeys,
    lovDelimiter,
    lovExpectedBucketOwner,
    lovBucket,

    -- * Destructuring the Response
    listObjectsV2Response,
    ListObjectsV2Response,

    -- * Response Lenses
    lovrsStartAfter,
    lovrsKeyCount,
    lovrsContents,
    lovrsContinuationToken,
    lovrsPrefix,
    lovrsCommonPrefixes,
    lovrsEncodingType,
    lovrsName,
    lovrsNextContinuationToken,
    lovrsMaxKeys,
    lovrsIsTruncated,
    lovrsDelimiter,
    lovrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'listObjectsV2' smart constructor.
data ListObjectsV2 = ListObjectsV2'
  { _lovStartAfter ::
      !(Maybe Text),
    _lovContinuationToken :: !(Maybe Text),
    _lovFetchOwner :: !(Maybe Bool),
    _lovPrefix :: !(Maybe Text),
    _lovEncodingType :: !(Maybe EncodingType),
    _lovRequestPayer :: !(Maybe RequestPayer),
    _lovMaxKeys :: !(Maybe Int),
    _lovDelimiter :: !(Maybe Delimiter),
    _lovExpectedBucketOwner :: !(Maybe Text),
    _lovBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListObjectsV2' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lovStartAfter' - StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket.
--
-- * 'lovContinuationToken' - ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key.
--
-- * 'lovFetchOwner' - The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true.
--
-- * 'lovPrefix' - Limits the response to keys that begin with the specified prefix.
--
-- * 'lovEncodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- * 'lovRequestPayer' - Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
--
-- * 'lovMaxKeys' - Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
--
-- * 'lovDelimiter' - A delimiter is a character you use to group keys.
--
-- * 'lovExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'lovBucket' - Bucket name to list.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
listObjectsV2 ::
  -- | 'lovBucket'
  BucketName ->
  ListObjectsV2
listObjectsV2 pBucket_ =
  ListObjectsV2'
    { _lovStartAfter = Nothing,
      _lovContinuationToken = Nothing,
      _lovFetchOwner = Nothing,
      _lovPrefix = Nothing,
      _lovEncodingType = Nothing,
      _lovRequestPayer = Nothing,
      _lovMaxKeys = Nothing,
      _lovDelimiter = Nothing,
      _lovExpectedBucketOwner = Nothing,
      _lovBucket = pBucket_
    }

-- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket.
lovStartAfter :: Lens' ListObjectsV2 (Maybe Text)
lovStartAfter = lens _lovStartAfter (\s a -> s {_lovStartAfter = a})

-- | ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key.
lovContinuationToken :: Lens' ListObjectsV2 (Maybe Text)
lovContinuationToken = lens _lovContinuationToken (\s a -> s {_lovContinuationToken = a})

-- | The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true.
lovFetchOwner :: Lens' ListObjectsV2 (Maybe Bool)
lovFetchOwner = lens _lovFetchOwner (\s a -> s {_lovFetchOwner = a})

-- | Limits the response to keys that begin with the specified prefix.
lovPrefix :: Lens' ListObjectsV2 (Maybe Text)
lovPrefix = lens _lovPrefix (\s a -> s {_lovPrefix = a})

-- | Encoding type used by Amazon S3 to encode object keys in the response.
lovEncodingType :: Lens' ListObjectsV2 (Maybe EncodingType)
lovEncodingType = lens _lovEncodingType (\s a -> s {_lovEncodingType = a})

-- | Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
lovRequestPayer :: Lens' ListObjectsV2 (Maybe RequestPayer)
lovRequestPayer = lens _lovRequestPayer (\s a -> s {_lovRequestPayer = a})

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
lovMaxKeys :: Lens' ListObjectsV2 (Maybe Int)
lovMaxKeys = lens _lovMaxKeys (\s a -> s {_lovMaxKeys = a})

-- | A delimiter is a character you use to group keys.
lovDelimiter :: Lens' ListObjectsV2 (Maybe Delimiter)
lovDelimiter = lens _lovDelimiter (\s a -> s {_lovDelimiter = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
lovExpectedBucketOwner :: Lens' ListObjectsV2 (Maybe Text)
lovExpectedBucketOwner = lens _lovExpectedBucketOwner (\s a -> s {_lovExpectedBucketOwner = a})

-- | Bucket name to list.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
lovBucket :: Lens' ListObjectsV2 BucketName
lovBucket = lens _lovBucket (\s a -> s {_lovBucket = a})

instance AWSPager ListObjectsV2 where
  page rq rs
    | stop (rs ^. lovrsIsTruncated) = Nothing
    | isNothing (rs ^. lovrsNextContinuationToken) = Nothing
    | otherwise =
      Just $
        rq
          & lovContinuationToken .~ rs ^. lovrsNextContinuationToken

instance AWSRequest ListObjectsV2 where
  type Rs ListObjectsV2 = ListObjectsV2Response
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          ListObjectsV2Response'
            <$> (x .@? "StartAfter")
            <*> (x .@? "KeyCount")
            <*> (may (parseXMLList "Contents") x)
            <*> (x .@? "ContinuationToken")
            <*> (x .@? "Prefix")
            <*> (may (parseXMLList "CommonPrefixes") x)
            <*> (x .@? "EncodingType")
            <*> (x .@? "Name")
            <*> (x .@? "NextContinuationToken")
            <*> (x .@? "MaxKeys")
            <*> (x .@? "IsTruncated")
            <*> (x .@? "Delimiter")
            <*> (pure (fromEnum s))
      )

instance Hashable ListObjectsV2

instance NFData ListObjectsV2

instance ToHeaders ListObjectsV2 where
  toHeaders ListObjectsV2' {..} =
    mconcat
      [ "x-amz-request-payer" =# _lovRequestPayer,
        "x-amz-expected-bucket-owner" =# _lovExpectedBucketOwner
      ]

instance ToPath ListObjectsV2 where
  toPath ListObjectsV2' {..} = mconcat ["/", toBS _lovBucket]

instance ToQuery ListObjectsV2 where
  toQuery ListObjectsV2' {..} =
    mconcat
      [ "start-after" =: _lovStartAfter,
        "continuation-token" =: _lovContinuationToken,
        "fetch-owner" =: _lovFetchOwner,
        "prefix" =: _lovPrefix,
        "encoding-type" =: _lovEncodingType,
        "max-keys" =: _lovMaxKeys,
        "delimiter" =: _lovDelimiter,
        "list-type=2"
      ]

-- | /See:/ 'listObjectsV2Response' smart constructor.
data ListObjectsV2Response = ListObjectsV2Response'
  { _lovrsStartAfter ::
      !(Maybe Text),
    _lovrsKeyCount :: !(Maybe Int),
    _lovrsContents :: !(Maybe [Object]),
    _lovrsContinuationToken :: !(Maybe Text),
    _lovrsPrefix :: !(Maybe Text),
    _lovrsCommonPrefixes :: !(Maybe [CommonPrefix]),
    _lovrsEncodingType :: !(Maybe EncodingType),
    _lovrsName :: !(Maybe BucketName),
    _lovrsNextContinuationToken :: !(Maybe Text),
    _lovrsMaxKeys :: !(Maybe Int),
    _lovrsIsTruncated :: !(Maybe Bool),
    _lovrsDelimiter :: !(Maybe Delimiter),
    _lovrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListObjectsV2Response' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lovrsStartAfter' - If StartAfter was sent with the request, it is included in the response.
--
-- * 'lovrsKeyCount' - KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
--
-- * 'lovrsContents' - Metadata about each object returned.
--
-- * 'lovrsContinuationToken' - If ContinuationToken was sent with the request, it is included in the response.
--
-- * 'lovrsPrefix' - Keys that begin with the indicated prefix.
--
-- * 'lovrsCommonPrefixes' - All of the keys rolled up into a common prefix count as a single return when calculating the number of returns. A response can contain @CommonPrefixes@ only if you specify a delimiter. @CommonPrefixes@ contains all (if there are any) keys between @Prefix@ and the next occurrence of the string specified by a delimiter. @CommonPrefixes@ lists keys that act like subdirectories in the directory specified by @Prefix@ . For example, if the prefix is @notes/@ and the delimiter is a slash (@/@ ) as in @notes/summer/july@ , the common prefix is @notes/summer/@ . All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
--
-- * 'lovrsEncodingType' - Encoding type used by Amazon S3 to encode object key names in the XML response. If you specify the encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements: @Delimiter, Prefix, Key,@ and @StartAfter@ .
--
-- * 'lovrsName' - The bucket name. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'lovrsNextContinuationToken' - @NextContinuationToken@ is sent when @isTruncated@ is true, which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this @NextContinuationToken@ . @NextContinuationToken@ is obfuscated and is not a real key
--
-- * 'lovrsMaxKeys' - Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
--
-- * 'lovrsIsTruncated' - Set to false if all of the results were returned. Set to true if more keys are available to return. If the number of results exceeds that specified by MaxKeys, all of the results might not be returned.
--
-- * 'lovrsDelimiter' - Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the CommonPrefixes collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
--
-- * 'lovrsResponseStatus' - -- | The response status code.
listObjectsV2Response ::
  -- | 'lovrsResponseStatus'
  Int ->
  ListObjectsV2Response
listObjectsV2Response pResponseStatus_ =
  ListObjectsV2Response'
    { _lovrsStartAfter = Nothing,
      _lovrsKeyCount = Nothing,
      _lovrsContents = Nothing,
      _lovrsContinuationToken = Nothing,
      _lovrsPrefix = Nothing,
      _lovrsCommonPrefixes = Nothing,
      _lovrsEncodingType = Nothing,
      _lovrsName = Nothing,
      _lovrsNextContinuationToken = Nothing,
      _lovrsMaxKeys = Nothing,
      _lovrsIsTruncated = Nothing,
      _lovrsDelimiter = Nothing,
      _lovrsResponseStatus = pResponseStatus_
    }

-- | If StartAfter was sent with the request, it is included in the response.
lovrsStartAfter :: Lens' ListObjectsV2Response (Maybe Text)
lovrsStartAfter = lens _lovrsStartAfter (\s a -> s {_lovrsStartAfter = a})

-- | KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
lovrsKeyCount :: Lens' ListObjectsV2Response (Maybe Int)
lovrsKeyCount = lens _lovrsKeyCount (\s a -> s {_lovrsKeyCount = a})

-- | Metadata about each object returned.
lovrsContents :: Lens' ListObjectsV2Response [Object]
lovrsContents = lens _lovrsContents (\s a -> s {_lovrsContents = a}) . _Default . _Coerce

-- | If ContinuationToken was sent with the request, it is included in the response.
lovrsContinuationToken :: Lens' ListObjectsV2Response (Maybe Text)
lovrsContinuationToken = lens _lovrsContinuationToken (\s a -> s {_lovrsContinuationToken = a})

-- | Keys that begin with the indicated prefix.
lovrsPrefix :: Lens' ListObjectsV2Response (Maybe Text)
lovrsPrefix = lens _lovrsPrefix (\s a -> s {_lovrsPrefix = a})

-- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns. A response can contain @CommonPrefixes@ only if you specify a delimiter. @CommonPrefixes@ contains all (if there are any) keys between @Prefix@ and the next occurrence of the string specified by a delimiter. @CommonPrefixes@ lists keys that act like subdirectories in the directory specified by @Prefix@ . For example, if the prefix is @notes/@ and the delimiter is a slash (@/@ ) as in @notes/summer/july@ , the common prefix is @notes/summer/@ . All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
lovrsCommonPrefixes :: Lens' ListObjectsV2Response [CommonPrefix]
lovrsCommonPrefixes = lens _lovrsCommonPrefixes (\s a -> s {_lovrsCommonPrefixes = a}) . _Default . _Coerce

-- | Encoding type used by Amazon S3 to encode object key names in the XML response. If you specify the encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements: @Delimiter, Prefix, Key,@ and @StartAfter@ .
lovrsEncodingType :: Lens' ListObjectsV2Response (Maybe EncodingType)
lovrsEncodingType = lens _lovrsEncodingType (\s a -> s {_lovrsEncodingType = a})

-- | The bucket name. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
lovrsName :: Lens' ListObjectsV2Response (Maybe BucketName)
lovrsName = lens _lovrsName (\s a -> s {_lovrsName = a})

-- | @NextContinuationToken@ is sent when @isTruncated@ is true, which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this @NextContinuationToken@ . @NextContinuationToken@ is obfuscated and is not a real key
lovrsNextContinuationToken :: Lens' ListObjectsV2Response (Maybe Text)
lovrsNextContinuationToken = lens _lovrsNextContinuationToken (\s a -> s {_lovrsNextContinuationToken = a})

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
lovrsMaxKeys :: Lens' ListObjectsV2Response (Maybe Int)
lovrsMaxKeys = lens _lovrsMaxKeys (\s a -> s {_lovrsMaxKeys = a})

-- | Set to false if all of the results were returned. Set to true if more keys are available to return. If the number of results exceeds that specified by MaxKeys, all of the results might not be returned.
lovrsIsTruncated :: Lens' ListObjectsV2Response (Maybe Bool)
lovrsIsTruncated = lens _lovrsIsTruncated (\s a -> s {_lovrsIsTruncated = a})

-- | Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the CommonPrefixes collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
lovrsDelimiter :: Lens' ListObjectsV2Response (Maybe Delimiter)
lovrsDelimiter = lens _lovrsDelimiter (\s a -> s {_lovrsDelimiter = a})

-- | -- | The response status code.
lovrsResponseStatus :: Lens' ListObjectsV2Response Int
lovrsResponseStatus = lens _lovrsResponseStatus (\s a -> s {_lovrsResponseStatus = a})

instance NFData ListObjectsV2Response
