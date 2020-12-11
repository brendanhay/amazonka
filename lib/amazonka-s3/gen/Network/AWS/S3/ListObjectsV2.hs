{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- To use this operation, you must have READ access to the bucket.
-- To use this operation in an AWS Identity and Access Management (IAM) policy, you must have permissions to perform the @s3:ListBucket@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- /Important:/ This section describes the latest revision of the API. We recommend that you use this revised API for application development. For backward compatibility, Amazon S3 continues to support the prior version of this API, <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects> .
-- To get a list of your buckets, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets> .
-- The following operations are related to @ListObjectsV2@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjectsV2
  ( -- * Creating a request
    ListObjectsV2 (..),
    mkListObjectsV2,

    -- ** Request lenses
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

    -- * Destructuring the response
    ListObjectsV2Response (..),
    mkListObjectsV2Response,

    -- ** Response lenses
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListObjectsV2' smart constructor.
data ListObjectsV2 = ListObjectsV2'
  { startAfter ::
      Lude.Maybe Lude.Text,
    continuationToken :: Lude.Maybe Lude.Text,
    fetchOwner :: Lude.Maybe Lude.Bool,
    prefix :: Lude.Maybe Lude.Text,
    encodingType :: Lude.Maybe EncodingType,
    requestPayer :: Lude.Maybe RequestPayer,
    maxKeys :: Lude.Maybe Lude.Int,
    delimiter :: Lude.Maybe Delimiter,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectsV2' with the minimum fields required to make a request.
--
-- * 'bucket' - Bucket name to list.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'continuationToken' - ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key.
-- * 'delimiter' - A delimiter is a character you use to group keys.
-- * 'encodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'fetchOwner' - The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true.
-- * 'maxKeys' - Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
-- * 'prefix' - Limits the response to keys that begin with the specified prefix.
-- * 'requestPayer' - Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
-- * 'startAfter' - StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket.
mkListObjectsV2 ::
  -- | 'bucket'
  BucketName ->
  ListObjectsV2
mkListObjectsV2 pBucket_ =
  ListObjectsV2'
    { startAfter = Lude.Nothing,
      continuationToken = Lude.Nothing,
      fetchOwner = Lude.Nothing,
      prefix = Lude.Nothing,
      encodingType = Lude.Nothing,
      requestPayer = Lude.Nothing,
      maxKeys = Lude.Nothing,
      delimiter = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket.
--
-- /Note:/ Consider using 'startAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovStartAfter :: Lens.Lens' ListObjectsV2 (Lude.Maybe Lude.Text)
lovStartAfter = Lens.lens (startAfter :: ListObjectsV2 -> Lude.Maybe Lude.Text) (\s a -> s {startAfter = a} :: ListObjectsV2)
{-# DEPRECATED lovStartAfter "Use generic-lens or generic-optics with 'startAfter' instead." #-}

-- | ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovContinuationToken :: Lens.Lens' ListObjectsV2 (Lude.Maybe Lude.Text)
lovContinuationToken = Lens.lens (continuationToken :: ListObjectsV2 -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListObjectsV2)
{-# DEPRECATED lovContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true.
--
-- /Note:/ Consider using 'fetchOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovFetchOwner :: Lens.Lens' ListObjectsV2 (Lude.Maybe Lude.Bool)
lovFetchOwner = Lens.lens (fetchOwner :: ListObjectsV2 -> Lude.Maybe Lude.Bool) (\s a -> s {fetchOwner = a} :: ListObjectsV2)
{-# DEPRECATED lovFetchOwner "Use generic-lens or generic-optics with 'fetchOwner' instead." #-}

-- | Limits the response to keys that begin with the specified prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovPrefix :: Lens.Lens' ListObjectsV2 (Lude.Maybe Lude.Text)
lovPrefix = Lens.lens (prefix :: ListObjectsV2 -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListObjectsV2)
{-# DEPRECATED lovPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovEncodingType :: Lens.Lens' ListObjectsV2 (Lude.Maybe EncodingType)
lovEncodingType = Lens.lens (encodingType :: ListObjectsV2 -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListObjectsV2)
{-# DEPRECATED lovEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovRequestPayer :: Lens.Lens' ListObjectsV2 (Lude.Maybe RequestPayer)
lovRequestPayer = Lens.lens (requestPayer :: ListObjectsV2 -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: ListObjectsV2)
{-# DEPRECATED lovRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovMaxKeys :: Lens.Lens' ListObjectsV2 (Lude.Maybe Lude.Int)
lovMaxKeys = Lens.lens (maxKeys :: ListObjectsV2 -> Lude.Maybe Lude.Int) (\s a -> s {maxKeys = a} :: ListObjectsV2)
{-# DEPRECATED lovMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | A delimiter is a character you use to group keys.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovDelimiter :: Lens.Lens' ListObjectsV2 (Lude.Maybe Delimiter)
lovDelimiter = Lens.lens (delimiter :: ListObjectsV2 -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListObjectsV2)
{-# DEPRECATED lovDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovExpectedBucketOwner :: Lens.Lens' ListObjectsV2 (Lude.Maybe Lude.Text)
lovExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListObjectsV2 -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListObjectsV2)
{-# DEPRECATED lovExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Bucket name to list.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovBucket :: Lens.Lens' ListObjectsV2 BucketName
lovBucket = Lens.lens (bucket :: ListObjectsV2 -> BucketName) (\s a -> s {bucket = a} :: ListObjectsV2)
{-# DEPRECATED lovBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Page.AWSPager ListObjectsV2 where
  page rq rs
    | Page.stop (rs Lens.^. lovrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lovrsNextContinuationToken) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lovContinuationToken Lens..~ rs Lens.^. lovrsNextContinuationToken

instance Lude.AWSRequest ListObjectsV2 where
  type Rs ListObjectsV2 = ListObjectsV2Response
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListObjectsV2Response'
            Lude.<$> (x Lude..@? "StartAfter")
            Lude.<*> (x Lude..@? "KeyCount")
            Lude.<*> (Lude.may (Lude.parseXMLList "Contents") x)
            Lude.<*> (x Lude..@? "ContinuationToken")
            Lude.<*> (x Lude..@? "Prefix")
            Lude.<*> (Lude.may (Lude.parseXMLList "CommonPrefixes") x)
            Lude.<*> (x Lude..@? "EncodingType")
            Lude.<*> (x Lude..@? "Name")
            Lude.<*> (x Lude..@? "NextContinuationToken")
            Lude.<*> (x Lude..@? "MaxKeys")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (x Lude..@? "Delimiter")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListObjectsV2 where
  toHeaders ListObjectsV2' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath ListObjectsV2 where
  toPath ListObjectsV2' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery ListObjectsV2 where
  toQuery ListObjectsV2' {..} =
    Lude.mconcat
      [ "start-after" Lude.=: startAfter,
        "continuation-token" Lude.=: continuationToken,
        "fetch-owner" Lude.=: fetchOwner,
        "prefix" Lude.=: prefix,
        "encoding-type" Lude.=: encodingType,
        "max-keys" Lude.=: maxKeys,
        "delimiter" Lude.=: delimiter,
        "list-type=2"
      ]

-- | /See:/ 'mkListObjectsV2Response' smart constructor.
data ListObjectsV2Response = ListObjectsV2Response'
  { startAfter ::
      Lude.Maybe Lude.Text,
    keyCount :: Lude.Maybe Lude.Int,
    contents :: Lude.Maybe [Object],
    continuationToken :: Lude.Maybe Lude.Text,
    prefix :: Lude.Maybe Lude.Text,
    commonPrefixes :: Lude.Maybe [CommonPrefix],
    encodingType :: Lude.Maybe EncodingType,
    name :: Lude.Maybe BucketName,
    nextContinuationToken :: Lude.Maybe Lude.Text,
    maxKeys :: Lude.Maybe Lude.Int,
    isTruncated :: Lude.Maybe Lude.Bool,
    delimiter :: Lude.Maybe Delimiter,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectsV2Response' with the minimum fields required to make a request.
--
-- * 'commonPrefixes' - All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
--
-- A response can contain @CommonPrefixes@ only if you specify a delimiter.
-- @CommonPrefixes@ contains all (if there are any) keys between @Prefix@ and the next occurrence of the string specified by a delimiter.
-- @CommonPrefixes@ lists keys that act like subdirectories in the directory specified by @Prefix@ .
-- For example, if the prefix is @notes/@ and the delimiter is a slash (@/@ ) as in @notes/summer/july@ , the common prefix is @notes/summer/@ . All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
-- * 'contents' - Metadata about each object returned.
-- * 'continuationToken' - If ContinuationToken was sent with the request, it is included in the response.
-- * 'delimiter' - Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the CommonPrefixes collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
-- * 'encodingType' - Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify the encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @Delimiter, Prefix, Key,@ and @StartAfter@ .
-- * 'isTruncated' - Set to false if all of the results were returned. Set to true if more keys are available to return. If the number of results exceeds that specified by MaxKeys, all of the results might not be returned.
-- * 'keyCount' - KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
-- * 'maxKeys' - Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
-- * 'name' - The bucket name.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'nextContinuationToken' - @NextContinuationToken@ is sent when @isTruncated@ is true, which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this @NextContinuationToken@ . @NextContinuationToken@ is obfuscated and is not a real key
-- * 'prefix' - Keys that begin with the indicated prefix.
-- * 'responseStatus' - The response status code.
-- * 'startAfter' - If StartAfter was sent with the request, it is included in the response.
mkListObjectsV2Response ::
  -- | 'responseStatus'
  Lude.Int ->
  ListObjectsV2Response
mkListObjectsV2Response pResponseStatus_ =
  ListObjectsV2Response'
    { startAfter = Lude.Nothing,
      keyCount = Lude.Nothing,
      contents = Lude.Nothing,
      continuationToken = Lude.Nothing,
      prefix = Lude.Nothing,
      commonPrefixes = Lude.Nothing,
      encodingType = Lude.Nothing,
      name = Lude.Nothing,
      nextContinuationToken = Lude.Nothing,
      maxKeys = Lude.Nothing,
      isTruncated = Lude.Nothing,
      delimiter = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If StartAfter was sent with the request, it is included in the response.
--
-- /Note:/ Consider using 'startAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsStartAfter :: Lens.Lens' ListObjectsV2Response (Lude.Maybe Lude.Text)
lovrsStartAfter = Lens.lens (startAfter :: ListObjectsV2Response -> Lude.Maybe Lude.Text) (\s a -> s {startAfter = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsStartAfter "Use generic-lens or generic-optics with 'startAfter' instead." #-}

-- | KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
--
-- /Note:/ Consider using 'keyCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsKeyCount :: Lens.Lens' ListObjectsV2Response (Lude.Maybe Lude.Int)
lovrsKeyCount = Lens.lens (keyCount :: ListObjectsV2Response -> Lude.Maybe Lude.Int) (\s a -> s {keyCount = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsKeyCount "Use generic-lens or generic-optics with 'keyCount' instead." #-}

-- | Metadata about each object returned.
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsContents :: Lens.Lens' ListObjectsV2Response (Lude.Maybe [Object])
lovrsContents = Lens.lens (contents :: ListObjectsV2Response -> Lude.Maybe [Object]) (\s a -> s {contents = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsContents "Use generic-lens or generic-optics with 'contents' instead." #-}

-- | If ContinuationToken was sent with the request, it is included in the response.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsContinuationToken :: Lens.Lens' ListObjectsV2Response (Lude.Maybe Lude.Text)
lovrsContinuationToken = Lens.lens (continuationToken :: ListObjectsV2Response -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | Keys that begin with the indicated prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsPrefix :: Lens.Lens' ListObjectsV2Response (Lude.Maybe Lude.Text)
lovrsPrefix = Lens.lens (prefix :: ListObjectsV2Response -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
--
-- A response can contain @CommonPrefixes@ only if you specify a delimiter.
-- @CommonPrefixes@ contains all (if there are any) keys between @Prefix@ and the next occurrence of the string specified by a delimiter.
-- @CommonPrefixes@ lists keys that act like subdirectories in the directory specified by @Prefix@ .
-- For example, if the prefix is @notes/@ and the delimiter is a slash (@/@ ) as in @notes/summer/july@ , the common prefix is @notes/summer/@ . All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsCommonPrefixes :: Lens.Lens' ListObjectsV2Response (Lude.Maybe [CommonPrefix])
lovrsCommonPrefixes = Lens.lens (commonPrefixes :: ListObjectsV2Response -> Lude.Maybe [CommonPrefix]) (\s a -> s {commonPrefixes = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsCommonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead." #-}

-- | Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify the encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @Delimiter, Prefix, Key,@ and @StartAfter@ .
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsEncodingType :: Lens.Lens' ListObjectsV2Response (Lude.Maybe EncodingType)
lovrsEncodingType = Lens.lens (encodingType :: ListObjectsV2Response -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The bucket name.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsName :: Lens.Lens' ListObjectsV2Response (Lude.Maybe BucketName)
lovrsName = Lens.lens (name :: ListObjectsV2Response -> Lude.Maybe BucketName) (\s a -> s {name = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | @NextContinuationToken@ is sent when @isTruncated@ is true, which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this @NextContinuationToken@ . @NextContinuationToken@ is obfuscated and is not a real key
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsNextContinuationToken :: Lens.Lens' ListObjectsV2Response (Lude.Maybe Lude.Text)
lovrsNextContinuationToken = Lens.lens (nextContinuationToken :: ListObjectsV2Response -> Lude.Maybe Lude.Text) (\s a -> s {nextContinuationToken = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsNextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsMaxKeys :: Lens.Lens' ListObjectsV2Response (Lude.Maybe Lude.Int)
lovrsMaxKeys = Lens.lens (maxKeys :: ListObjectsV2Response -> Lude.Maybe Lude.Int) (\s a -> s {maxKeys = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | Set to false if all of the results were returned. Set to true if more keys are available to return. If the number of results exceeds that specified by MaxKeys, all of the results might not be returned.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsIsTruncated :: Lens.Lens' ListObjectsV2Response (Lude.Maybe Lude.Bool)
lovrsIsTruncated = Lens.lens (isTruncated :: ListObjectsV2Response -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the CommonPrefixes collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsDelimiter :: Lens.Lens' ListObjectsV2Response (Lude.Maybe Delimiter)
lovrsDelimiter = Lens.lens (delimiter :: ListObjectsV2Response -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsResponseStatus :: Lens.Lens' ListObjectsV2Response Lude.Int
lovrsResponseStatus = Lens.lens (responseStatus :: ListObjectsV2Response -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectsV2Response)
{-# DEPRECATED lovrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
