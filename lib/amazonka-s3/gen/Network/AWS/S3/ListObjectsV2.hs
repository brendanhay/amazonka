{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
    lovBucket,
    lovContinuationToken,
    lovDelimiter,
    lovEncodingType,
    lovExpectedBucketOwner,
    lovFetchOwner,
    lovMaxKeys,
    lovPrefix,
    lovRequestPayer,
    lovStartAfter,

    -- * Destructuring the response
    ListObjectsV2Response (..),
    mkListObjectsV2Response,

    -- ** Response lenses
    lovrrsCommonPrefixes,
    lovrrsContents,
    lovrrsContinuationToken,
    lovrrsDelimiter,
    lovrrsEncodingType,
    lovrrsIsTruncated,
    lovrrsKeyCount,
    lovrrsMaxKeys,
    lovrrsName,
    lovrrsNextContinuationToken,
    lovrrsPrefix,
    lovrrsStartAfter,
    lovrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListObjectsV2' smart constructor.
data ListObjectsV2 = ListObjectsV2'
  { -- | Bucket name to list.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key.
    continuationToken :: Core.Maybe Types.Token,
    -- | A delimiter is a character you use to group keys.
    delimiter :: Core.Maybe Types.Delimiter,
    -- | Encoding type used by Amazon S3 to encode object keys in the response.
    encodingType :: Core.Maybe Types.EncodingType,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId,
    -- | The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true.
    fetchOwner :: Core.Maybe Core.Bool,
    -- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
    maxKeys :: Core.Maybe Core.Int,
    -- | Limits the response to keys that begin with the specified prefix.
    prefix :: Core.Maybe Types.Prefix,
    -- | Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
    requestPayer :: Core.Maybe Types.RequestPayer,
    -- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket.
    startAfter :: Core.Maybe Types.StartAfter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectsV2' value with any optional fields omitted.
mkListObjectsV2 ::
  -- | 'bucket'
  Types.BucketName ->
  ListObjectsV2
mkListObjectsV2 bucket =
  ListObjectsV2'
    { bucket,
      continuationToken = Core.Nothing,
      delimiter = Core.Nothing,
      encodingType = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      fetchOwner = Core.Nothing,
      maxKeys = Core.Nothing,
      prefix = Core.Nothing,
      requestPayer = Core.Nothing,
      startAfter = Core.Nothing
    }

-- | Bucket name to list.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovBucket :: Lens.Lens' ListObjectsV2 Types.BucketName
lovBucket = Lens.field @"bucket"
{-# DEPRECATED lovBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | ContinuationToken indicates Amazon S3 that the list is being continued on this bucket with a token. ContinuationToken is obfuscated and is not a real key.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovContinuationToken :: Lens.Lens' ListObjectsV2 (Core.Maybe Types.Token)
lovContinuationToken = Lens.field @"continuationToken"
{-# DEPRECATED lovContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | A delimiter is a character you use to group keys.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovDelimiter :: Lens.Lens' ListObjectsV2 (Core.Maybe Types.Delimiter)
lovDelimiter = Lens.field @"delimiter"
{-# DEPRECATED lovDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovEncodingType :: Lens.Lens' ListObjectsV2 (Core.Maybe Types.EncodingType)
lovEncodingType = Lens.field @"encodingType"
{-# DEPRECATED lovEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovExpectedBucketOwner :: Lens.Lens' ListObjectsV2 (Core.Maybe Types.AccountId)
lovExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED lovExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The owner field is not present in listV2 by default, if you want to return owner field with each key in the result then set the fetch owner field to true.
--
-- /Note:/ Consider using 'fetchOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovFetchOwner :: Lens.Lens' ListObjectsV2 (Core.Maybe Core.Bool)
lovFetchOwner = Lens.field @"fetchOwner"
{-# DEPRECATED lovFetchOwner "Use generic-lens or generic-optics with 'fetchOwner' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovMaxKeys :: Lens.Lens' ListObjectsV2 (Core.Maybe Core.Int)
lovMaxKeys = Lens.field @"maxKeys"
{-# DEPRECATED lovMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | Limits the response to keys that begin with the specified prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovPrefix :: Lens.Lens' ListObjectsV2 (Core.Maybe Types.Prefix)
lovPrefix = Lens.field @"prefix"
{-# DEPRECATED lovPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Confirms that the requester knows that she or he will be charged for the list objects request in V2 style. Bucket owners need not specify this parameter in their requests.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovRequestPayer :: Lens.Lens' ListObjectsV2 (Core.Maybe Types.RequestPayer)
lovRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED lovRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3 starts listing after this specified key. StartAfter can be any key in the bucket.
--
-- /Note:/ Consider using 'startAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovStartAfter :: Lens.Lens' ListObjectsV2 (Core.Maybe Types.StartAfter)
lovStartAfter = Lens.field @"startAfter"
{-# DEPRECATED lovStartAfter "Use generic-lens or generic-optics with 'startAfter' instead." #-}

instance Core.AWSRequest ListObjectsV2 where
  type Rs ListObjectsV2 = ListObjectsV2Response
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "continuation-token" Core.<$> continuationToken
            Core.<> (Core.toQueryValue "delimiter" Core.<$> delimiter)
            Core.<> (Core.toQueryValue "encoding-type" Core.<$> encodingType)
            Core.<> (Core.toQueryValue "fetch-owner" Core.<$> fetchOwner)
            Core.<> (Core.toQueryValue "max-keys" Core.<$> maxKeys)
            Core.<> (Core.toQueryValue "prefix" Core.<$> prefix)
            Core.<> (Core.toQueryValue "start-after" Core.<$> startAfter)
            Core.<> (Core.pure ("list-type=2", "")),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer),
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListObjectsV2Response'
            Core.<$> (x Core..@? "CommonPrefixes")
            Core.<*> (x Core..@? "Contents")
            Core.<*> (x Core..@? "ContinuationToken")
            Core.<*> (x Core..@? "Delimiter")
            Core.<*> (x Core..@? "EncodingType")
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "KeyCount")
            Core.<*> (x Core..@? "MaxKeys")
            Core.<*> (x Core..@? "Name")
            Core.<*> (x Core..@? "NextContinuationToken")
            Core.<*> (x Core..@? "Prefix")
            Core.<*> (x Core..@? "StartAfter")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListObjectsV2 where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"nextContinuationToken") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"continuationToken"
            Lens..~ rs Lens.^. Lens.field @"nextContinuationToken"
        )

-- | /See:/ 'mkListObjectsV2Response' smart constructor.
data ListObjectsV2Response = ListObjectsV2Response'
  { -- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
    --
    -- A response can contain @CommonPrefixes@ only if you specify a delimiter.
    -- @CommonPrefixes@ contains all (if there are any) keys between @Prefix@ and the next occurrence of the string specified by a delimiter.
    -- @CommonPrefixes@ lists keys that act like subdirectories in the directory specified by @Prefix@ .
    -- For example, if the prefix is @notes/@ and the delimiter is a slash (@/@ ) as in @notes/summer/july@ , the common prefix is @notes/summer/@ . All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
    commonPrefixes :: Core.Maybe [Types.CommonPrefix],
    -- | Metadata about each object returned.
    contents :: Core.Maybe [Types.Object],
    -- | If ContinuationToken was sent with the request, it is included in the response.
    continuationToken :: Core.Maybe Types.Token,
    -- | Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the CommonPrefixes collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
    delimiter :: Core.Maybe Types.Delimiter,
    -- | Encoding type used by Amazon S3 to encode object key names in the XML response.
    --
    -- If you specify the encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
    -- @Delimiter, Prefix, Key,@ and @StartAfter@ .
    encodingType :: Core.Maybe Types.EncodingType,
    -- | Set to false if all of the results were returned. Set to true if more keys are available to return. If the number of results exceeds that specified by MaxKeys, all of the results might not be returned.
    isTruncated :: Core.Maybe Core.Bool,
    -- | KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
    keyCount :: Core.Maybe Core.Int,
    -- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
    maxKeys :: Core.Maybe Core.Int,
    -- | The bucket name.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    name :: Core.Maybe Types.BucketName,
    -- | @NextContinuationToken@ is sent when @isTruncated@ is true, which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this @NextContinuationToken@ . @NextContinuationToken@ is obfuscated and is not a real key
    nextContinuationToken :: Core.Maybe Types.NextToken,
    -- | Keys that begin with the indicated prefix.
    prefix :: Core.Maybe Types.Prefix,
    -- | If StartAfter was sent with the request, it is included in the response.
    startAfter :: Core.Maybe Types.StartAfter,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListObjectsV2Response' value with any optional fields omitted.
mkListObjectsV2Response ::
  -- | 'responseStatus'
  Core.Int ->
  ListObjectsV2Response
mkListObjectsV2Response responseStatus =
  ListObjectsV2Response'
    { commonPrefixes = Core.Nothing,
      contents = Core.Nothing,
      continuationToken = Core.Nothing,
      delimiter = Core.Nothing,
      encodingType = Core.Nothing,
      isTruncated = Core.Nothing,
      keyCount = Core.Nothing,
      maxKeys = Core.Nothing,
      name = Core.Nothing,
      nextContinuationToken = Core.Nothing,
      prefix = Core.Nothing,
      startAfter = Core.Nothing,
      responseStatus
    }

-- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
--
-- A response can contain @CommonPrefixes@ only if you specify a delimiter.
-- @CommonPrefixes@ contains all (if there are any) keys between @Prefix@ and the next occurrence of the string specified by a delimiter.
-- @CommonPrefixes@ lists keys that act like subdirectories in the directory specified by @Prefix@ .
-- For example, if the prefix is @notes/@ and the delimiter is a slash (@/@ ) as in @notes/summer/july@ , the common prefix is @notes/summer/@ . All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsCommonPrefixes :: Lens.Lens' ListObjectsV2Response (Core.Maybe [Types.CommonPrefix])
lovrrsCommonPrefixes = Lens.field @"commonPrefixes"
{-# DEPRECATED lovrrsCommonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead." #-}

-- | Metadata about each object returned.
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsContents :: Lens.Lens' ListObjectsV2Response (Core.Maybe [Types.Object])
lovrrsContents = Lens.field @"contents"
{-# DEPRECATED lovrrsContents "Use generic-lens or generic-optics with 'contents' instead." #-}

-- | If ContinuationToken was sent with the request, it is included in the response.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsContinuationToken :: Lens.Lens' ListObjectsV2Response (Core.Maybe Types.Token)
lovrrsContinuationToken = Lens.field @"continuationToken"
{-# DEPRECATED lovrrsContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the CommonPrefixes collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsDelimiter :: Lens.Lens' ListObjectsV2Response (Core.Maybe Types.Delimiter)
lovrrsDelimiter = Lens.field @"delimiter"
{-# DEPRECATED lovrrsDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify the encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @Delimiter, Prefix, Key,@ and @StartAfter@ .
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsEncodingType :: Lens.Lens' ListObjectsV2Response (Core.Maybe Types.EncodingType)
lovrrsEncodingType = Lens.field @"encodingType"
{-# DEPRECATED lovrrsEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | Set to false if all of the results were returned. Set to true if more keys are available to return. If the number of results exceeds that specified by MaxKeys, all of the results might not be returned.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsIsTruncated :: Lens.Lens' ListObjectsV2Response (Core.Maybe Core.Bool)
lovrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lovrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | KeyCount is the number of keys returned with this request. KeyCount will always be less than equals to MaxKeys field. Say you ask for 50 keys, your result will include less than equals 50 keys
--
-- /Note:/ Consider using 'keyCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsKeyCount :: Lens.Lens' ListObjectsV2Response (Core.Maybe Core.Int)
lovrrsKeyCount = Lens.field @"keyCount"
{-# DEPRECATED lovrrsKeyCount "Use generic-lens or generic-optics with 'keyCount' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsMaxKeys :: Lens.Lens' ListObjectsV2Response (Core.Maybe Core.Int)
lovrrsMaxKeys = Lens.field @"maxKeys"
{-# DEPRECATED lovrrsMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | The bucket name.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsName :: Lens.Lens' ListObjectsV2Response (Core.Maybe Types.BucketName)
lovrrsName = Lens.field @"name"
{-# DEPRECATED lovrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | @NextContinuationToken@ is sent when @isTruncated@ is true, which means there are more keys in the bucket that can be listed. The next list requests to Amazon S3 can be continued with this @NextContinuationToken@ . @NextContinuationToken@ is obfuscated and is not a real key
--
-- /Note:/ Consider using 'nextContinuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsNextContinuationToken :: Lens.Lens' ListObjectsV2Response (Core.Maybe Types.NextToken)
lovrrsNextContinuationToken = Lens.field @"nextContinuationToken"
{-# DEPRECATED lovrrsNextContinuationToken "Use generic-lens or generic-optics with 'nextContinuationToken' instead." #-}

-- | Keys that begin with the indicated prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsPrefix :: Lens.Lens' ListObjectsV2Response (Core.Maybe Types.Prefix)
lovrrsPrefix = Lens.field @"prefix"
{-# DEPRECATED lovrrsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | If StartAfter was sent with the request, it is included in the response.
--
-- /Note:/ Consider using 'startAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsStartAfter :: Lens.Lens' ListObjectsV2Response (Core.Maybe Types.StartAfter)
lovrrsStartAfter = Lens.field @"startAfter"
{-# DEPRECATED lovrrsStartAfter "Use generic-lens or generic-optics with 'startAfter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrrsResponseStatus :: Lens.Lens' ListObjectsV2Response Core.Int
lovrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lovrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
