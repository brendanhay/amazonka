{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1,000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. A 200 OK response can contain valid or invalid XML. Be sure to design your application to parse the contents of the response and handle it appropriately.
--
-- /Important:/ This API has been revised. We recommend that you use the newer version, <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html ListObjectsV2> , when developing applications. For backward compatibility, Amazon S3 continues to support @ListObjects@ .
-- The following operations are related to @ListObjects@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html ListObjectsV2>
--
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
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets>
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjects
  ( -- * Creating a request
    ListObjects (..),
    mkListObjects,

    -- ** Request lenses
    loBucket,
    loDelimiter,
    loEncodingType,
    loExpectedBucketOwner,
    loMarker,
    loMaxKeys,
    loPrefix,
    loRequestPayer,

    -- * Destructuring the response
    ListObjectsResponse (..),
    mkListObjectsResponse,

    -- ** Response lenses
    lorrsCommonPrefixes,
    lorrsContents,
    lorrsDelimiter,
    lorrsEncodingType,
    lorrsIsTruncated,
    lorrsMarker,
    lorrsMaxKeys,
    lorrsName,
    lorrsNextMarker,
    lorrsPrefix,
    lorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListObjects' smart constructor.
data ListObjects = ListObjects'
  { -- | The name of the bucket containing the objects.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | A delimiter is a character you use to group keys.
    delimiter :: Core.Maybe Types.Delimiter,
    encodingType :: Core.Maybe Types.EncodingType,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    -- | Specifies the key to start with when listing objects in a bucket.
    marker :: Core.Maybe Types.Marker,
    -- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
    maxKeys :: Core.Maybe Core.Int,
    -- | Limits the response to keys that begin with the specified prefix.
    prefix :: Core.Maybe Types.Prefix,
    -- | Confirms that the requester knows that she or he will be charged for the list objects request. Bucket owners need not specify this parameter in their requests.
    requestPayer :: Core.Maybe Types.RequestPayer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjects' value with any optional fields omitted.
mkListObjects ::
  -- | 'bucket'
  Types.BucketName ->
  ListObjects
mkListObjects bucket =
  ListObjects'
    { bucket,
      delimiter = Core.Nothing,
      encodingType = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      marker = Core.Nothing,
      maxKeys = Core.Nothing,
      prefix = Core.Nothing,
      requestPayer = Core.Nothing
    }

-- | The name of the bucket containing the objects.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loBucket :: Lens.Lens' ListObjects Types.BucketName
loBucket = Lens.field @"bucket"
{-# DEPRECATED loBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | A delimiter is a character you use to group keys.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loDelimiter :: Lens.Lens' ListObjects (Core.Maybe Types.Delimiter)
loDelimiter = Lens.field @"delimiter"
{-# DEPRECATED loDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loEncodingType :: Lens.Lens' ListObjects (Core.Maybe Types.EncodingType)
loEncodingType = Lens.field @"encodingType"
{-# DEPRECATED loEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loExpectedBucketOwner :: Lens.Lens' ListObjects (Core.Maybe Types.ExpectedBucketOwner)
loExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED loExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Specifies the key to start with when listing objects in a bucket.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMarker :: Lens.Lens' ListObjects (Core.Maybe Types.Marker)
loMarker = Lens.field @"marker"
{-# DEPRECATED loMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxKeys :: Lens.Lens' ListObjects (Core.Maybe Core.Int)
loMaxKeys = Lens.field @"maxKeys"
{-# DEPRECATED loMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | Limits the response to keys that begin with the specified prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loPrefix :: Lens.Lens' ListObjects (Core.Maybe Types.Prefix)
loPrefix = Lens.field @"prefix"
{-# DEPRECATED loPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Confirms that the requester knows that she or he will be charged for the list objects request. Bucket owners need not specify this parameter in their requests.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loRequestPayer :: Lens.Lens' ListObjects (Core.Maybe Types.RequestPayer)
loRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED loRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

instance Core.AWSRequest ListObjects where
  type Rs ListObjects = ListObjectsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "delimiter" Core.<$> delimiter
            Core.<> (Core.toQueryValue "encoding-type" Core.<$> encodingType)
            Core.<> (Core.toQueryValue "marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "max-keys" Core.<$> maxKeys)
            Core.<> (Core.toQueryValue "prefix" Core.<$> prefix),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer),
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListObjectsResponse'
            Core.<$> (x Core..@? "CommonPrefixes")
            Core.<*> (x Core..@? "Contents")
            Core.<*> (x Core..@? "Delimiter")
            Core.<*> (x Core..@? "EncodingType")
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (x Core..@? "MaxKeys")
            Core.<*> (x Core..@? "Name")
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (x Core..@? "Prefix")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListObjects where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"nextMarker") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListObjectsResponse' smart constructor.
data ListObjectsResponse = ListObjectsResponse'
  { -- | All of the keys rolled up in a common prefix count as a single return when calculating the number of returns.
    --
    -- A response can contain CommonPrefixes only if you specify a delimiter.
    -- CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by the delimiter.
    -- CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix.
    -- For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/. All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
    commonPrefixes :: Core.Maybe [Types.CommonPrefix],
    -- | Metadata about each object returned.
    contents :: Core.Maybe [Types.Object],
    -- | Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the @CommonPrefixes@ collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
    delimiter :: Core.Maybe Types.Delimiter,
    -- | Encoding type used by Amazon S3 to encode object keys in the response.
    encodingType :: Core.Maybe Types.EncodingType,
    -- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria.
    isTruncated :: Core.Maybe Core.Bool,
    -- | Indicates where in the bucket listing begins. Marker is included in the response if it was sent with the request.
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of keys returned in the response body.
    maxKeys :: Core.Maybe Core.Int,
    -- | The bucket name.
    name :: Core.Maybe Types.Name,
    -- | When response is truncated (the IsTruncated element value in the response is true), you can use the key name in this field as marker in the subsequent request to get next set of objects. Amazon S3 lists objects in alphabetical order Note: This element is returned only if you have delimiter request parameter specified. If response does not include the NextMarker and it is truncated, you can use the value of the last Key in the response as the marker in the subsequent request to get the next set of object keys.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | Keys that begin with the indicated prefix.
    prefix :: Core.Maybe Types.Prefix,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListObjectsResponse' value with any optional fields omitted.
mkListObjectsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListObjectsResponse
mkListObjectsResponse responseStatus =
  ListObjectsResponse'
    { commonPrefixes = Core.Nothing,
      contents = Core.Nothing,
      delimiter = Core.Nothing,
      encodingType = Core.Nothing,
      isTruncated = Core.Nothing,
      marker = Core.Nothing,
      maxKeys = Core.Nothing,
      name = Core.Nothing,
      nextMarker = Core.Nothing,
      prefix = Core.Nothing,
      responseStatus
    }

-- | All of the keys rolled up in a common prefix count as a single return when calculating the number of returns.
--
-- A response can contain CommonPrefixes only if you specify a delimiter.
-- CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by the delimiter.
-- CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix.
-- For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/. All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsCommonPrefixes :: Lens.Lens' ListObjectsResponse (Core.Maybe [Types.CommonPrefix])
lorrsCommonPrefixes = Lens.field @"commonPrefixes"
{-# DEPRECATED lorrsCommonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead." #-}

-- | Metadata about each object returned.
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsContents :: Lens.Lens' ListObjectsResponse (Core.Maybe [Types.Object])
lorrsContents = Lens.field @"contents"
{-# DEPRECATED lorrsContents "Use generic-lens or generic-optics with 'contents' instead." #-}

-- | Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the @CommonPrefixes@ collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsDelimiter :: Lens.Lens' ListObjectsResponse (Core.Maybe Types.Delimiter)
lorrsDelimiter = Lens.field @"delimiter"
{-# DEPRECATED lorrsDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsEncodingType :: Lens.Lens' ListObjectsResponse (Core.Maybe Types.EncodingType)
lorrsEncodingType = Lens.field @"encodingType"
{-# DEPRECATED lorrsEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsIsTruncated :: Lens.Lens' ListObjectsResponse (Core.Maybe Core.Bool)
lorrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lorrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | Indicates where in the bucket listing begins. Marker is included in the response if it was sent with the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsMarker :: Lens.Lens' ListObjectsResponse (Core.Maybe Types.Marker)
lorrsMarker = Lens.field @"marker"
{-# DEPRECATED lorrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of keys returned in the response body.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsMaxKeys :: Lens.Lens' ListObjectsResponse (Core.Maybe Core.Int)
lorrsMaxKeys = Lens.field @"maxKeys"
{-# DEPRECATED lorrsMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsName :: Lens.Lens' ListObjectsResponse (Core.Maybe Types.Name)
lorrsName = Lens.field @"name"
{-# DEPRECATED lorrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When response is truncated (the IsTruncated element value in the response is true), you can use the key name in this field as marker in the subsequent request to get next set of objects. Amazon S3 lists objects in alphabetical order Note: This element is returned only if you have delimiter request parameter specified. If response does not include the NextMarker and it is truncated, you can use the value of the last Key in the response as the marker in the subsequent request to get the next set of object keys.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsNextMarker :: Lens.Lens' ListObjectsResponse (Core.Maybe Types.NextMarker)
lorrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lorrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | Keys that begin with the indicated prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsPrefix :: Lens.Lens' ListObjectsResponse (Core.Maybe Types.Prefix)
lorrsPrefix = Lens.field @"prefix"
{-# DEPRECATED lorrsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsResponseStatus :: Lens.Lens' ListObjectsResponse Core.Int
lorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
