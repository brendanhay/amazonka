{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListMultipartUploads
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists in-progress multipart uploads. An in-progress multipart upload is a multipart upload that has been initiated using the Initiate Multipart Upload request, but has not yet been completed or aborted.
--
-- This operation returns at most 1,000 multipart uploads in the response. 1,000 multipart uploads is the maximum number of uploads a response can include, which is also the default value. You can further limit the number of uploads in a response by specifying the @max-uploads@ parameter in the response. If additional multipart uploads satisfy the list criteria, the response will contain an @IsTruncated@ element with the value true. To list the additional multipart uploads, use the @key-marker@ and @upload-id-marker@ request parameters.
-- In the response, the uploads are sorted by key. If your application has initiated more than one multipart upload using the same object key, then uploads in the response are first sorted by key. Additionally, uploads are sorted in ascending order within each key by the upload initiation time.
-- For more information on multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> .
-- For information on permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
-- The following operations are related to @ListMultipartUploads@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload> 
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListMultipartUploads
    (
    -- * Creating a request
      ListMultipartUploads (..)
    , mkListMultipartUploads
    -- ** Request lenses
    , lmuBucket
    , lmuDelimiter
    , lmuEncodingType
    , lmuExpectedBucketOwner
    , lmuKeyMarker
    , lmuMaxUploads
    , lmuPrefix
    , lmuUploadIdMarker

    -- * Destructuring the response
    , ListMultipartUploadsResponse (..)
    , mkListMultipartUploadsResponse
    -- ** Response lenses
    , lmurrsBucket
    , lmurrsCommonPrefixes
    , lmurrsDelimiter
    , lmurrsEncodingType
    , lmurrsIsTruncated
    , lmurrsKeyMarker
    , lmurrsMaxUploads
    , lmurrsNextKeyMarker
    , lmurrsNextUploadIdMarker
    , lmurrsPrefix
    , lmurrsUploadIdMarker
    , lmurrsUploads
    , lmurrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket to which the multipart upload was initiated. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
  , delimiter :: Core.Maybe Types.Delimiter
    -- ^ Character you use to group keys.
--
-- All keys that contain the same string between the prefix, if specified, and the first occurrence of the delimiter after the prefix are grouped under a single result element, @CommonPrefixes@ . If you don't specify the prefix parameter, then the substring starts at the beginning of the key. The keys that are grouped under @CommonPrefixes@ result element are not returned elsewhere in the response.
  , encodingType :: Core.Maybe Types.EncodingType
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , keyMarker :: Core.Maybe Types.KeyMarker
    -- ^ Together with upload-id-marker, this parameter specifies the multipart upload after which listing should begin.
--
-- If @upload-id-marker@ is not specified, only the keys lexicographically greater than the specified @key-marker@ will be included in the list.
-- If @upload-id-marker@ is specified, any multipart uploads for a key equal to the @key-marker@ might also be included, provided those multipart uploads have upload IDs lexicographically greater than the specified @upload-id-marker@ .
  , maxUploads :: Core.Maybe Core.Int
    -- ^ Sets the maximum number of multipart uploads, from 1 to 1,000, to return in the response body. 1,000 is the maximum number of uploads that can be returned in a response.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ Lists in-progress uploads only for those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different grouping of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.)
  , uploadIdMarker :: Core.Maybe Types.UploadIdMarker
    -- ^ Together with key-marker, specifies the multipart upload after which listing should begin. If key-marker is not specified, the upload-id-marker parameter is ignored. Otherwise, any multipart uploads for a key equal to the key-marker might be included in the list only if they have an upload ID lexicographically greater than the specified @upload-id-marker@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMultipartUploads' value with any optional fields omitted.
mkListMultipartUploads
    :: Types.BucketName -- ^ 'bucket'
    -> ListMultipartUploads
mkListMultipartUploads bucket
  = ListMultipartUploads'{bucket, delimiter = Core.Nothing,
                          encodingType = Core.Nothing, expectedBucketOwner = Core.Nothing,
                          keyMarker = Core.Nothing, maxUploads = Core.Nothing,
                          prefix = Core.Nothing, uploadIdMarker = Core.Nothing}

-- | The name of the bucket to which the multipart upload was initiated. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuBucket :: Lens.Lens' ListMultipartUploads Types.BucketName
lmuBucket = Lens.field @"bucket"
{-# INLINEABLE lmuBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Character you use to group keys.
--
-- All keys that contain the same string between the prefix, if specified, and the first occurrence of the delimiter after the prefix are grouped under a single result element, @CommonPrefixes@ . If you don't specify the prefix parameter, then the substring starts at the beginning of the key. The keys that are grouped under @CommonPrefixes@ result element are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuDelimiter :: Lens.Lens' ListMultipartUploads (Core.Maybe Types.Delimiter)
lmuDelimiter = Lens.field @"delimiter"
{-# INLINEABLE lmuDelimiter #-}
{-# DEPRECATED delimiter "Use generic-lens or generic-optics with 'delimiter' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuEncodingType :: Lens.Lens' ListMultipartUploads (Core.Maybe Types.EncodingType)
lmuEncodingType = Lens.field @"encodingType"
{-# INLINEABLE lmuEncodingType #-}
{-# DEPRECATED encodingType "Use generic-lens or generic-optics with 'encodingType' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuExpectedBucketOwner :: Lens.Lens' ListMultipartUploads (Core.Maybe Types.ExpectedBucketOwner)
lmuExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE lmuExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Together with upload-id-marker, this parameter specifies the multipart upload after which listing should begin.
--
-- If @upload-id-marker@ is not specified, only the keys lexicographically greater than the specified @key-marker@ will be included in the list.
-- If @upload-id-marker@ is specified, any multipart uploads for a key equal to the @key-marker@ might also be included, provided those multipart uploads have upload IDs lexicographically greater than the specified @upload-id-marker@ .
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuKeyMarker :: Lens.Lens' ListMultipartUploads (Core.Maybe Types.KeyMarker)
lmuKeyMarker = Lens.field @"keyMarker"
{-# INLINEABLE lmuKeyMarker #-}
{-# DEPRECATED keyMarker "Use generic-lens or generic-optics with 'keyMarker' instead"  #-}

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return in the response body. 1,000 is the maximum number of uploads that can be returned in a response.
--
-- /Note:/ Consider using 'maxUploads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuMaxUploads :: Lens.Lens' ListMultipartUploads (Core.Maybe Core.Int)
lmuMaxUploads = Lens.field @"maxUploads"
{-# INLINEABLE lmuMaxUploads #-}
{-# DEPRECATED maxUploads "Use generic-lens or generic-optics with 'maxUploads' instead"  #-}

-- | Lists in-progress uploads only for those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different grouping of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.)
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuPrefix :: Lens.Lens' ListMultipartUploads (Core.Maybe Types.Prefix)
lmuPrefix = Lens.field @"prefix"
{-# INLINEABLE lmuPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | Together with key-marker, specifies the multipart upload after which listing should begin. If key-marker is not specified, the upload-id-marker parameter is ignored. Otherwise, any multipart uploads for a key equal to the key-marker might be included in the list only if they have an upload ID lexicographically greater than the specified @upload-id-marker@ .
--
-- /Note:/ Consider using 'uploadIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuUploadIdMarker :: Lens.Lens' ListMultipartUploads (Core.Maybe Types.UploadIdMarker)
lmuUploadIdMarker = Lens.field @"uploadIdMarker"
{-# INLINEABLE lmuUploadIdMarker #-}
{-# DEPRECATED uploadIdMarker "Use generic-lens or generic-optics with 'uploadIdMarker' instead"  #-}

instance Core.ToQuery ListMultipartUploads where
        toQuery ListMultipartUploads{..}
          = Core.maybe Core.mempty (Core.toQueryPair "delimiter") delimiter
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "encoding-type")
                encodingType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "key-marker") keyMarker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "max-uploads") maxUploads
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "prefix") prefix
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "upload-id-marker")
                uploadIdMarker
              Core.<> Core.toQueryPair "uploads" ("" :: Core.Text)

instance Core.ToHeaders ListMultipartUploads where
        toHeaders ListMultipartUploads{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest ListMultipartUploads where
        type Rs ListMultipartUploads = ListMultipartUploadsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListMultipartUploadsResponse' Core.<$>
                   (x Core..@? "Bucket") Core.<*> x Core..@? "CommonPrefixes" Core.<*>
                     x Core..@? "Delimiter"
                     Core.<*> x Core..@? "EncodingType"
                     Core.<*> x Core..@? "IsTruncated"
                     Core.<*> x Core..@? "KeyMarker"
                     Core.<*> x Core..@? "MaxUploads"
                     Core.<*> x Core..@? "NextKeyMarker"
                     Core.<*> x Core..@? "NextUploadIdMarker"
                     Core.<*> x Core..@? "Prefix"
                     Core.<*> x Core..@? "UploadIdMarker"
                     Core.<*> x Core..@? "Upload"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListMultipartUploads where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"nextKeyMarker") Core.&&
              Core.isNothing (rs Lens.^. Lens.field @"nextUploadIdMarker")
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"keyMarker" Lens..~
                   rs Lens.^. Lens.field @"nextKeyMarker"
                 Core.&
                 Lens.field @"uploadIdMarker" Lens..~
                   rs Lens.^. Lens.field @"nextUploadIdMarker")

-- | /See:/ 'mkListMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { bucket :: Core.Maybe Types.BucketName
    -- ^ The name of the bucket to which the multipart upload was initiated.
  , commonPrefixes :: Core.Maybe [Types.CommonPrefix]
    -- ^ If you specify a delimiter in the request, then the result returns each distinct key prefix containing the delimiter in a @CommonPrefixes@ element. The distinct key prefixes are returned in the @Prefix@ child element.
  , delimiter :: Core.Maybe Types.Delimiter
    -- ^ Contains the delimiter you specified in the request. If you don't specify a delimiter in your request, this element is absent from the response.
  , encodingType :: Core.Maybe Types.EncodingType
    -- ^ Encoding type used by Amazon S3 to encode object keys in the response.
--
-- If you specify @encoding-type@ request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @Delimiter@ , @KeyMarker@ , @Prefix@ , @NextKeyMarker@ , @Key@ .
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ Indicates whether the returned list of multipart uploads is truncated. A value of true indicates that the list was truncated. The list can be truncated if the number of multipart uploads exceeds the limit allowed or specified by max uploads.
  , keyMarker :: Core.Maybe Types.KeyMarker
    -- ^ The key at or after which the listing began.
  , maxUploads :: Core.Maybe Core.Int
    -- ^ Maximum number of multipart uploads that could have been included in the response.
  , nextKeyMarker :: Core.Maybe Types.NextKeyMarker
    -- ^ When a list is truncated, this element specifies the value that should be used for the key-marker request parameter in a subsequent request.
  , nextUploadIdMarker :: Core.Maybe Types.NextUploadIdMarker
    -- ^ When a list is truncated, this element specifies the value that should be used for the @upload-id-marker@ request parameter in a subsequent request.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ When a prefix is provided in the request, this field contains the specified prefix. The result contains only keys starting with the specified prefix.
  , uploadIdMarker :: Core.Maybe Types.UploadIdMarker
    -- ^ Upload ID after which listing began.
  , uploads :: Core.Maybe [Types.MultipartUpload]
    -- ^ Container for elements related to a particular multipart upload. A response can contain zero or more @Upload@ elements.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListMultipartUploadsResponse' value with any optional fields omitted.
mkListMultipartUploadsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListMultipartUploadsResponse
mkListMultipartUploadsResponse responseStatus
  = ListMultipartUploadsResponse'{bucket = Core.Nothing,
                                  commonPrefixes = Core.Nothing, delimiter = Core.Nothing,
                                  encodingType = Core.Nothing, isTruncated = Core.Nothing,
                                  keyMarker = Core.Nothing, maxUploads = Core.Nothing,
                                  nextKeyMarker = Core.Nothing, nextUploadIdMarker = Core.Nothing,
                                  prefix = Core.Nothing, uploadIdMarker = Core.Nothing,
                                  uploads = Core.Nothing, responseStatus}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsBucket :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Types.BucketName)
lmurrsBucket = Lens.field @"bucket"
{-# INLINEABLE lmurrsBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | If you specify a delimiter in the request, then the result returns each distinct key prefix containing the delimiter in a @CommonPrefixes@ element. The distinct key prefixes are returned in the @Prefix@ child element.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsCommonPrefixes :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe [Types.CommonPrefix])
lmurrsCommonPrefixes = Lens.field @"commonPrefixes"
{-# INLINEABLE lmurrsCommonPrefixes #-}
{-# DEPRECATED commonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead"  #-}

-- | Contains the delimiter you specified in the request. If you don't specify a delimiter in your request, this element is absent from the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsDelimiter :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Types.Delimiter)
lmurrsDelimiter = Lens.field @"delimiter"
{-# INLINEABLE lmurrsDelimiter #-}
{-# DEPRECATED delimiter "Use generic-lens or generic-optics with 'delimiter' instead"  #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
--
-- If you specify @encoding-type@ request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @Delimiter@ , @KeyMarker@ , @Prefix@ , @NextKeyMarker@ , @Key@ .
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsEncodingType :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Types.EncodingType)
lmurrsEncodingType = Lens.field @"encodingType"
{-# INLINEABLE lmurrsEncodingType #-}
{-# DEPRECATED encodingType "Use generic-lens or generic-optics with 'encodingType' instead"  #-}

-- | Indicates whether the returned list of multipart uploads is truncated. A value of true indicates that the list was truncated. The list can be truncated if the number of multipart uploads exceeds the limit allowed or specified by max uploads.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsIsTruncated :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Core.Bool)
lmurrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE lmurrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | The key at or after which the listing began.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsKeyMarker :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Types.KeyMarker)
lmurrsKeyMarker = Lens.field @"keyMarker"
{-# INLINEABLE lmurrsKeyMarker #-}
{-# DEPRECATED keyMarker "Use generic-lens or generic-optics with 'keyMarker' instead"  #-}

-- | Maximum number of multipart uploads that could have been included in the response.
--
-- /Note:/ Consider using 'maxUploads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsMaxUploads :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Core.Int)
lmurrsMaxUploads = Lens.field @"maxUploads"
{-# INLINEABLE lmurrsMaxUploads #-}
{-# DEPRECATED maxUploads "Use generic-lens or generic-optics with 'maxUploads' instead"  #-}

-- | When a list is truncated, this element specifies the value that should be used for the key-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextKeyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsNextKeyMarker :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Types.NextKeyMarker)
lmurrsNextKeyMarker = Lens.field @"nextKeyMarker"
{-# INLINEABLE lmurrsNextKeyMarker #-}
{-# DEPRECATED nextKeyMarker "Use generic-lens or generic-optics with 'nextKeyMarker' instead"  #-}

-- | When a list is truncated, this element specifies the value that should be used for the @upload-id-marker@ request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextUploadIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsNextUploadIdMarker :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Types.NextUploadIdMarker)
lmurrsNextUploadIdMarker = Lens.field @"nextUploadIdMarker"
{-# INLINEABLE lmurrsNextUploadIdMarker #-}
{-# DEPRECATED nextUploadIdMarker "Use generic-lens or generic-optics with 'nextUploadIdMarker' instead"  #-}

-- | When a prefix is provided in the request, this field contains the specified prefix. The result contains only keys starting with the specified prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsPrefix :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Types.Prefix)
lmurrsPrefix = Lens.field @"prefix"
{-# INLINEABLE lmurrsPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | Upload ID after which listing began.
--
-- /Note:/ Consider using 'uploadIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsUploadIdMarker :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Types.UploadIdMarker)
lmurrsUploadIdMarker = Lens.field @"uploadIdMarker"
{-# INLINEABLE lmurrsUploadIdMarker #-}
{-# DEPRECATED uploadIdMarker "Use generic-lens or generic-optics with 'uploadIdMarker' instead"  #-}

-- | Container for elements related to a particular multipart upload. A response can contain zero or more @Upload@ elements.
--
-- /Note:/ Consider using 'uploads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsUploads :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe [Types.MultipartUpload])
lmurrsUploads = Lens.field @"uploads"
{-# INLINEABLE lmurrsUploads #-}
{-# DEPRECATED uploads "Use generic-lens or generic-optics with 'uploads' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsResponseStatus :: Lens.Lens' ListMultipartUploadsResponse Core.Int
lmurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
