{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- To use this operation, you must have READ access to the bucket.
-- This action is not supported by Amazon S3 on Outposts.
-- The following operations are related to @ListObjectVersions@ :
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
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjectVersions
  ( -- * Creating a request
    ListObjectVersions (..),
    mkListObjectVersions,

    -- ** Request lenses
    lBucket,
    lDelimiter,
    lEncodingType,
    lExpectedBucketOwner,
    lKeyMarker,
    lMaxKeys,
    lPrefix,
    lVersionIdMarker,

    -- * Destructuring the response
    ListObjectVersionsResponse (..),
    mkListObjectVersionsResponse,

    -- ** Response lenses
    lrsCommonPrefixes,
    lrsDeleteMarkers,
    lrsDelimiter,
    lrsEncodingType,
    lrsIsTruncated,
    lrsKeyMarker,
    lrsMaxKeys,
    lrsName,
    lrsNextKeyMarker,
    lrsNextVersionIdMarker,
    lrsPrefix,
    lrsVersionIdMarker,
    lrsVersions,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListObjectVersions' smart constructor.
data ListObjectVersions = ListObjectVersions'
  { -- | The bucket name that contains the objects.
    bucket :: Types.BucketName,
    -- | A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
    delimiter :: Core.Maybe Types.Delimiter,
    encodingType :: Core.Maybe Types.EncodingType,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    -- | Specifies the key to start with when listing objects in a bucket.
    keyMarker :: Core.Maybe Types.KeyMarker,
    -- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
    maxKeys :: Core.Maybe Core.Int,
    -- | Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
    prefix :: Core.Maybe Types.Prefix,
    -- | Specifies the object version you want to start listing from.
    versionIdMarker :: Core.Maybe Types.VersionIdMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectVersions' value with any optional fields omitted.
mkListObjectVersions ::
  -- | 'bucket'
  Types.BucketName ->
  ListObjectVersions
mkListObjectVersions bucket =
  ListObjectVersions'
    { bucket,
      delimiter = Core.Nothing,
      encodingType = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      keyMarker = Core.Nothing,
      maxKeys = Core.Nothing,
      prefix = Core.Nothing,
      versionIdMarker = Core.Nothing
    }

-- | The bucket name that contains the objects.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lBucket :: Lens.Lens' ListObjectVersions Types.BucketName
lBucket = Lens.field @"bucket"
{-# DEPRECATED lBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDelimiter :: Lens.Lens' ListObjectVersions (Core.Maybe Types.Delimiter)
lDelimiter = Lens.field @"delimiter"
{-# DEPRECATED lDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lEncodingType :: Lens.Lens' ListObjectVersions (Core.Maybe Types.EncodingType)
lEncodingType = Lens.field @"encodingType"
{-# DEPRECATED lEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lExpectedBucketOwner :: Lens.Lens' ListObjectVersions (Core.Maybe Types.ExpectedBucketOwner)
lExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED lExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Specifies the key to start with when listing objects in a bucket.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lKeyMarker :: Lens.Lens' ListObjectVersions (Core.Maybe Types.KeyMarker)
lKeyMarker = Lens.field @"keyMarker"
{-# DEPRECATED lKeyMarker "Use generic-lens or generic-optics with 'keyMarker' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxKeys :: Lens.Lens' ListObjectVersions (Core.Maybe Core.Int)
lMaxKeys = Lens.field @"maxKeys"
{-# DEPRECATED lMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPrefix :: Lens.Lens' ListObjectVersions (Core.Maybe Types.Prefix)
lPrefix = Lens.field @"prefix"
{-# DEPRECATED lPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Specifies the object version you want to start listing from.
--
-- /Note:/ Consider using 'versionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lVersionIdMarker :: Lens.Lens' ListObjectVersions (Core.Maybe Types.VersionIdMarker)
lVersionIdMarker = Lens.field @"versionIdMarker"
{-# DEPRECATED lVersionIdMarker "Use generic-lens or generic-optics with 'versionIdMarker' instead." #-}

instance Core.AWSRequest ListObjectVersions where
  type Rs ListObjectVersions = ListObjectVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "delimiter" Core.<$> delimiter
            Core.<> (Core.toQueryValue "encoding-type" Core.<$> encodingType)
            Core.<> (Core.toQueryValue "key-marker" Core.<$> keyMarker)
            Core.<> (Core.toQueryValue "max-keys" Core.<$> maxKeys)
            Core.<> (Core.toQueryValue "prefix" Core.<$> prefix)
            Core.<> (Core.toQueryValue "version-id-marker" Core.<$> versionIdMarker)
            Core.<> (Core.pure ("versions", "")),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListObjectVersionsResponse'
            Core.<$> (x Core..@? "CommonPrefixes")
            Core.<*> (x Core..@? "DeleteMarker")
            Core.<*> (x Core..@? "Delimiter")
            Core.<*> (x Core..@? "EncodingType")
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "KeyMarker")
            Core.<*> (x Core..@? "MaxKeys")
            Core.<*> (x Core..@? "Name")
            Core.<*> (x Core..@? "NextKeyMarker")
            Core.<*> (x Core..@? "NextVersionIdMarker")
            Core.<*> (x Core..@? "Prefix")
            Core.<*> (x Core..@? "VersionIdMarker")
            Core.<*> (x Core..@? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListObjectVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
    | Core.isNothing (rs Lens.^. Lens.field @"nextKeyMarker")
        Core.&& Core.isNothing (rs Lens.^. Lens.field @"nextVersionIdMarker") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"keyMarker"
            Lens..~ rs Lens.^. Lens.field @"nextKeyMarker"
            Core.& Lens.field @"versionIdMarker"
            Lens..~ rs Lens.^. Lens.field @"nextVersionIdMarker"
        )

-- | /See:/ 'mkListObjectVersionsResponse' smart constructor.
data ListObjectVersionsResponse = ListObjectVersionsResponse'
  { -- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
    commonPrefixes :: Core.Maybe [Types.CommonPrefix],
    -- | Container for an object that is a delete marker.
    deleteMarkers :: Core.Maybe [Types.DeleteMarkerEntry],
    -- | The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
    delimiter :: Core.Maybe Types.Delimiter,
    -- | Encoding type used by Amazon S3 to encode object key names in the XML response.
    --
    -- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
    -- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
    encodingType :: Core.Maybe Types.EncodingType,
    -- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | Marks the last key returned in a truncated response.
    keyMarker :: Core.Maybe Types.KeyMarker,
    -- | Specifies the maximum number of objects to return.
    maxKeys :: Core.Maybe Core.Int,
    -- | The bucket name.
    name :: Core.Maybe Types.Name,
    -- | When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
    nextKeyMarker :: Core.Maybe Types.NextKeyMarker,
    -- | When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
    nextVersionIdMarker :: Core.Maybe Types.NextVersionIdMarker,
    -- | Selects objects that start with the value supplied by this parameter.
    prefix :: Core.Maybe Types.Prefix,
    -- | Marks the last version of the key returned in a truncated response.
    versionIdMarker :: Core.Maybe Types.VersionIdMarker,
    -- | Container for version information.
    versions :: Core.Maybe [Types.ObjectVersion],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListObjectVersionsResponse' value with any optional fields omitted.
mkListObjectVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListObjectVersionsResponse
mkListObjectVersionsResponse responseStatus =
  ListObjectVersionsResponse'
    { commonPrefixes = Core.Nothing,
      deleteMarkers = Core.Nothing,
      delimiter = Core.Nothing,
      encodingType = Core.Nothing,
      isTruncated = Core.Nothing,
      keyMarker = Core.Nothing,
      maxKeys = Core.Nothing,
      name = Core.Nothing,
      nextKeyMarker = Core.Nothing,
      nextVersionIdMarker = Core.Nothing,
      prefix = Core.Nothing,
      versionIdMarker = Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsCommonPrefixes :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe [Types.CommonPrefix])
lrsCommonPrefixes = Lens.field @"commonPrefixes"
{-# DEPRECATED lrsCommonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead." #-}

-- | Container for an object that is a delete marker.
--
-- /Note:/ Consider using 'deleteMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDeleteMarkers :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe [Types.DeleteMarkerEntry])
lrsDeleteMarkers = Lens.field @"deleteMarkers"
{-# DEPRECATED lrsDeleteMarkers "Use generic-lens or generic-optics with 'deleteMarkers' instead." #-}

-- | The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDelimiter :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.Delimiter)
lrsDelimiter = Lens.field @"delimiter"
{-# DEPRECATED lrsDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsEncodingType :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.EncodingType)
lrsEncodingType = Lens.field @"encodingType"
{-# DEPRECATED lrsEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsIsTruncated :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Core.Bool)
lrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | Marks the last key returned in a truncated response.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.KeyMarker)
lrsKeyMarker = Lens.field @"keyMarker"
{-# DEPRECATED lrsKeyMarker "Use generic-lens or generic-optics with 'keyMarker' instead." #-}

-- | Specifies the maximum number of objects to return.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsMaxKeys :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Core.Int)
lrsMaxKeys = Lens.field @"maxKeys"
{-# DEPRECATED lrsMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsName :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.Name)
lrsName = Lens.field @"name"
{-# DEPRECATED lrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextKeyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.NextKeyMarker)
lrsNextKeyMarker = Lens.field @"nextKeyMarker"
{-# DEPRECATED lrsNextKeyMarker "Use generic-lens or generic-optics with 'nextKeyMarker' instead." #-}

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextVersionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.NextVersionIdMarker)
lrsNextVersionIdMarker = Lens.field @"nextVersionIdMarker"
{-# DEPRECATED lrsNextVersionIdMarker "Use generic-lens or generic-optics with 'nextVersionIdMarker' instead." #-}

-- | Selects objects that start with the value supplied by this parameter.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsPrefix :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.Prefix)
lrsPrefix = Lens.field @"prefix"
{-# DEPRECATED lrsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Marks the last version of the key returned in a truncated response.
--
-- /Note:/ Consider using 'versionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.VersionIdMarker)
lrsVersionIdMarker = Lens.field @"versionIdMarker"
{-# DEPRECATED lrsVersionIdMarker "Use generic-lens or generic-optics with 'versionIdMarker' instead." #-}

-- | Container for version information.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsVersions :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe [Types.ObjectVersion])
lrsVersions = Lens.field @"versions"
{-# DEPRECATED lrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListObjectVersionsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
