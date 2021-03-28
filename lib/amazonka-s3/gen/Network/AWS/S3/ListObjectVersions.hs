{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListObjectVersions (..)
    , mkListObjectVersions
    -- ** Request lenses
    , lBucket
    , lDelimiter
    , lEncodingType
    , lExpectedBucketOwner
    , lKeyMarker
    , lMaxKeys
    , lPrefix
    , lVersionIdMarker

    -- * Destructuring the response
    , ListObjectVersionsResponse (..)
    , mkListObjectVersionsResponse
    -- ** Response lenses
    , lrsCommonPrefixes
    , lrsDeleteMarkers
    , lrsDelimiter
    , lrsEncodingType
    , lrsIsTruncated
    , lrsKeyMarker
    , lrsMaxKeys
    , lrsName
    , lrsNextKeyMarker
    , lrsNextVersionIdMarker
    , lrsPrefix
    , lrsVersionIdMarker
    , lrsVersions
    , lrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkListObjectVersions' smart constructor.
data ListObjectVersions = ListObjectVersions'
  { bucket :: Types.BucketName
    -- ^ The bucket name that contains the objects. 
  , delimiter :: Core.Maybe Types.Delimiter
    -- ^ A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
  , encodingType :: Core.Maybe Types.EncodingType
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , keyMarker :: Core.Maybe Types.KeyMarker
    -- ^ Specifies the key to start with when listing objects in a bucket.
  , maxKeys :: Core.Maybe Core.Int
    -- ^ Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes. 
  , versionIdMarker :: Core.Maybe Types.VersionIdMarker
    -- ^ Specifies the object version you want to start listing from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectVersions' value with any optional fields omitted.
mkListObjectVersions
    :: Types.BucketName -- ^ 'bucket'
    -> ListObjectVersions
mkListObjectVersions bucket
  = ListObjectVersions'{bucket, delimiter = Core.Nothing,
                        encodingType = Core.Nothing, expectedBucketOwner = Core.Nothing,
                        keyMarker = Core.Nothing, maxKeys = Core.Nothing,
                        prefix = Core.Nothing, versionIdMarker = Core.Nothing}

-- | The bucket name that contains the objects. 
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lBucket :: Lens.Lens' ListObjectVersions Types.BucketName
lBucket = Lens.field @"bucket"
{-# INLINEABLE lBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDelimiter :: Lens.Lens' ListObjectVersions (Core.Maybe Types.Delimiter)
lDelimiter = Lens.field @"delimiter"
{-# INLINEABLE lDelimiter #-}
{-# DEPRECATED delimiter "Use generic-lens or generic-optics with 'delimiter' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lEncodingType :: Lens.Lens' ListObjectVersions (Core.Maybe Types.EncodingType)
lEncodingType = Lens.field @"encodingType"
{-# INLINEABLE lEncodingType #-}
{-# DEPRECATED encodingType "Use generic-lens or generic-optics with 'encodingType' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lExpectedBucketOwner :: Lens.Lens' ListObjectVersions (Core.Maybe Types.ExpectedBucketOwner)
lExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE lExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Specifies the key to start with when listing objects in a bucket.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lKeyMarker :: Lens.Lens' ListObjectVersions (Core.Maybe Types.KeyMarker)
lKeyMarker = Lens.field @"keyMarker"
{-# INLINEABLE lKeyMarker #-}
{-# DEPRECATED keyMarker "Use generic-lens or generic-optics with 'keyMarker' instead"  #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxKeys :: Lens.Lens' ListObjectVersions (Core.Maybe Core.Int)
lMaxKeys = Lens.field @"maxKeys"
{-# INLINEABLE lMaxKeys #-}
{-# DEPRECATED maxKeys "Use generic-lens or generic-optics with 'maxKeys' instead"  #-}

-- | Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes. 
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPrefix :: Lens.Lens' ListObjectVersions (Core.Maybe Types.Prefix)
lPrefix = Lens.field @"prefix"
{-# INLINEABLE lPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | Specifies the object version you want to start listing from.
--
-- /Note:/ Consider using 'versionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lVersionIdMarker :: Lens.Lens' ListObjectVersions (Core.Maybe Types.VersionIdMarker)
lVersionIdMarker = Lens.field @"versionIdMarker"
{-# INLINEABLE lVersionIdMarker #-}
{-# DEPRECATED versionIdMarker "Use generic-lens or generic-optics with 'versionIdMarker' instead"  #-}

instance Core.ToQuery ListObjectVersions where
        toQuery ListObjectVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "delimiter") delimiter
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "encoding-type")
                encodingType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "key-marker") keyMarker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "max-keys") maxKeys
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "prefix") prefix
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "version-id-marker")
                versionIdMarker
              Core.<> Core.toQueryPair "versions" ("" :: Core.Text)

instance Core.ToHeaders ListObjectVersions where
        toHeaders ListObjectVersions{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest ListObjectVersions where
        type Rs ListObjectVersions = ListObjectVersionsResponse
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
                 ListObjectVersionsResponse' Core.<$>
                   (x Core..@? "CommonPrefixes") Core.<*> x Core..@? "DeleteMarker"
                     Core.<*> x Core..@? "Delimiter"
                     Core.<*> x Core..@? "EncodingType"
                     Core.<*> x Core..@? "IsTruncated"
                     Core.<*> x Core..@? "KeyMarker"
                     Core.<*> x Core..@? "MaxKeys"
                     Core.<*> x Core..@? "Name"
                     Core.<*> x Core..@? "NextKeyMarker"
                     Core.<*> x Core..@? "NextVersionIdMarker"
                     Core.<*> x Core..@? "Prefix"
                     Core.<*> x Core..@? "VersionIdMarker"
                     Core.<*> x Core..@? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListObjectVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"nextKeyMarker") Core.&&
              Core.isNothing (rs Lens.^. Lens.field @"nextVersionIdMarker")
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"keyMarker" Lens..~
                   rs Lens.^. Lens.field @"nextKeyMarker"
                 Core.&
                 Lens.field @"versionIdMarker" Lens..~
                   rs Lens.^. Lens.field @"nextVersionIdMarker")

-- | /See:/ 'mkListObjectVersionsResponse' smart constructor.
data ListObjectVersionsResponse = ListObjectVersionsResponse'
  { commonPrefixes :: Core.Maybe [Types.CommonPrefix]
    -- ^ All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
  , deleteMarkers :: Core.Maybe [Types.DeleteMarkerEntry]
    -- ^ Container for an object that is a delete marker.
  , delimiter :: Core.Maybe Types.Delimiter
    -- ^ The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
  , encodingType :: Core.Maybe Types.EncodingType
    -- ^ Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
  , isTruncated :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
  , keyMarker :: Core.Maybe Types.KeyMarker
    -- ^ Marks the last key returned in a truncated response.
  , maxKeys :: Core.Maybe Core.Int
    -- ^ Specifies the maximum number of objects to return.
  , name :: Core.Maybe Types.Name
    -- ^ The bucket name.
  , nextKeyMarker :: Core.Maybe Types.NextKeyMarker
    -- ^ When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
  , nextVersionIdMarker :: Core.Maybe Types.NextVersionIdMarker
    -- ^ When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ Selects objects that start with the value supplied by this parameter.
  , versionIdMarker :: Core.Maybe Types.VersionIdMarker
    -- ^ Marks the last version of the key returned in a truncated response.
  , versions :: Core.Maybe [Types.ObjectVersion]
    -- ^ Container for version information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListObjectVersionsResponse' value with any optional fields omitted.
mkListObjectVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListObjectVersionsResponse
mkListObjectVersionsResponse responseStatus
  = ListObjectVersionsResponse'{commonPrefixes = Core.Nothing,
                                deleteMarkers = Core.Nothing, delimiter = Core.Nothing,
                                encodingType = Core.Nothing, isTruncated = Core.Nothing,
                                keyMarker = Core.Nothing, maxKeys = Core.Nothing,
                                name = Core.Nothing, nextKeyMarker = Core.Nothing,
                                nextVersionIdMarker = Core.Nothing, prefix = Core.Nothing,
                                versionIdMarker = Core.Nothing, versions = Core.Nothing,
                                responseStatus}

-- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsCommonPrefixes :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe [Types.CommonPrefix])
lrsCommonPrefixes = Lens.field @"commonPrefixes"
{-# INLINEABLE lrsCommonPrefixes #-}
{-# DEPRECATED commonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead"  #-}

-- | Container for an object that is a delete marker.
--
-- /Note:/ Consider using 'deleteMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDeleteMarkers :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe [Types.DeleteMarkerEntry])
lrsDeleteMarkers = Lens.field @"deleteMarkers"
{-# INLINEABLE lrsDeleteMarkers #-}
{-# DEPRECATED deleteMarkers "Use generic-lens or generic-optics with 'deleteMarkers' instead"  #-}

-- | The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDelimiter :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.Delimiter)
lrsDelimiter = Lens.field @"delimiter"
{-# INLINEABLE lrsDelimiter #-}
{-# DEPRECATED delimiter "Use generic-lens or generic-optics with 'delimiter' instead"  #-}

-- | Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsEncodingType :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.EncodingType)
lrsEncodingType = Lens.field @"encodingType"
{-# INLINEABLE lrsEncodingType #-}
{-# DEPRECATED encodingType "Use generic-lens or generic-optics with 'encodingType' instead"  #-}

-- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsIsTruncated :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Core.Bool)
lrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE lrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | Marks the last key returned in a truncated response.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.KeyMarker)
lrsKeyMarker = Lens.field @"keyMarker"
{-# INLINEABLE lrsKeyMarker #-}
{-# DEPRECATED keyMarker "Use generic-lens or generic-optics with 'keyMarker' instead"  #-}

-- | Specifies the maximum number of objects to return.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsMaxKeys :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Core.Int)
lrsMaxKeys = Lens.field @"maxKeys"
{-# INLINEABLE lrsMaxKeys #-}
{-# DEPRECATED maxKeys "Use generic-lens or generic-optics with 'maxKeys' instead"  #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsName :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.Name)
lrsName = Lens.field @"name"
{-# INLINEABLE lrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextKeyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.NextKeyMarker)
lrsNextKeyMarker = Lens.field @"nextKeyMarker"
{-# INLINEABLE lrsNextKeyMarker #-}
{-# DEPRECATED nextKeyMarker "Use generic-lens or generic-optics with 'nextKeyMarker' instead"  #-}

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextVersionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.NextVersionIdMarker)
lrsNextVersionIdMarker = Lens.field @"nextVersionIdMarker"
{-# INLINEABLE lrsNextVersionIdMarker #-}
{-# DEPRECATED nextVersionIdMarker "Use generic-lens or generic-optics with 'nextVersionIdMarker' instead"  #-}

-- | Selects objects that start with the value supplied by this parameter.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsPrefix :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.Prefix)
lrsPrefix = Lens.field @"prefix"
{-# INLINEABLE lrsPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | Marks the last version of the key returned in a truncated response.
--
-- /Note:/ Consider using 'versionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe Types.VersionIdMarker)
lrsVersionIdMarker = Lens.field @"versionIdMarker"
{-# INLINEABLE lrsVersionIdMarker #-}
{-# DEPRECATED versionIdMarker "Use generic-lens or generic-optics with 'versionIdMarker' instead"  #-}

-- | Container for version information.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsVersions :: Lens.Lens' ListObjectVersionsResponse (Core.Maybe [Types.ObjectVersion])
lrsVersions = Lens.field @"versions"
{-# INLINEABLE lrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListObjectVersionsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
