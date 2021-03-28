{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObjectTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the entire tag set from the specified object. For more information about managing object tags, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging> .
--
-- To use this operation, you must have permission to perform the @s3:DeleteObjectTagging@ action.
-- To delete tags of a specific object version, add the @versionId@ query parameter in the request. You will need permission for the @s3:DeleteObjectVersionTagging@ action.
-- The following operations are related to @DeleteBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectTagging.html PutObjectTagging> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging> 
--
--
module Network.AWS.S3.DeleteObjectTagging
    (
    -- * Creating a request
      DeleteObjectTagging (..)
    , mkDeleteObjectTagging
    -- ** Request lenses
    , dotBucket
    , dotKey
    , dotExpectedBucketOwner
    , dotVersionId

    -- * Destructuring the response
    , DeleteObjectTaggingResponse (..)
    , mkDeleteObjectTaggingResponse
    -- ** Response lenses
    , dotrrsVersionId
    , dotrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteObjectTagging' smart constructor.
data DeleteObjectTagging = DeleteObjectTagging'
  { bucket :: Types.BucketName
    -- ^ The bucket name containing the objects from which to remove the tags. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.ObjectKey
    -- ^ Name of the object key.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ The versionId of the object that the tag-set will be removed from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteObjectTagging' value with any optional fields omitted.
mkDeleteObjectTagging
    :: Types.BucketName -- ^ 'bucket'
    -> Types.ObjectKey -- ^ 'key'
    -> DeleteObjectTagging
mkDeleteObjectTagging bucket key
  = DeleteObjectTagging'{bucket, key,
                         expectedBucketOwner = Core.Nothing, versionId = Core.Nothing}

-- | The bucket name containing the objects from which to remove the tags. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotBucket :: Lens.Lens' DeleteObjectTagging Types.BucketName
dotBucket = Lens.field @"bucket"
{-# INLINEABLE dotBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Name of the object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotKey :: Lens.Lens' DeleteObjectTagging Types.ObjectKey
dotKey = Lens.field @"key"
{-# INLINEABLE dotKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotExpectedBucketOwner :: Lens.Lens' DeleteObjectTagging (Core.Maybe Types.AccountId)
dotExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE dotExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | The versionId of the object that the tag-set will be removed from.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotVersionId :: Lens.Lens' DeleteObjectTagging (Core.Maybe Types.ObjectVersionId)
dotVersionId = Lens.field @"versionId"
{-# INLINEABLE dotVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery DeleteObjectTagging where
        toQuery DeleteObjectTagging{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId
              Core.<> Core.toQueryPair "tagging" ("" :: Core.Text)

instance Core.ToHeaders DeleteObjectTagging where
        toHeaders DeleteObjectTagging{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest DeleteObjectTagging where
        type Rs DeleteObjectTagging = DeleteObjectTaggingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteObjectTaggingResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-version-id" h) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteObjectTaggingResponse' smart constructor.
data DeleteObjectTaggingResponse = DeleteObjectTaggingResponse'
  { versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ The versionId of the object the tag-set was removed from.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteObjectTaggingResponse' value with any optional fields omitted.
mkDeleteObjectTaggingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteObjectTaggingResponse
mkDeleteObjectTaggingResponse responseStatus
  = DeleteObjectTaggingResponse'{versionId = Core.Nothing,
                                 responseStatus}

-- | The versionId of the object the tag-set was removed from.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotrrsVersionId :: Lens.Lens' DeleteObjectTaggingResponse (Core.Maybe Types.ObjectVersionId)
dotrrsVersionId = Lens.field @"versionId"
{-# INLINEABLE dotrrsVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotrrsResponseStatus :: Lens.Lens' DeleteObjectTaggingResponse Core.Int
dotrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dotrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
