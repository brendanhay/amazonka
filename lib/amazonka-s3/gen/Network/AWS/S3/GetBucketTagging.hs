{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag set associated with the bucket.
--
-- To use this operation, you must have permission to perform the @s3:GetBucketTagging@ action. By default, the bucket owner has this permission and can grant this permission to others.
-- @GetBucketTagging@ has the following special error:
--
--     * Error code: @NoSuchTagSetError@ 
--
--     * Description: There is no tag set associated with the bucket.
--
--
--
--
-- The following operations are related to @GetBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging> 
--
--
module Network.AWS.S3.GetBucketTagging
    (
    -- * Creating a request
      GetBucketTagging (..)
    , mkGetBucketTagging
    -- ** Request lenses
    , gbtBucket
    , gbtExpectedBucketOwner

    -- * Destructuring the response
    , GetBucketTaggingResponse (..)
    , mkGetBucketTaggingResponse
    -- ** Response lenses
    , gbtrrsTagSet
    , gbtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketTagging' smart constructor.
data GetBucketTagging = GetBucketTagging'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket for which to get the tagging information.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketTagging' value with any optional fields omitted.
mkGetBucketTagging
    :: Types.BucketName -- ^ 'bucket'
    -> GetBucketTagging
mkGetBucketTagging bucket
  = GetBucketTagging'{bucket, expectedBucketOwner = Core.Nothing}

-- | The name of the bucket for which to get the tagging information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtBucket :: Lens.Lens' GetBucketTagging Types.BucketName
gbtBucket = Lens.field @"bucket"
{-# INLINEABLE gbtBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtExpectedBucketOwner :: Lens.Lens' GetBucketTagging (Core.Maybe Types.AccountId)
gbtExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gbtExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketTagging where
        toQuery GetBucketTagging{..}
          = Core.toQueryPair "tagging" ("" :: Core.Text)

instance Core.ToHeaders GetBucketTagging where
        toHeaders GetBucketTagging{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketTagging where
        type Rs GetBucketTagging = GetBucketTaggingResponse
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
                 GetBucketTaggingResponse' Core.<$>
                   (x Core..@ "TagSet" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "Tag")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketTaggingResponse' smart constructor.
data GetBucketTaggingResponse = GetBucketTaggingResponse'
  { tagSet :: [Types.Tag]
    -- ^ Contains the tag set.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketTaggingResponse' value with any optional fields omitted.
mkGetBucketTaggingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBucketTaggingResponse
mkGetBucketTaggingResponse responseStatus
  = GetBucketTaggingResponse'{tagSet = Core.mempty, responseStatus}

-- | Contains the tag set.
--
-- /Note:/ Consider using 'tagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtrrsTagSet :: Lens.Lens' GetBucketTaggingResponse [Types.Tag]
gbtrrsTagSet = Lens.field @"tagSet"
{-# INLINEABLE gbtrrsTagSet #-}
{-# DEPRECATED tagSet "Use generic-lens or generic-optics with 'tagSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtrrsResponseStatus :: Lens.Lens' GetBucketTaggingResponse Core.Int
gbtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
