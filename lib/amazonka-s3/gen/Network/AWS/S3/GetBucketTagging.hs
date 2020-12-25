{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.S3.GetBucketTagging
  ( -- * Creating a request
    GetBucketTagging (..),
    mkGetBucketTagging,

    -- ** Request lenses
    gbtBucket,
    gbtExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketTaggingResponse (..),
    mkGetBucketTaggingResponse,

    -- ** Response lenses
    gbtrrsTagSet,
    gbtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketTagging' smart constructor.
data GetBucketTagging = GetBucketTagging'
  { -- | The name of the bucket for which to get the tagging information.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketTagging' value with any optional fields omitted.
mkGetBucketTagging ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketTagging
mkGetBucketTagging bucket =
  GetBucketTagging' {bucket, expectedBucketOwner = Core.Nothing}

-- | The name of the bucket for which to get the tagging information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtBucket :: Lens.Lens' GetBucketTagging Types.BucketName
gbtBucket = Lens.field @"bucket"
{-# DEPRECATED gbtBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtExpectedBucketOwner :: Lens.Lens' GetBucketTagging (Core.Maybe Types.AccountId)
gbtExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbtExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketTagging where
  type Rs GetBucketTagging = GetBucketTaggingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("tagging", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketTaggingResponse'
            Core.<$> ( x Core..@? "TagSet" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "Tag"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketTaggingResponse' smart constructor.
data GetBucketTaggingResponse = GetBucketTaggingResponse'
  { -- | Contains the tag set.
    tagSet :: [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketTaggingResponse' value with any optional fields omitted.
mkGetBucketTaggingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketTaggingResponse
mkGetBucketTaggingResponse responseStatus =
  GetBucketTaggingResponse' {tagSet = Core.mempty, responseStatus}

-- | Contains the tag set.
--
-- /Note:/ Consider using 'tagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtrrsTagSet :: Lens.Lens' GetBucketTaggingResponse [Types.Tag]
gbtrrsTagSet = Lens.field @"tagSet"
{-# DEPRECATED gbtrrsTagSet "Use generic-lens or generic-optics with 'tagSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtrrsResponseStatus :: Lens.Lens' GetBucketTaggingResponse Core.Int
gbtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
