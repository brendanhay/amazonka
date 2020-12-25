{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetPublicAccessBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @PublicAccessBlock@ configuration for an Amazon S3 bucket. To use this operation, you must have the @s3:GetBucketPublicAccessBlock@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- /Important:/ When Amazon S3 evaluates the @PublicAccessBlock@ configuration for a bucket or an object, it checks the @PublicAccessBlock@ configuration for both the bucket (or the bucket that contains the object) and the bucket owner's account. If the @PublicAccessBlock@ settings are different between the bucket and the account, Amazon S3 uses the most restrictive combination of the bucket-level and account-level settings.
-- For more information about when Amazon S3 considers a bucket or an object public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> .
-- The following operations are related to @GetPublicAccessBlock@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
module Network.AWS.S3.GetPublicAccessBlock
  ( -- * Creating a request
    GetPublicAccessBlock (..),
    mkGetPublicAccessBlock,

    -- ** Request lenses
    gpabBucket,
    gpabExpectedBucketOwner,

    -- * Destructuring the response
    GetPublicAccessBlockResponse (..),
    mkGetPublicAccessBlockResponse,

    -- ** Response lenses
    gpabrrsPublicAccessBlockConfiguration,
    gpabrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetPublicAccessBlock' smart constructor.
data GetPublicAccessBlock = GetPublicAccessBlock'
  { -- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to retrieve.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicAccessBlock' value with any optional fields omitted.
mkGetPublicAccessBlock ::
  -- | 'bucket'
  Types.BucketName ->
  GetPublicAccessBlock
mkGetPublicAccessBlock bucket =
  GetPublicAccessBlock' {bucket, expectedBucketOwner = Core.Nothing}

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpabBucket :: Lens.Lens' GetPublicAccessBlock Types.BucketName
gpabBucket = Lens.field @"bucket"
{-# DEPRECATED gpabBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpabExpectedBucketOwner :: Lens.Lens' GetPublicAccessBlock (Core.Maybe Types.ExpectedBucketOwner)
gpabExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gpabExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetPublicAccessBlock where
  type Rs GetPublicAccessBlock = GetPublicAccessBlockResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("publicAccessBlock", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetPublicAccessBlockResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetPublicAccessBlockResponse' smart constructor.
data GetPublicAccessBlockResponse = GetPublicAccessBlockResponse'
  { -- | The @PublicAccessBlock@ configuration currently in effect for this Amazon S3 bucket.
    publicAccessBlockConfiguration :: Core.Maybe Types.PublicAccessBlockConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPublicAccessBlockResponse' value with any optional fields omitted.
mkGetPublicAccessBlockResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPublicAccessBlockResponse
mkGetPublicAccessBlockResponse responseStatus =
  GetPublicAccessBlockResponse'
    { publicAccessBlockConfiguration =
        Core.Nothing,
      responseStatus
    }

-- | The @PublicAccessBlock@ configuration currently in effect for this Amazon S3 bucket.
--
-- /Note:/ Consider using 'publicAccessBlockConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpabrrsPublicAccessBlockConfiguration :: Lens.Lens' GetPublicAccessBlockResponse (Core.Maybe Types.PublicAccessBlockConfiguration)
gpabrrsPublicAccessBlockConfiguration = Lens.field @"publicAccessBlockConfiguration"
{-# DEPRECATED gpabrrsPublicAccessBlockConfiguration "Use generic-lens or generic-optics with 'publicAccessBlockConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpabrrsResponseStatus :: Lens.Lens' GetPublicAccessBlockResponse Core.Int
gpabrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpabrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
