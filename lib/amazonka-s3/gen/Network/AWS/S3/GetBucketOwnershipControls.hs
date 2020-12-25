{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:GetBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> .
-- The following operations are related to @GetBucketOwnershipControls@ :
--
--     * 'PutBucketOwnershipControls'
--
--
--     * 'DeleteBucketOwnershipControls'
module Network.AWS.S3.GetBucketOwnershipControls
  ( -- * Creating a request
    GetBucketOwnershipControls (..),
    mkGetBucketOwnershipControls,

    -- ** Request lenses
    gbocBucket,
    gbocExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketOwnershipControlsResponse (..),
    mkGetBucketOwnershipControlsResponse,

    -- ** Response lenses
    gbocrrsOwnershipControls,
    gbocrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketOwnershipControls' smart constructor.
data GetBucketOwnershipControls = GetBucketOwnershipControls'
  { -- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to retrieve.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketOwnershipControls' value with any optional fields omitted.
mkGetBucketOwnershipControls ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketOwnershipControls
mkGetBucketOwnershipControls bucket =
  GetBucketOwnershipControls'
    { bucket,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbocBucket :: Lens.Lens' GetBucketOwnershipControls Types.BucketName
gbocBucket = Lens.field @"bucket"
{-# DEPRECATED gbocBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbocExpectedBucketOwner :: Lens.Lens' GetBucketOwnershipControls (Core.Maybe Types.ExpectedBucketOwner)
gbocExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbocExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketOwnershipControls where
  type
    Rs GetBucketOwnershipControls =
      GetBucketOwnershipControlsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("ownershipControls", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketOwnershipControlsResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketOwnershipControlsResponse' smart constructor.
data GetBucketOwnershipControlsResponse = GetBucketOwnershipControlsResponse'
  { -- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) currently in effect for this Amazon S3 bucket.
    ownershipControls :: Core.Maybe Types.OwnershipControls,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketOwnershipControlsResponse' value with any optional fields omitted.
mkGetBucketOwnershipControlsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketOwnershipControlsResponse
mkGetBucketOwnershipControlsResponse responseStatus =
  GetBucketOwnershipControlsResponse'
    { ownershipControls =
        Core.Nothing,
      responseStatus
    }

-- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) currently in effect for this Amazon S3 bucket.
--
-- /Note:/ Consider using 'ownershipControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbocrrsOwnershipControls :: Lens.Lens' GetBucketOwnershipControlsResponse (Core.Maybe Types.OwnershipControls)
gbocrrsOwnershipControls = Lens.field @"ownershipControls"
{-# DEPRECATED gbocrrsOwnershipControls "Use generic-lens or generic-optics with 'ownershipControls' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbocrrsResponseStatus :: Lens.Lens' GetBucketOwnershipControlsResponse Core.Int
gbocrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbocrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
