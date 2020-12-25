{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the versioning state of a bucket.
--
-- To retrieve the versioning state of a bucket, you must be the bucket owner.
-- This implementation also returns the MFA Delete status of the versioning state. If the MFA Delete status is @enabled@ , the bucket owner must use an authentication device to change the versioning state of the bucket.
-- The following operations are related to @GetBucketVersioning@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.GetBucketVersioning
  ( -- * Creating a request
    GetBucketVersioning (..),
    mkGetBucketVersioning,

    -- ** Request lenses
    gbvBucket,
    gbvExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketVersioningResponse (..),
    mkGetBucketVersioningResponse,

    -- ** Response lenses
    gbvrrsMFADelete,
    gbvrrsStatus,
    gbvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketVersioning' smart constructor.
data GetBucketVersioning = GetBucketVersioning'
  { -- | The name of the bucket for which to get the versioning information.
    bucket :: Types.BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketVersioning' value with any optional fields omitted.
mkGetBucketVersioning ::
  -- | 'bucket'
  Types.BucketName ->
  GetBucketVersioning
mkGetBucketVersioning bucket =
  GetBucketVersioning' {bucket, expectedBucketOwner = Core.Nothing}

-- | The name of the bucket for which to get the versioning information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvBucket :: Lens.Lens' GetBucketVersioning Types.BucketName
gbvBucket = Lens.field @"bucket"
{-# DEPRECATED gbvBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvExpectedBucketOwner :: Lens.Lens' GetBucketVersioning (Core.Maybe Types.ExpectedBucketOwner)
gbvExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gbvExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest GetBucketVersioning where
  type Rs GetBucketVersioning = GetBucketVersioningResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("versioning", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketVersioningResponse'
            Core.<$> (x Core..@? "MfaDelete")
            Core.<*> (x Core..@? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBucketVersioningResponse' smart constructor.
data GetBucketVersioningResponse = GetBucketVersioningResponse'
  { -- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
    mFADelete :: Core.Maybe Types.MFADeleteStatus,
    -- | The versioning state of the bucket.
    status :: Core.Maybe Types.BucketVersioningStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketVersioningResponse' value with any optional fields omitted.
mkGetBucketVersioningResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBucketVersioningResponse
mkGetBucketVersioningResponse responseStatus =
  GetBucketVersioningResponse'
    { mFADelete = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
--
-- /Note:/ Consider using 'mFADelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrrsMFADelete :: Lens.Lens' GetBucketVersioningResponse (Core.Maybe Types.MFADeleteStatus)
gbvrrsMFADelete = Lens.field @"mFADelete"
{-# DEPRECATED gbvrrsMFADelete "Use generic-lens or generic-optics with 'mFADelete' instead." #-}

-- | The versioning state of the bucket.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrrsStatus :: Lens.Lens' GetBucketVersioningResponse (Core.Maybe Types.BucketVersioningStatus)
gbvrrsStatus = Lens.field @"status"
{-# DEPRECATED gbvrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrrsResponseStatus :: Lens.Lens' GetBucketVersioningResponse Core.Int
gbvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
