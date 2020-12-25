{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetBulkPublishDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the status of the last BulkPublish operation for an identity pool.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.GetBulkPublishDetails
  ( -- * Creating a request
    GetBulkPublishDetails (..),
    mkGetBulkPublishDetails,

    -- ** Request lenses
    gbpdIdentityPoolId,

    -- * Destructuring the response
    GetBulkPublishDetailsResponse (..),
    mkGetBulkPublishDetailsResponse,

    -- ** Response lenses
    gbpdrrsBulkPublishCompleteTime,
    gbpdrrsBulkPublishStartTime,
    gbpdrrsBulkPublishStatus,
    gbpdrrsFailureMessage,
    gbpdrrsIdentityPoolId,
    gbpdrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetBulkPublishDetails operation.
--
-- /See:/ 'mkGetBulkPublishDetails' smart constructor.
newtype GetBulkPublishDetails = GetBulkPublishDetails'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Types.IdentityPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetBulkPublishDetails' value with any optional fields omitted.
mkGetBulkPublishDetails ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  GetBulkPublishDetails
mkGetBulkPublishDetails identityPoolId =
  GetBulkPublishDetails' {identityPoolId}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdIdentityPoolId :: Lens.Lens' GetBulkPublishDetails Types.IdentityPoolId
gbpdIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED gbpdIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Core.FromJSON GetBulkPublishDetails where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetBulkPublishDetails where
  type Rs GetBulkPublishDetails = GetBulkPublishDetailsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/getBulkPublishDetails")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBulkPublishDetailsResponse'
            Core.<$> (x Core..:? "BulkPublishCompleteTime")
            Core.<*> (x Core..:? "BulkPublishStartTime")
            Core.<*> (x Core..:? "BulkPublishStatus")
            Core.<*> (x Core..:? "FailureMessage")
            Core.<*> (x Core..:? "IdentityPoolId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for the GetBulkPublishDetails operation.
--
-- /See:/ 'mkGetBulkPublishDetailsResponse' smart constructor.
data GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse'
  { -- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish operation completed.
    bulkPublishCompleteTime :: Core.Maybe Core.NominalDiffTime,
    -- | The date/time at which the last bulk publish was initiated.
    bulkPublishStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | Status of the last bulk publish operation, valid values are: NOT_STARTED - No bulk publish has been requested for this identity pool
    --
    -- IN_PROGRESS - Data is being published to the configured stream
    -- SUCCEEDED - All data for the identity pool has been published to the configured stream
    -- FAILED - Some portion of the data has failed to publish, check FailureMessage for the cause.
    bulkPublishStatus :: Core.Maybe Types.BulkPublishStatus,
    -- | If BulkPublishStatus is FAILED this field will contain the error message that caused the bulk publish to fail.
    failureMessage :: Core.Maybe Types.String,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Core.Maybe Types.IdentityPoolId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBulkPublishDetailsResponse' value with any optional fields omitted.
mkGetBulkPublishDetailsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBulkPublishDetailsResponse
mkGetBulkPublishDetailsResponse responseStatus =
  GetBulkPublishDetailsResponse'
    { bulkPublishCompleteTime =
        Core.Nothing,
      bulkPublishStartTime = Core.Nothing,
      bulkPublishStatus = Core.Nothing,
      failureMessage = Core.Nothing,
      identityPoolId = Core.Nothing,
      responseStatus
    }

-- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish operation completed.
--
-- /Note:/ Consider using 'bulkPublishCompleteTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrrsBulkPublishCompleteTime :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Core.NominalDiffTime)
gbpdrrsBulkPublishCompleteTime = Lens.field @"bulkPublishCompleteTime"
{-# DEPRECATED gbpdrrsBulkPublishCompleteTime "Use generic-lens or generic-optics with 'bulkPublishCompleteTime' instead." #-}

-- | The date/time at which the last bulk publish was initiated.
--
-- /Note:/ Consider using 'bulkPublishStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrrsBulkPublishStartTime :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Core.NominalDiffTime)
gbpdrrsBulkPublishStartTime = Lens.field @"bulkPublishStartTime"
{-# DEPRECATED gbpdrrsBulkPublishStartTime "Use generic-lens or generic-optics with 'bulkPublishStartTime' instead." #-}

-- | Status of the last bulk publish operation, valid values are: NOT_STARTED - No bulk publish has been requested for this identity pool
--
-- IN_PROGRESS - Data is being published to the configured stream
-- SUCCEEDED - All data for the identity pool has been published to the configured stream
-- FAILED - Some portion of the data has failed to publish, check FailureMessage for the cause.
--
-- /Note:/ Consider using 'bulkPublishStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrrsBulkPublishStatus :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Types.BulkPublishStatus)
gbpdrrsBulkPublishStatus = Lens.field @"bulkPublishStatus"
{-# DEPRECATED gbpdrrsBulkPublishStatus "Use generic-lens or generic-optics with 'bulkPublishStatus' instead." #-}

-- | If BulkPublishStatus is FAILED this field will contain the error message that caused the bulk publish to fail.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrrsFailureMessage :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Types.String)
gbpdrrsFailureMessage = Lens.field @"failureMessage"
{-# DEPRECATED gbpdrrsFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrrsIdentityPoolId :: Lens.Lens' GetBulkPublishDetailsResponse (Core.Maybe Types.IdentityPoolId)
gbpdrrsIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED gbpdrrsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpdrrsResponseStatus :: Lens.Lens' GetBulkPublishDetailsResponse Core.Int
gbpdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbpdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
