{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetBulkDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a bulk deployment.
module Network.AWS.Greengrass.GetBulkDeploymentStatus
  ( -- * Creating a request
    GetBulkDeploymentStatus (..),
    mkGetBulkDeploymentStatus,

    -- ** Request lenses
    gbdsBulkDeploymentId,

    -- * Destructuring the response
    GetBulkDeploymentStatusResponse (..),
    mkGetBulkDeploymentStatusResponse,

    -- ** Response lenses
    gbdsrrsBulkDeploymentMetrics,
    gbdsrrsBulkDeploymentStatus,
    gbdsrrsCreatedAt,
    gbdsrrsErrorDetails,
    gbdsrrsErrorMessage,
    gbdsrrsTags,
    gbdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBulkDeploymentStatus' smart constructor.
newtype GetBulkDeploymentStatus = GetBulkDeploymentStatus'
  { -- | The ID of the bulk deployment.
    bulkDeploymentId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetBulkDeploymentStatus' value with any optional fields omitted.
mkGetBulkDeploymentStatus ::
  -- | 'bulkDeploymentId'
  Core.Text ->
  GetBulkDeploymentStatus
mkGetBulkDeploymentStatus bulkDeploymentId =
  GetBulkDeploymentStatus' {bulkDeploymentId}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsBulkDeploymentId :: Lens.Lens' GetBulkDeploymentStatus Core.Text
gbdsBulkDeploymentId = Lens.field @"bulkDeploymentId"
{-# DEPRECATED gbdsBulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead." #-}

instance Core.AWSRequest GetBulkDeploymentStatus where
  type Rs GetBulkDeploymentStatus = GetBulkDeploymentStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/bulk/deployments/"
                Core.<> (Core.toText bulkDeploymentId)
                Core.<> ("/status")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBulkDeploymentStatusResponse'
            Core.<$> (x Core..:? "BulkDeploymentMetrics")
            Core.<*> (x Core..:? "BulkDeploymentStatus")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "ErrorDetails")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBulkDeploymentStatusResponse' smart constructor.
data GetBulkDeploymentStatusResponse = GetBulkDeploymentStatusResponse'
  { -- | Relevant metrics on input records processed during bulk deployment.
    bulkDeploymentMetrics :: Core.Maybe Types.BulkDeploymentMetrics,
    -- | The status of the bulk deployment.
    bulkDeploymentStatus :: Core.Maybe Types.BulkDeploymentStatus,
    -- | The time, in ISO format, when the deployment was created.
    createdAt :: Core.Maybe Core.Text,
    -- | Error details
    errorDetails :: Core.Maybe [Types.ErrorDetail],
    -- | Error message
    errorMessage :: Core.Maybe Core.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBulkDeploymentStatusResponse' value with any optional fields omitted.
mkGetBulkDeploymentStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBulkDeploymentStatusResponse
mkGetBulkDeploymentStatusResponse responseStatus =
  GetBulkDeploymentStatusResponse'
    { bulkDeploymentMetrics =
        Core.Nothing,
      bulkDeploymentStatus = Core.Nothing,
      createdAt = Core.Nothing,
      errorDetails = Core.Nothing,
      errorMessage = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Relevant metrics on input records processed during bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsBulkDeploymentMetrics :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe Types.BulkDeploymentMetrics)
gbdsrrsBulkDeploymentMetrics = Lens.field @"bulkDeploymentMetrics"
{-# DEPRECATED gbdsrrsBulkDeploymentMetrics "Use generic-lens or generic-optics with 'bulkDeploymentMetrics' instead." #-}

-- | The status of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsBulkDeploymentStatus :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe Types.BulkDeploymentStatus)
gbdsrrsBulkDeploymentStatus = Lens.field @"bulkDeploymentStatus"
{-# DEPRECATED gbdsrrsBulkDeploymentStatus "Use generic-lens or generic-optics with 'bulkDeploymentStatus' instead." #-}

-- | The time, in ISO format, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsCreatedAt :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe Core.Text)
gbdsrrsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED gbdsrrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Error details
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsErrorDetails :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe [Types.ErrorDetail])
gbdsrrsErrorDetails = Lens.field @"errorDetails"
{-# DEPRECATED gbdsrrsErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | Error message
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsErrorMessage :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe Core.Text)
gbdsrrsErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED gbdsrrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsTags :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gbdsrrsTags = Lens.field @"tags"
{-# DEPRECATED gbdsrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsResponseStatus :: Lens.Lens' GetBulkDeploymentStatusResponse Core.Int
gbdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
