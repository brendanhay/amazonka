{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a deployment.
module Network.AWS.Greengrass.GetDeploymentStatus
  ( -- * Creating a request
    GetDeploymentStatus (..),
    mkGetDeploymentStatus,

    -- ** Request lenses
    gdsGroupId,
    gdsDeploymentId,

    -- * Destructuring the response
    GetDeploymentStatusResponse (..),
    mkGetDeploymentStatusResponse,

    -- ** Response lenses
    gdsrrsDeploymentStatus,
    gdsrrsDeploymentType,
    gdsrrsErrorDetails,
    gdsrrsErrorMessage,
    gdsrrsUpdatedAt,
    gdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDeploymentStatus' smart constructor.
data GetDeploymentStatus = GetDeploymentStatus'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text,
    -- | The ID of the deployment.
    deploymentId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeploymentStatus' value with any optional fields omitted.
mkGetDeploymentStatus ::
  -- | 'groupId'
  Core.Text ->
  -- | 'deploymentId'
  Core.Text ->
  GetDeploymentStatus
mkGetDeploymentStatus groupId deploymentId =
  GetDeploymentStatus' {groupId, deploymentId}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsGroupId :: Lens.Lens' GetDeploymentStatus Core.Text
gdsGroupId = Lens.field @"groupId"
{-# DEPRECATED gdsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDeploymentId :: Lens.Lens' GetDeploymentStatus Core.Text
gdsDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED gdsDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

instance Core.AWSRequest GetDeploymentStatus where
  type Rs GetDeploymentStatus = GetDeploymentStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/groups/" Core.<> (Core.toText groupId)
                Core.<> ("/deployments/")
                Core.<> (Core.toText deploymentId)
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
          GetDeploymentStatusResponse'
            Core.<$> (x Core..:? "DeploymentStatus")
            Core.<*> (x Core..:? "DeploymentType")
            Core.<*> (x Core..:? "ErrorDetails")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "UpdatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDeploymentStatusResponse' smart constructor.
data GetDeploymentStatusResponse = GetDeploymentStatusResponse'
  { -- | The status of the deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
    deploymentStatus :: Core.Maybe Core.Text,
    -- | The type of the deployment.
    deploymentType :: Core.Maybe Types.DeploymentType,
    -- | Error details
    errorDetails :: Core.Maybe [Types.ErrorDetail],
    -- | Error message
    errorMessage :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the deployment status was updated.
    updatedAt :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeploymentStatusResponse' value with any optional fields omitted.
mkGetDeploymentStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDeploymentStatusResponse
mkGetDeploymentStatusResponse responseStatus =
  GetDeploymentStatusResponse'
    { deploymentStatus = Core.Nothing,
      deploymentType = Core.Nothing,
      errorDetails = Core.Nothing,
      errorMessage = Core.Nothing,
      updatedAt = Core.Nothing,
      responseStatus
    }

-- | The status of the deployment: ''InProgress'', ''Building'', ''Success'', or ''Failure''.
--
-- /Note:/ Consider using 'deploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDeploymentStatus :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
gdsrrsDeploymentStatus = Lens.field @"deploymentStatus"
{-# DEPRECATED gdsrrsDeploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead." #-}

-- | The type of the deployment.
--
-- /Note:/ Consider using 'deploymentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsDeploymentType :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Types.DeploymentType)
gdsrrsDeploymentType = Lens.field @"deploymentType"
{-# DEPRECATED gdsrrsDeploymentType "Use generic-lens or generic-optics with 'deploymentType' instead." #-}

-- | Error details
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsErrorDetails :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe [Types.ErrorDetail])
gdsrrsErrorDetails = Lens.field @"errorDetails"
{-# DEPRECATED gdsrrsErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | Error message
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsErrorMessage :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
gdsrrsErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED gdsrrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The time, in milliseconds since the epoch, when the deployment status was updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsUpdatedAt :: Lens.Lens' GetDeploymentStatusResponse (Core.Maybe Core.Text)
gdsrrsUpdatedAt = Lens.field @"updatedAt"
{-# DEPRECATED gdsrrsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsResponseStatus :: Lens.Lens' GetDeploymentStatusResponse Core.Int
gdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
