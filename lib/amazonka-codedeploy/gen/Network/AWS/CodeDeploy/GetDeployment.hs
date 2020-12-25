{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment.
module Network.AWS.CodeDeploy.GetDeployment
  ( -- * Creating a request
    GetDeployment (..),
    mkGetDeployment,

    -- ** Request lenses
    gdDeploymentId,

    -- * Destructuring the response
    GetDeploymentResponse (..),
    mkGetDeploymentResponse,

    -- ** Response lenses
    gdrrsDeploymentInfo,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetDeployment@ operation.
--
-- /See:/ 'mkGetDeployment' smart constructor.
newtype GetDeployment = GetDeployment'
  { -- | The unique ID of a deployment associated with the IAM user or AWS account.
    deploymentId :: Types.DeploymentId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeployment' value with any optional fields omitted.
mkGetDeployment ::
  -- | 'deploymentId'
  Types.DeploymentId ->
  GetDeployment
mkGetDeployment deploymentId = GetDeployment' {deploymentId}

-- | The unique ID of a deployment associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDeploymentId :: Lens.Lens' GetDeployment Types.DeploymentId
gdDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED gdDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

instance Core.FromJSON GetDeployment where
  toJSON GetDeployment {..} =
    Core.object
      (Core.catMaybes [Core.Just ("deploymentId" Core..= deploymentId)])

instance Core.AWSRequest GetDeployment where
  type Rs GetDeployment = GetDeploymentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeDeploy_20141006.GetDeployment")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentResponse'
            Core.<$> (x Core..:? "deploymentInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetDeployment@ operation.
--
-- /See:/ 'mkGetDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { -- | Information about the deployment.
    deploymentInfo :: Core.Maybe Types.DeploymentInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDeploymentResponse' value with any optional fields omitted.
mkGetDeploymentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDeploymentResponse
mkGetDeploymentResponse responseStatus =
  GetDeploymentResponse'
    { deploymentInfo = Core.Nothing,
      responseStatus
    }

-- | Information about the deployment.
--
-- /Note:/ Consider using 'deploymentInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDeploymentInfo :: Lens.Lens' GetDeploymentResponse (Core.Maybe Types.DeploymentInfo)
gdrrsDeploymentInfo = Lens.field @"deploymentInfo"
{-# DEPRECATED gdrrsDeploymentInfo "Use generic-lens or generic-optics with 'deploymentInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDeploymentResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
