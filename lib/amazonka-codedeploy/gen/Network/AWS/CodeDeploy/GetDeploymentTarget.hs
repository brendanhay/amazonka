{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeploymentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a deployment target.
module Network.AWS.CodeDeploy.GetDeploymentTarget
  ( -- * Creating a request
    GetDeploymentTarget (..),
    mkGetDeploymentTarget,

    -- ** Request lenses
    gdtDeploymentId,
    gdtTargetId,

    -- * Destructuring the response
    GetDeploymentTargetResponse (..),
    mkGetDeploymentTargetResponse,

    -- ** Response lenses
    gdtrrsDeploymentTarget,
    gdtrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDeploymentTarget' smart constructor.
data GetDeploymentTarget = GetDeploymentTarget'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | The unique ID of a deployment target.
    targetId :: Core.Maybe Types.TargetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeploymentTarget' value with any optional fields omitted.
mkGetDeploymentTarget ::
  GetDeploymentTarget
mkGetDeploymentTarget =
  GetDeploymentTarget'
    { deploymentId = Core.Nothing,
      targetId = Core.Nothing
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdtDeploymentId :: Lens.Lens' GetDeploymentTarget (Core.Maybe Types.DeploymentId)
gdtDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED gdtDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The unique ID of a deployment target.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdtTargetId :: Lens.Lens' GetDeploymentTarget (Core.Maybe Types.TargetId)
gdtTargetId = Lens.field @"targetId"
{-# DEPRECATED gdtTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

instance Core.FromJSON GetDeploymentTarget where
  toJSON GetDeploymentTarget {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentId" Core..=) Core.<$> deploymentId,
            ("targetId" Core..=) Core.<$> targetId
          ]
      )

instance Core.AWSRequest GetDeploymentTarget where
  type Rs GetDeploymentTarget = GetDeploymentTargetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.GetDeploymentTarget")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentTargetResponse'
            Core.<$> (x Core..:? "deploymentTarget")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDeploymentTargetResponse' smart constructor.
data GetDeploymentTargetResponse = GetDeploymentTargetResponse'
  { -- | A deployment target that contains information about a deployment such as its status, lifecycle events, and when it was last updated. It also contains metadata about the deployment target. The deployment target metadata depends on the deployment target's type (@instanceTarget@ , @lambdaTarget@ , or @ecsTarget@ ).
    deploymentTarget :: Core.Maybe Types.DeploymentTarget,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDeploymentTargetResponse' value with any optional fields omitted.
mkGetDeploymentTargetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDeploymentTargetResponse
mkGetDeploymentTargetResponse responseStatus =
  GetDeploymentTargetResponse'
    { deploymentTarget = Core.Nothing,
      responseStatus
    }

-- | A deployment target that contains information about a deployment such as its status, lifecycle events, and when it was last updated. It also contains metadata about the deployment target. The deployment target metadata depends on the deployment target's type (@instanceTarget@ , @lambdaTarget@ , or @ecsTarget@ ).
--
-- /Note:/ Consider using 'deploymentTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdtrrsDeploymentTarget :: Lens.Lens' GetDeploymentTargetResponse (Core.Maybe Types.DeploymentTarget)
gdtrrsDeploymentTarget = Lens.field @"deploymentTarget"
{-# DEPRECATED gdtrrsDeploymentTarget "Use generic-lens or generic-optics with 'deploymentTarget' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdtrrsResponseStatus :: Lens.Lens' GetDeploymentTargetResponse Core.Int
gdtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
