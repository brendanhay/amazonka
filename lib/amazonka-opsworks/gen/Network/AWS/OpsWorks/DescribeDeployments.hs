{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of deployments.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeDeployments
  ( -- * Creating a request
    DescribeDeployments (..),
    mkDescribeDeployments,

    -- ** Request lenses
    ddAppId,
    ddDeploymentIds,
    ddStackId,

    -- * Destructuring the response
    DescribeDeploymentsResponse (..),
    mkDescribeDeploymentsResponse,

    -- ** Response lenses
    ddrrsDeployments,
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDeployments' smart constructor.
data DescribeDeployments = DescribeDeployments'
  { -- | The app ID. If you include this parameter, the command returns a description of the commands associated with the specified app.
    appId :: Core.Maybe Types.String,
    -- | An array of deployment IDs to be described. If you include this parameter, the command returns a description of the specified deployments. Otherwise, it returns a description of every deployment.
    deploymentIds :: Core.Maybe [Types.String],
    -- | The stack ID. If you include this parameter, the command returns a description of the commands associated with the specified stack.
    stackId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDeployments' value with any optional fields omitted.
mkDescribeDeployments ::
  DescribeDeployments
mkDescribeDeployments =
  DescribeDeployments'
    { appId = Core.Nothing,
      deploymentIds = Core.Nothing,
      stackId = Core.Nothing
    }

-- | The app ID. If you include this parameter, the command returns a description of the commands associated with the specified app.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAppId :: Lens.Lens' DescribeDeployments (Core.Maybe Types.String)
ddAppId = Lens.field @"appId"
{-# DEPRECATED ddAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | An array of deployment IDs to be described. If you include this parameter, the command returns a description of the specified deployments. Otherwise, it returns a description of every deployment.
--
-- /Note:/ Consider using 'deploymentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeploymentIds :: Lens.Lens' DescribeDeployments (Core.Maybe [Types.String])
ddDeploymentIds = Lens.field @"deploymentIds"
{-# DEPRECATED ddDeploymentIds "Use generic-lens or generic-optics with 'deploymentIds' instead." #-}

-- | The stack ID. If you include this parameter, the command returns a description of the commands associated with the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStackId :: Lens.Lens' DescribeDeployments (Core.Maybe Types.String)
ddStackId = Lens.field @"stackId"
{-# DEPRECATED ddStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribeDeployments where
  toJSON DescribeDeployments {..} =
    Core.object
      ( Core.catMaybes
          [ ("AppId" Core..=) Core.<$> appId,
            ("DeploymentIds" Core..=) Core.<$> deploymentIds,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.AWSRequest DescribeDeployments where
  type Rs DescribeDeployments = DescribeDeploymentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeDeployments")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeploymentsResponse'
            Core.<$> (x Core..:? "Deployments") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeDeployments@ request.
--
-- /See:/ 'mkDescribeDeploymentsResponse' smart constructor.
data DescribeDeploymentsResponse = DescribeDeploymentsResponse'
  { -- | An array of @Deployment@ objects that describe the deployments.
    deployments :: Core.Maybe [Types.Deployment],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDeploymentsResponse' value with any optional fields omitted.
mkDescribeDeploymentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDeploymentsResponse
mkDescribeDeploymentsResponse responseStatus =
  DescribeDeploymentsResponse'
    { deployments = Core.Nothing,
      responseStatus
    }

-- | An array of @Deployment@ objects that describe the deployments.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDeployments :: Lens.Lens' DescribeDeploymentsResponse (Core.Maybe [Types.Deployment])
ddrrsDeployments = Lens.field @"deployments"
{-# DEPRECATED ddrrsDeployments "Use generic-lens or generic-optics with 'deployments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDeploymentsResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
