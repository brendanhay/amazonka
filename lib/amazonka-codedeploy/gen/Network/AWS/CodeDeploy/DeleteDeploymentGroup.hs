{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment group.
module Network.AWS.CodeDeploy.DeleteDeploymentGroup
  ( -- * Creating a request
    DeleteDeploymentGroup (..),
    mkDeleteDeploymentGroup,

    -- ** Request lenses
    ddgApplicationName,
    ddgDeploymentGroupName,

    -- * Destructuring the response
    DeleteDeploymentGroupResponse (..),
    mkDeleteDeploymentGroupResponse,

    -- ** Response lenses
    ddgrrsHooksNotCleanedUp,
    ddgrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteDeploymentGroup@ operation.
--
-- /See:/ 'mkDeleteDeploymentGroup' smart constructor.
data DeleteDeploymentGroup = DeleteDeploymentGroup'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
    applicationName :: Types.ApplicationName,
    -- | The name of a deployment group for the specified application.
    deploymentGroupName :: Types.DeploymentGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentGroup' value with any optional fields omitted.
mkDeleteDeploymentGroup ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'deploymentGroupName'
  Types.DeploymentGroupName ->
  DeleteDeploymentGroup
mkDeleteDeploymentGroup applicationName deploymentGroupName =
  DeleteDeploymentGroup' {applicationName, deploymentGroupName}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgApplicationName :: Lens.Lens' DeleteDeploymentGroup Types.ApplicationName
ddgApplicationName = Lens.field @"applicationName"
{-# DEPRECATED ddgApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of a deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgDeploymentGroupName :: Lens.Lens' DeleteDeploymentGroup Types.DeploymentGroupName
ddgDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# DEPRECATED ddgDeploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead." #-}

instance Core.FromJSON DeleteDeploymentGroup where
  toJSON DeleteDeploymentGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("applicationName" Core..= applicationName),
            Core.Just ("deploymentGroupName" Core..= deploymentGroupName)
          ]
      )

instance Core.AWSRequest DeleteDeploymentGroup where
  type Rs DeleteDeploymentGroup = DeleteDeploymentGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.DeleteDeploymentGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDeploymentGroupResponse'
            Core.<$> (x Core..:? "hooksNotCleanedUp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @DeleteDeploymentGroup@ operation.
--
-- /See:/ 'mkDeleteDeploymentGroupResponse' smart constructor.
data DeleteDeploymentGroupResponse = DeleteDeploymentGroupResponse'
  { -- | If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
    hooksNotCleanedUp :: Core.Maybe [Types.AutoScalingGroup],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentGroupResponse' value with any optional fields omitted.
mkDeleteDeploymentGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDeploymentGroupResponse
mkDeleteDeploymentGroupResponse responseStatus =
  DeleteDeploymentGroupResponse'
    { hooksNotCleanedUp = Core.Nothing,
      responseStatus
    }

-- | If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
--
-- /Note:/ Consider using 'hooksNotCleanedUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgrrsHooksNotCleanedUp :: Lens.Lens' DeleteDeploymentGroupResponse (Core.Maybe [Types.AutoScalingGroup])
ddgrrsHooksNotCleanedUp = Lens.field @"hooksNotCleanedUp"
{-# DEPRECATED ddgrrsHooksNotCleanedUp "Use generic-lens or generic-optics with 'hooksNotCleanedUp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgrrsResponseStatus :: Lens.Lens' DeleteDeploymentGroupResponse Core.Int
ddgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
