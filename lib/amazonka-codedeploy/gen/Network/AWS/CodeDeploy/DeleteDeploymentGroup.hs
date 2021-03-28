{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteDeploymentGroup (..)
    , mkDeleteDeploymentGroup
    -- ** Request lenses
    , ddgApplicationName
    , ddgDeploymentGroupName

    -- * Destructuring the response
    , DeleteDeploymentGroupResponse (..)
    , mkDeleteDeploymentGroupResponse
    -- ** Response lenses
    , ddgrrsHooksNotCleanedUp
    , ddgrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteDeploymentGroup@ operation.
--
-- /See:/ 'mkDeleteDeploymentGroup' smart constructor.
data DeleteDeploymentGroup = DeleteDeploymentGroup'
  { applicationName :: Types.ApplicationName
    -- ^ The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
  , deploymentGroupName :: Types.DeploymentGroupName
    -- ^ The name of a deployment group for the specified application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentGroup' value with any optional fields omitted.
mkDeleteDeploymentGroup
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.DeploymentGroupName -- ^ 'deploymentGroupName'
    -> DeleteDeploymentGroup
mkDeleteDeploymentGroup applicationName deploymentGroupName
  = DeleteDeploymentGroup'{applicationName, deploymentGroupName}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgApplicationName :: Lens.Lens' DeleteDeploymentGroup Types.ApplicationName
ddgApplicationName = Lens.field @"applicationName"
{-# INLINEABLE ddgApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The name of a deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgDeploymentGroupName :: Lens.Lens' DeleteDeploymentGroup Types.DeploymentGroupName
ddgDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# INLINEABLE ddgDeploymentGroupName #-}
{-# DEPRECATED deploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead"  #-}

instance Core.ToQuery DeleteDeploymentGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDeploymentGroup where
        toHeaders DeleteDeploymentGroup{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.DeleteDeploymentGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDeploymentGroup where
        toJSON DeleteDeploymentGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("applicationName" Core..= applicationName),
                  Core.Just ("deploymentGroupName" Core..= deploymentGroupName)])

instance Core.AWSRequest DeleteDeploymentGroup where
        type Rs DeleteDeploymentGroup = DeleteDeploymentGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDeploymentGroupResponse' Core.<$>
                   (x Core..:? "hooksNotCleanedUp") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @DeleteDeploymentGroup@ operation.
--
-- /See:/ 'mkDeleteDeploymentGroupResponse' smart constructor.
data DeleteDeploymentGroupResponse = DeleteDeploymentGroupResponse'
  { hooksNotCleanedUp :: Core.Maybe [Types.AutoScalingGroup]
    -- ^ If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentGroupResponse' value with any optional fields omitted.
mkDeleteDeploymentGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDeploymentGroupResponse
mkDeleteDeploymentGroupResponse responseStatus
  = DeleteDeploymentGroupResponse'{hooksNotCleanedUp = Core.Nothing,
                                   responseStatus}

-- | If the output contains no data, and the corresponding deployment group contained at least one Auto Scaling group, AWS CodeDeploy successfully removed all corresponding Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group. If the output contains data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event hooks from the Amazon EC2 instances in the Auto Scaling group.
--
-- /Note:/ Consider using 'hooksNotCleanedUp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgrrsHooksNotCleanedUp :: Lens.Lens' DeleteDeploymentGroupResponse (Core.Maybe [Types.AutoScalingGroup])
ddgrrsHooksNotCleanedUp = Lens.field @"hooksNotCleanedUp"
{-# INLINEABLE ddgrrsHooksNotCleanedUp #-}
{-# DEPRECATED hooksNotCleanedUp "Use generic-lens or generic-optics with 'hooksNotCleanedUp' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgrrsResponseStatus :: Lens.Lens' DeleteDeploymentGroupResponse Core.Int
ddgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
