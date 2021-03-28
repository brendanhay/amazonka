{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeploymentGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment group.
module Network.AWS.CodeDeploy.GetDeploymentGroup
    (
    -- * Creating a request
      GetDeploymentGroup (..)
    , mkGetDeploymentGroup
    -- ** Request lenses
    , gdgApplicationName
    , gdgDeploymentGroupName

    -- * Destructuring the response
    , GetDeploymentGroupResponse (..)
    , mkGetDeploymentGroupResponse
    -- ** Response lenses
    , gdgrrsDeploymentGroupInfo
    , gdgrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetDeploymentGroup@ operation.
--
-- /See:/ 'mkGetDeploymentGroup' smart constructor.
data GetDeploymentGroup = GetDeploymentGroup'
  { applicationName :: Types.ApplicationName
    -- ^ The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
  , deploymentGroupName :: Types.DeploymentGroupName
    -- ^ The name of a deployment group for the specified application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeploymentGroup' value with any optional fields omitted.
mkGetDeploymentGroup
    :: Types.ApplicationName -- ^ 'applicationName'
    -> Types.DeploymentGroupName -- ^ 'deploymentGroupName'
    -> GetDeploymentGroup
mkGetDeploymentGroup applicationName deploymentGroupName
  = GetDeploymentGroup'{applicationName, deploymentGroupName}

-- | The name of an AWS CodeDeploy application associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgApplicationName :: Lens.Lens' GetDeploymentGroup Types.ApplicationName
gdgApplicationName = Lens.field @"applicationName"
{-# INLINEABLE gdgApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The name of a deployment group for the specified application.
--
-- /Note:/ Consider using 'deploymentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgDeploymentGroupName :: Lens.Lens' GetDeploymentGroup Types.DeploymentGroupName
gdgDeploymentGroupName = Lens.field @"deploymentGroupName"
{-# INLINEABLE gdgDeploymentGroupName #-}
{-# DEPRECATED deploymentGroupName "Use generic-lens or generic-optics with 'deploymentGroupName' instead"  #-}

instance Core.ToQuery GetDeploymentGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDeploymentGroup where
        toHeaders GetDeploymentGroup{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.GetDeploymentGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDeploymentGroup where
        toJSON GetDeploymentGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("applicationName" Core..= applicationName),
                  Core.Just ("deploymentGroupName" Core..= deploymentGroupName)])

instance Core.AWSRequest GetDeploymentGroup where
        type Rs GetDeploymentGroup = GetDeploymentGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeploymentGroupResponse' Core.<$>
                   (x Core..:? "deploymentGroupInfo") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetDeploymentGroup@ operation.
--
-- /See:/ 'mkGetDeploymentGroupResponse' smart constructor.
data GetDeploymentGroupResponse = GetDeploymentGroupResponse'
  { deploymentGroupInfo :: Core.Maybe Types.DeploymentGroupInfo
    -- ^ Information about the deployment group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDeploymentGroupResponse' value with any optional fields omitted.
mkGetDeploymentGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDeploymentGroupResponse
mkGetDeploymentGroupResponse responseStatus
  = GetDeploymentGroupResponse'{deploymentGroupInfo = Core.Nothing,
                                responseStatus}

-- | Information about the deployment group.
--
-- /Note:/ Consider using 'deploymentGroupInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrrsDeploymentGroupInfo :: Lens.Lens' GetDeploymentGroupResponse (Core.Maybe Types.DeploymentGroupInfo)
gdgrrsDeploymentGroupInfo = Lens.field @"deploymentGroupInfo"
{-# INLINEABLE gdgrrsDeploymentGroupInfo #-}
{-# DEPRECATED deploymentGroupInfo "Use generic-lens or generic-optics with 'deploymentGroupInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdgrrsResponseStatus :: Lens.Lens' GetDeploymentGroupResponse Core.Int
gdgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
