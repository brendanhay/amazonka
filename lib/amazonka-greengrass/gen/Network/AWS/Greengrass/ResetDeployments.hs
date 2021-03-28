{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ResetDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a group's deployments.
module Network.AWS.Greengrass.ResetDeployments
    (
    -- * Creating a request
      ResetDeployments (..)
    , mkResetDeployments
    -- ** Request lenses
    , rdGroupId
    , rdAmznClientToken
    , rdForce

    -- * Destructuring the response
    , ResetDeploymentsResponse (..)
    , mkResetDeploymentsResponse
    -- ** Response lenses
    , rdrrsDeploymentArn
    , rdrrsDeploymentId
    , rdrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Information needed to reset deployments.
--
-- /See:/ 'mkResetDeployments' smart constructor.
data ResetDeployments = ResetDeployments'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  , amznClientToken :: Core.Maybe Core.Text
    -- ^ A client token used to correlate requests and responses.
  , force :: Core.Maybe Core.Bool
    -- ^ If true, performs a best-effort only core reset.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetDeployments' value with any optional fields omitted.
mkResetDeployments
    :: Core.Text -- ^ 'groupId'
    -> ResetDeployments
mkResetDeployments groupId
  = ResetDeployments'{groupId, amznClientToken = Core.Nothing,
                      force = Core.Nothing}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdGroupId :: Lens.Lens' ResetDeployments Core.Text
rdGroupId = Lens.field @"groupId"
{-# INLINEABLE rdGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdAmznClientToken :: Lens.Lens' ResetDeployments (Core.Maybe Core.Text)
rdAmznClientToken = Lens.field @"amznClientToken"
{-# INLINEABLE rdAmznClientToken #-}
{-# DEPRECATED amznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead"  #-}

-- | If true, performs a best-effort only core reset.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdForce :: Lens.Lens' ResetDeployments (Core.Maybe Core.Bool)
rdForce = Lens.field @"force"
{-# INLINEABLE rdForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

instance Core.ToQuery ResetDeployments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResetDeployments where
        toHeaders ResetDeployments{..}
          = Core.toHeaders "X-Amzn-Client-Token" amznClientToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResetDeployments where
        toJSON ResetDeployments{..}
          = Core.object (Core.catMaybes [("Force" Core..=) Core.<$> force])

instance Core.AWSRequest ResetDeployments where
        type Rs ResetDeployments = ResetDeploymentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<>
                             "/deployments/$reset",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResetDeploymentsResponse' Core.<$>
                   (x Core..:? "DeploymentArn") Core.<*> x Core..:? "DeploymentId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetDeploymentsResponse' smart constructor.
data ResetDeploymentsResponse = ResetDeploymentsResponse'
  { deploymentArn :: Core.Maybe Core.Text
    -- ^ The ARN of the deployment.
  , deploymentId :: Core.Maybe Core.Text
    -- ^ The ID of the deployment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetDeploymentsResponse' value with any optional fields omitted.
mkResetDeploymentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetDeploymentsResponse
mkResetDeploymentsResponse responseStatus
  = ResetDeploymentsResponse'{deploymentArn = Core.Nothing,
                              deploymentId = Core.Nothing, responseStatus}

-- | The ARN of the deployment.
--
-- /Note:/ Consider using 'deploymentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrrsDeploymentArn :: Lens.Lens' ResetDeploymentsResponse (Core.Maybe Core.Text)
rdrrsDeploymentArn = Lens.field @"deploymentArn"
{-# INLINEABLE rdrrsDeploymentArn #-}
{-# DEPRECATED deploymentArn "Use generic-lens or generic-optics with 'deploymentArn' instead"  #-}

-- | The ID of the deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrrsDeploymentId :: Lens.Lens' ResetDeploymentsResponse (Core.Maybe Core.Text)
rdrrsDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE rdrrsDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdrrsResponseStatus :: Lens.Lens' ResetDeploymentsResponse Core.Int
rdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
