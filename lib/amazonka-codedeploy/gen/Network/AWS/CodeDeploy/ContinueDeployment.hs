{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ContinueDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a blue/green deployment, starts the process of rerouting traffic from instances in the original environment to instances in the replacement environment without waiting for a specified wait time to elapse. (Traffic rerouting, which is achieved by registering instances in the replacement environment with the load balancer, can start as soon as all instances have a status of Ready.) 
module Network.AWS.CodeDeploy.ContinueDeployment
    (
    -- * Creating a request
      ContinueDeployment (..)
    , mkContinueDeployment
    -- ** Request lenses
    , cdDeploymentId
    , cdDeploymentWaitType

    -- * Destructuring the response
    , ContinueDeploymentResponse (..)
    , mkContinueDeploymentResponse
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkContinueDeployment' smart constructor.
data ContinueDeployment = ContinueDeployment'
  { deploymentId :: Core.Maybe Types.DeploymentId
    -- ^ The unique ID of a blue/green deployment for which you want to start rerouting traffic to the replacement environment. 
  , deploymentWaitType :: Core.Maybe Types.DeploymentWaitType
    -- ^ The status of the deployment's waiting period. @READY_WAIT@ indicates that the deployment is ready to start shifting traffic. @TERMINATION_WAIT@ indicates that the traffic is shifted, but the original target is not terminated. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContinueDeployment' value with any optional fields omitted.
mkContinueDeployment
    :: ContinueDeployment
mkContinueDeployment
  = ContinueDeployment'{deploymentId = Core.Nothing,
                        deploymentWaitType = Core.Nothing}

-- | The unique ID of a blue/green deployment for which you want to start rerouting traffic to the replacement environment. 
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentId :: Lens.Lens' ContinueDeployment (Core.Maybe Types.DeploymentId)
cdDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE cdDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The status of the deployment's waiting period. @READY_WAIT@ indicates that the deployment is ready to start shifting traffic. @TERMINATION_WAIT@ indicates that the traffic is shifted, but the original target is not terminated. 
--
-- /Note:/ Consider using 'deploymentWaitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentWaitType :: Lens.Lens' ContinueDeployment (Core.Maybe Types.DeploymentWaitType)
cdDeploymentWaitType = Lens.field @"deploymentWaitType"
{-# INLINEABLE cdDeploymentWaitType #-}
{-# DEPRECATED deploymentWaitType "Use generic-lens or generic-optics with 'deploymentWaitType' instead"  #-}

instance Core.ToQuery ContinueDeployment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ContinueDeployment where
        toHeaders ContinueDeployment{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.ContinueDeployment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ContinueDeployment where
        toJSON ContinueDeployment{..}
          = Core.object
              (Core.catMaybes
                 [("deploymentId" Core..=) Core.<$> deploymentId,
                  ("deploymentWaitType" Core..=) Core.<$> deploymentWaitType])

instance Core.AWSRequest ContinueDeployment where
        type Rs ContinueDeployment = ContinueDeploymentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull ContinueDeploymentResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkContinueDeploymentResponse' smart constructor.
data ContinueDeploymentResponse = ContinueDeploymentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContinueDeploymentResponse' value with any optional fields omitted.
mkContinueDeploymentResponse
    :: ContinueDeploymentResponse
mkContinueDeploymentResponse = ContinueDeploymentResponse'
