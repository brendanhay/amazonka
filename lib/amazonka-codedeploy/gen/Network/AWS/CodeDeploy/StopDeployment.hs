{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.StopDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop an ongoing deployment.
module Network.AWS.CodeDeploy.StopDeployment
    (
    -- * Creating a request
      StopDeployment (..)
    , mkStopDeployment
    -- ** Request lenses
    , sdDeploymentId
    , sdAutoRollbackEnabled

    -- * Destructuring the response
    , StopDeploymentResponse (..)
    , mkStopDeploymentResponse
    -- ** Response lenses
    , sdrrsStatus
    , sdrrsStatusMessage
    , sdrrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @StopDeployment@ operation. 
--
-- /See:/ 'mkStopDeployment' smart constructor.
data StopDeployment = StopDeployment'
  { deploymentId :: Types.DeploymentId
    -- ^ The unique ID of a deployment. 
  , autoRollbackEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates, when a deployment is stopped, whether instances that have been updated should be rolled back to the previous version of the application revision. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopDeployment' value with any optional fields omitted.
mkStopDeployment
    :: Types.DeploymentId -- ^ 'deploymentId'
    -> StopDeployment
mkStopDeployment deploymentId
  = StopDeployment'{deploymentId, autoRollbackEnabled = Core.Nothing}

-- | The unique ID of a deployment. 
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDeploymentId :: Lens.Lens' StopDeployment Types.DeploymentId
sdDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE sdDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | Indicates, when a deployment is stopped, whether instances that have been updated should be rolled back to the previous version of the application revision. 
--
-- /Note:/ Consider using 'autoRollbackEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAutoRollbackEnabled :: Lens.Lens' StopDeployment (Core.Maybe Core.Bool)
sdAutoRollbackEnabled = Lens.field @"autoRollbackEnabled"
{-# INLINEABLE sdAutoRollbackEnabled #-}
{-# DEPRECATED autoRollbackEnabled "Use generic-lens or generic-optics with 'autoRollbackEnabled' instead"  #-}

instance Core.ToQuery StopDeployment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopDeployment where
        toHeaders StopDeployment{..}
          = Core.pure ("X-Amz-Target", "CodeDeploy_20141006.StopDeployment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopDeployment where
        toJSON StopDeployment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("deploymentId" Core..= deploymentId),
                  ("autoRollbackEnabled" Core..=) Core.<$> autoRollbackEnabled])

instance Core.AWSRequest StopDeployment where
        type Rs StopDeployment = StopDeploymentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopDeploymentResponse' Core.<$>
                   (x Core..:? "status") Core.<*> x Core..:? "statusMessage" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @StopDeployment@ operation. 
--
-- /See:/ 'mkStopDeploymentResponse' smart constructor.
data StopDeploymentResponse = StopDeploymentResponse'
  { status :: Core.Maybe Types.StopStatus
    -- ^ The status of the stop deployment operation:
--
--
--     * Pending: The stop operation is pending.
--
--
--     * Succeeded: The stop operation was successful.
--
--
  , statusMessage :: Core.Maybe Types.StatusMessage
    -- ^ An accompanying status message.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopDeploymentResponse' value with any optional fields omitted.
mkStopDeploymentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopDeploymentResponse
mkStopDeploymentResponse responseStatus
  = StopDeploymentResponse'{status = Core.Nothing,
                            statusMessage = Core.Nothing, responseStatus}

-- | The status of the stop deployment operation:
--
--
--     * Pending: The stop operation is pending.
--
--
--     * Succeeded: The stop operation was successful.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsStatus :: Lens.Lens' StopDeploymentResponse (Core.Maybe Types.StopStatus)
sdrrsStatus = Lens.field @"status"
{-# INLINEABLE sdrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | An accompanying status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsStatusMessage :: Lens.Lens' StopDeploymentResponse (Core.Maybe Types.StatusMessage)
sdrrsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE sdrrsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsResponseStatus :: Lens.Lens' StopDeploymentResponse Core.Int
sdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
