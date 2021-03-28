{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CancelMaintenanceWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a maintenance window execution that is already in progress and cancels any tasks in the window that have not already starting running. (Tasks already in progress will continue to completion.)
module Network.AWS.SSM.CancelMaintenanceWindowExecution
    (
    -- * Creating a request
      CancelMaintenanceWindowExecution (..)
    , mkCancelMaintenanceWindowExecution
    -- ** Request lenses
    , cmweWindowExecutionId

    -- * Destructuring the response
    , CancelMaintenanceWindowExecutionResponse (..)
    , mkCancelMaintenanceWindowExecutionResponse
    -- ** Response lenses
    , cmwerrsWindowExecutionId
    , cmwerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCancelMaintenanceWindowExecution' smart constructor.
newtype CancelMaintenanceWindowExecution = CancelMaintenanceWindowExecution'
  { windowExecutionId :: Types.WindowExecutionId
    -- ^ The ID of the maintenance window execution to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMaintenanceWindowExecution' value with any optional fields omitted.
mkCancelMaintenanceWindowExecution
    :: Types.WindowExecutionId -- ^ 'windowExecutionId'
    -> CancelMaintenanceWindowExecution
mkCancelMaintenanceWindowExecution windowExecutionId
  = CancelMaintenanceWindowExecution'{windowExecutionId}

-- | The ID of the maintenance window execution to stop.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmweWindowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecution Types.WindowExecutionId
cmweWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE cmweWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

instance Core.ToQuery CancelMaintenanceWindowExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelMaintenanceWindowExecution where
        toHeaders CancelMaintenanceWindowExecution{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.CancelMaintenanceWindowExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelMaintenanceWindowExecution where
        toJSON CancelMaintenanceWindowExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WindowExecutionId" Core..= windowExecutionId)])

instance Core.AWSRequest CancelMaintenanceWindowExecution where
        type Rs CancelMaintenanceWindowExecution =
             CancelMaintenanceWindowExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelMaintenanceWindowExecutionResponse' Core.<$>
                   (x Core..:? "WindowExecutionId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelMaintenanceWindowExecutionResponse' smart constructor.
data CancelMaintenanceWindowExecutionResponse = CancelMaintenanceWindowExecutionResponse'
  { windowExecutionId :: Core.Maybe Types.WindowExecutionId
    -- ^ The ID of the maintenance window execution that has been stopped.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMaintenanceWindowExecutionResponse' value with any optional fields omitted.
mkCancelMaintenanceWindowExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelMaintenanceWindowExecutionResponse
mkCancelMaintenanceWindowExecutionResponse responseStatus
  = CancelMaintenanceWindowExecutionResponse'{windowExecutionId =
                                                Core.Nothing,
                                              responseStatus}

-- | The ID of the maintenance window execution that has been stopped.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwerrsWindowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecutionResponse (Core.Maybe Types.WindowExecutionId)
cmwerrsWindowExecutionId = Lens.field @"windowExecutionId"
{-# INLINEABLE cmwerrsWindowExecutionId #-}
{-# DEPRECATED windowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwerrsResponseStatus :: Lens.Lens' CancelMaintenanceWindowExecutionResponse Core.Int
cmwerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmwerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
