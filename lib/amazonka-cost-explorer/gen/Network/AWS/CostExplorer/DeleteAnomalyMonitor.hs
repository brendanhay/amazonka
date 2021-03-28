{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.DeleteAnomalyMonitor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cost anomaly monitor. 
module Network.AWS.CostExplorer.DeleteAnomalyMonitor
    (
    -- * Creating a request
      DeleteAnomalyMonitor (..)
    , mkDeleteAnomalyMonitor
    -- ** Request lenses
    , damMonitorArn

    -- * Destructuring the response
    , DeleteAnomalyMonitorResponse (..)
    , mkDeleteAnomalyMonitorResponse
    -- ** Response lenses
    , damrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAnomalyMonitor' smart constructor.
newtype DeleteAnomalyMonitor = DeleteAnomalyMonitor'
  { monitorArn :: Types.MonitorArn
    -- ^ The unique identifier of the cost anomaly monitor that you want to delete. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnomalyMonitor' value with any optional fields omitted.
mkDeleteAnomalyMonitor
    :: Types.MonitorArn -- ^ 'monitorArn'
    -> DeleteAnomalyMonitor
mkDeleteAnomalyMonitor monitorArn
  = DeleteAnomalyMonitor'{monitorArn}

-- | The unique identifier of the cost anomaly monitor that you want to delete. 
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damMonitorArn :: Lens.Lens' DeleteAnomalyMonitor Types.MonitorArn
damMonitorArn = Lens.field @"monitorArn"
{-# INLINEABLE damMonitorArn #-}
{-# DEPRECATED monitorArn "Use generic-lens or generic-optics with 'monitorArn' instead"  #-}

instance Core.ToQuery DeleteAnomalyMonitor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAnomalyMonitor where
        toHeaders DeleteAnomalyMonitor{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.DeleteAnomalyMonitor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAnomalyMonitor where
        toJSON DeleteAnomalyMonitor{..}
          = Core.object
              (Core.catMaybes [Core.Just ("MonitorArn" Core..= monitorArn)])

instance Core.AWSRequest DeleteAnomalyMonitor where
        type Rs DeleteAnomalyMonitor = DeleteAnomalyMonitorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteAnomalyMonitorResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAnomalyMonitorResponse' smart constructor.
newtype DeleteAnomalyMonitorResponse = DeleteAnomalyMonitorResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnomalyMonitorResponse' value with any optional fields omitted.
mkDeleteAnomalyMonitorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAnomalyMonitorResponse
mkDeleteAnomalyMonitorResponse responseStatus
  = DeleteAnomalyMonitorResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrrsResponseStatus :: Lens.Lens' DeleteAnomalyMonitorResponse Core.Int
damrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE damrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
