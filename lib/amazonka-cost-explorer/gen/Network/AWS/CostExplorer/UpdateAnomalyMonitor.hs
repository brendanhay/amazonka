{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateAnomalyMonitor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing cost anomaly monitor. The changes made are applied going forward, and does not change anomalies detected in the past. 
module Network.AWS.CostExplorer.UpdateAnomalyMonitor
    (
    -- * Creating a request
      UpdateAnomalyMonitor (..)
    , mkUpdateAnomalyMonitor
    -- ** Request lenses
    , uamMonitorArn
    , uamMonitorName

    -- * Destructuring the response
    , UpdateAnomalyMonitorResponse (..)
    , mkUpdateAnomalyMonitorResponse
    -- ** Response lenses
    , uamrrsMonitorArn
    , uamrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAnomalyMonitor' smart constructor.
data UpdateAnomalyMonitor = UpdateAnomalyMonitor'
  { monitorArn :: Types.MonitorArn
    -- ^ Cost anomaly monitor Amazon Resource Names (ARNs). 
  , monitorName :: Core.Maybe Types.MonitorName
    -- ^ The new name for the cost anomaly monitor. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAnomalyMonitor' value with any optional fields omitted.
mkUpdateAnomalyMonitor
    :: Types.MonitorArn -- ^ 'monitorArn'
    -> UpdateAnomalyMonitor
mkUpdateAnomalyMonitor monitorArn
  = UpdateAnomalyMonitor'{monitorArn, monitorName = Core.Nothing}

-- | Cost anomaly monitor Amazon Resource Names (ARNs). 
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamMonitorArn :: Lens.Lens' UpdateAnomalyMonitor Types.MonitorArn
uamMonitorArn = Lens.field @"monitorArn"
{-# INLINEABLE uamMonitorArn #-}
{-# DEPRECATED monitorArn "Use generic-lens or generic-optics with 'monitorArn' instead"  #-}

-- | The new name for the cost anomaly monitor. 
--
-- /Note:/ Consider using 'monitorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamMonitorName :: Lens.Lens' UpdateAnomalyMonitor (Core.Maybe Types.MonitorName)
uamMonitorName = Lens.field @"monitorName"
{-# INLINEABLE uamMonitorName #-}
{-# DEPRECATED monitorName "Use generic-lens or generic-optics with 'monitorName' instead"  #-}

instance Core.ToQuery UpdateAnomalyMonitor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAnomalyMonitor where
        toHeaders UpdateAnomalyMonitor{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.UpdateAnomalyMonitor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateAnomalyMonitor where
        toJSON UpdateAnomalyMonitor{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MonitorArn" Core..= monitorArn),
                  ("MonitorName" Core..=) Core.<$> monitorName])

instance Core.AWSRequest UpdateAnomalyMonitor where
        type Rs UpdateAnomalyMonitor = UpdateAnomalyMonitorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateAnomalyMonitorResponse' Core.<$>
                   (x Core..: "MonitorArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAnomalyMonitorResponse' smart constructor.
data UpdateAnomalyMonitorResponse = UpdateAnomalyMonitorResponse'
  { monitorArn :: Types.GenericString
    -- ^ A cost anomaly monitor ARN. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAnomalyMonitorResponse' value with any optional fields omitted.
mkUpdateAnomalyMonitorResponse
    :: Types.GenericString -- ^ 'monitorArn'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateAnomalyMonitorResponse
mkUpdateAnomalyMonitorResponse monitorArn responseStatus
  = UpdateAnomalyMonitorResponse'{monitorArn, responseStatus}

-- | A cost anomaly monitor ARN. 
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamrrsMonitorArn :: Lens.Lens' UpdateAnomalyMonitorResponse Types.GenericString
uamrrsMonitorArn = Lens.field @"monitorArn"
{-# INLINEABLE uamrrsMonitorArn #-}
{-# DEPRECATED monitorArn "Use generic-lens or generic-optics with 'monitorArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamrrsResponseStatus :: Lens.Lens' UpdateAnomalyMonitorResponse Core.Int
uamrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uamrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
