{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.CreateAnomalyMonitor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cost anomaly detection monitor with the requested type and monitor specification. 
module Network.AWS.CostExplorer.CreateAnomalyMonitor
    (
    -- * Creating a request
      CreateAnomalyMonitor (..)
    , mkCreateAnomalyMonitor
    -- ** Request lenses
    , camAnomalyMonitor

    -- * Destructuring the response
    , CreateAnomalyMonitorResponse (..)
    , mkCreateAnomalyMonitorResponse
    -- ** Response lenses
    , camrrsMonitorArn
    , camrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAnomalyMonitor' smart constructor.
newtype CreateAnomalyMonitor = CreateAnomalyMonitor'
  { anomalyMonitor :: Types.AnomalyMonitor
    -- ^ The cost anomaly detection monitor object that you want to create.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAnomalyMonitor' value with any optional fields omitted.
mkCreateAnomalyMonitor
    :: Types.AnomalyMonitor -- ^ 'anomalyMonitor'
    -> CreateAnomalyMonitor
mkCreateAnomalyMonitor anomalyMonitor
  = CreateAnomalyMonitor'{anomalyMonitor}

-- | The cost anomaly detection monitor object that you want to create.
--
-- /Note:/ Consider using 'anomalyMonitor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camAnomalyMonitor :: Lens.Lens' CreateAnomalyMonitor Types.AnomalyMonitor
camAnomalyMonitor = Lens.field @"anomalyMonitor"
{-# INLINEABLE camAnomalyMonitor #-}
{-# DEPRECATED anomalyMonitor "Use generic-lens or generic-optics with 'anomalyMonitor' instead"  #-}

instance Core.ToQuery CreateAnomalyMonitor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAnomalyMonitor where
        toHeaders CreateAnomalyMonitor{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.CreateAnomalyMonitor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateAnomalyMonitor where
        toJSON CreateAnomalyMonitor{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AnomalyMonitor" Core..= anomalyMonitor)])

instance Core.AWSRequest CreateAnomalyMonitor where
        type Rs CreateAnomalyMonitor = CreateAnomalyMonitorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAnomalyMonitorResponse' Core.<$>
                   (x Core..: "MonitorArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAnomalyMonitorResponse' smart constructor.
data CreateAnomalyMonitorResponse = CreateAnomalyMonitorResponse'
  { monitorArn :: Types.GenericString
    -- ^ The unique identifier of your newly created cost anomaly detection monitor.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAnomalyMonitorResponse' value with any optional fields omitted.
mkCreateAnomalyMonitorResponse
    :: Types.GenericString -- ^ 'monitorArn'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateAnomalyMonitorResponse
mkCreateAnomalyMonitorResponse monitorArn responseStatus
  = CreateAnomalyMonitorResponse'{monitorArn, responseStatus}

-- | The unique identifier of your newly created cost anomaly detection monitor.
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camrrsMonitorArn :: Lens.Lens' CreateAnomalyMonitorResponse Types.GenericString
camrrsMonitorArn = Lens.field @"monitorArn"
{-# INLINEABLE camrrsMonitorArn #-}
{-# DEPRECATED monitorArn "Use generic-lens or generic-optics with 'monitorArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
camrrsResponseStatus :: Lens.Lens' CreateAnomalyMonitorResponse Core.Int
camrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE camrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
