{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateAnomalyMonitor (..),
    mkUpdateAnomalyMonitor,

    -- ** Request lenses
    uamMonitorArn,
    uamMonitorName,

    -- * Destructuring the response
    UpdateAnomalyMonitorResponse (..),
    mkUpdateAnomalyMonitorResponse,

    -- ** Response lenses
    uamrrsMonitorArn,
    uamrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAnomalyMonitor' smart constructor.
data UpdateAnomalyMonitor = UpdateAnomalyMonitor'
  { -- | Cost anomaly monitor Amazon Resource Names (ARNs).
    monitorArn :: Types.MonitorArn,
    -- | The new name for the cost anomaly monitor.
    monitorName :: Core.Maybe Types.MonitorName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAnomalyMonitor' value with any optional fields omitted.
mkUpdateAnomalyMonitor ::
  -- | 'monitorArn'
  Types.MonitorArn ->
  UpdateAnomalyMonitor
mkUpdateAnomalyMonitor monitorArn =
  UpdateAnomalyMonitor' {monitorArn, monitorName = Core.Nothing}

-- | Cost anomaly monitor Amazon Resource Names (ARNs).
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamMonitorArn :: Lens.Lens' UpdateAnomalyMonitor Types.MonitorArn
uamMonitorArn = Lens.field @"monitorArn"
{-# DEPRECATED uamMonitorArn "Use generic-lens or generic-optics with 'monitorArn' instead." #-}

-- | The new name for the cost anomaly monitor.
--
-- /Note:/ Consider using 'monitorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamMonitorName :: Lens.Lens' UpdateAnomalyMonitor (Core.Maybe Types.MonitorName)
uamMonitorName = Lens.field @"monitorName"
{-# DEPRECATED uamMonitorName "Use generic-lens or generic-optics with 'monitorName' instead." #-}

instance Core.FromJSON UpdateAnomalyMonitor where
  toJSON UpdateAnomalyMonitor {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MonitorArn" Core..= monitorArn),
            ("MonitorName" Core..=) Core.<$> monitorName
          ]
      )

instance Core.AWSRequest UpdateAnomalyMonitor where
  type Rs UpdateAnomalyMonitor = UpdateAnomalyMonitorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSInsightsIndexService.UpdateAnomalyMonitor")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnomalyMonitorResponse'
            Core.<$> (x Core..: "MonitorArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateAnomalyMonitorResponse' smart constructor.
data UpdateAnomalyMonitorResponse = UpdateAnomalyMonitorResponse'
  { -- | A cost anomaly monitor ARN.
    monitorArn :: Types.GenericString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAnomalyMonitorResponse' value with any optional fields omitted.
mkUpdateAnomalyMonitorResponse ::
  -- | 'monitorArn'
  Types.GenericString ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateAnomalyMonitorResponse
mkUpdateAnomalyMonitorResponse monitorArn responseStatus =
  UpdateAnomalyMonitorResponse' {monitorArn, responseStatus}

-- | A cost anomaly monitor ARN.
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamrrsMonitorArn :: Lens.Lens' UpdateAnomalyMonitorResponse Types.GenericString
uamrrsMonitorArn = Lens.field @"monitorArn"
{-# DEPRECATED uamrrsMonitorArn "Use generic-lens or generic-optics with 'monitorArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uamrrsResponseStatus :: Lens.Lens' UpdateAnomalyMonitorResponse Core.Int
uamrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uamrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
