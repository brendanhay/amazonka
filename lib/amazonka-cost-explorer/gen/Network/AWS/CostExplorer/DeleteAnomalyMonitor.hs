{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteAnomalyMonitor (..),
    mkDeleteAnomalyMonitor,

    -- ** Request lenses
    damMonitorArn,

    -- * Destructuring the response
    DeleteAnomalyMonitorResponse (..),
    mkDeleteAnomalyMonitorResponse,

    -- ** Response lenses
    damrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAnomalyMonitor' smart constructor.
newtype DeleteAnomalyMonitor = DeleteAnomalyMonitor'
  { -- | The unique identifier of the cost anomaly monitor that you want to delete.
    monitorArn :: Types.MonitorArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnomalyMonitor' value with any optional fields omitted.
mkDeleteAnomalyMonitor ::
  -- | 'monitorArn'
  Types.MonitorArn ->
  DeleteAnomalyMonitor
mkDeleteAnomalyMonitor monitorArn =
  DeleteAnomalyMonitor' {monitorArn}

-- | The unique identifier of the cost anomaly monitor that you want to delete.
--
-- /Note:/ Consider using 'monitorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damMonitorArn :: Lens.Lens' DeleteAnomalyMonitor Types.MonitorArn
damMonitorArn = Lens.field @"monitorArn"
{-# DEPRECATED damMonitorArn "Use generic-lens or generic-optics with 'monitorArn' instead." #-}

instance Core.FromJSON DeleteAnomalyMonitor where
  toJSON DeleteAnomalyMonitor {..} =
    Core.object
      (Core.catMaybes [Core.Just ("MonitorArn" Core..= monitorArn)])

instance Core.AWSRequest DeleteAnomalyMonitor where
  type Rs DeleteAnomalyMonitor = DeleteAnomalyMonitorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSInsightsIndexService.DeleteAnomalyMonitor")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAnomalyMonitorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAnomalyMonitorResponse' smart constructor.
newtype DeleteAnomalyMonitorResponse = DeleteAnomalyMonitorResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAnomalyMonitorResponse' value with any optional fields omitted.
mkDeleteAnomalyMonitorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAnomalyMonitorResponse
mkDeleteAnomalyMonitorResponse responseStatus =
  DeleteAnomalyMonitorResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrrsResponseStatus :: Lens.Lens' DeleteAnomalyMonitorResponse Core.Int
damrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED damrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
