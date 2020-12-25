{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a monitoring schedule. Also stops the schedule had not already been stopped. This does not delete the job execution history of the monitoring schedule.
module Network.AWS.SageMaker.DeleteMonitoringSchedule
  ( -- * Creating a request
    DeleteMonitoringSchedule (..),
    mkDeleteMonitoringSchedule,

    -- ** Request lenses
    dMonitoringScheduleName,

    -- * Destructuring the response
    DeleteMonitoringScheduleResponse (..),
    mkDeleteMonitoringScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteMonitoringSchedule' smart constructor.
newtype DeleteMonitoringSchedule = DeleteMonitoringSchedule'
  { -- | The name of the monitoring schedule to delete.
    monitoringScheduleName :: Types.MonitoringScheduleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMonitoringSchedule' value with any optional fields omitted.
mkDeleteMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Types.MonitoringScheduleName ->
  DeleteMonitoringSchedule
mkDeleteMonitoringSchedule monitoringScheduleName =
  DeleteMonitoringSchedule' {monitoringScheduleName}

-- | The name of the monitoring schedule to delete.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMonitoringScheduleName :: Lens.Lens' DeleteMonitoringSchedule Types.MonitoringScheduleName
dMonitoringScheduleName = Lens.field @"monitoringScheduleName"
{-# DEPRECATED dMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

instance Core.FromJSON DeleteMonitoringSchedule where
  toJSON DeleteMonitoringSchedule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("MonitoringScheduleName" Core..= monitoringScheduleName)
          ]
      )

instance Core.AWSRequest DeleteMonitoringSchedule where
  type Rs DeleteMonitoringSchedule = DeleteMonitoringScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteMonitoringSchedule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteMonitoringScheduleResponse'

-- | /See:/ 'mkDeleteMonitoringScheduleResponse' smart constructor.
data DeleteMonitoringScheduleResponse = DeleteMonitoringScheduleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMonitoringScheduleResponse' value with any optional fields omitted.
mkDeleteMonitoringScheduleResponse ::
  DeleteMonitoringScheduleResponse
mkDeleteMonitoringScheduleResponse =
  DeleteMonitoringScheduleResponse'
