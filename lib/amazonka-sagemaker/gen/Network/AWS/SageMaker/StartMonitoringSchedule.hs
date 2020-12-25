{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StartMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a previously stopped monitoring schedule.
module Network.AWS.SageMaker.StartMonitoringSchedule
  ( -- * Creating a request
    StartMonitoringSchedule (..),
    mkStartMonitoringSchedule,

    -- ** Request lenses
    sMonitoringScheduleName,

    -- * Destructuring the response
    StartMonitoringScheduleResponse (..),
    mkStartMonitoringScheduleResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStartMonitoringSchedule' smart constructor.
newtype StartMonitoringSchedule = StartMonitoringSchedule'
  { -- | The name of the schedule to start.
    monitoringScheduleName :: Types.MonitoringScheduleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartMonitoringSchedule' value with any optional fields omitted.
mkStartMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Types.MonitoringScheduleName ->
  StartMonitoringSchedule
mkStartMonitoringSchedule monitoringScheduleName =
  StartMonitoringSchedule' {monitoringScheduleName}

-- | The name of the schedule to start.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMonitoringScheduleName :: Lens.Lens' StartMonitoringSchedule Types.MonitoringScheduleName
sMonitoringScheduleName = Lens.field @"monitoringScheduleName"
{-# DEPRECATED sMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

instance Core.FromJSON StartMonitoringSchedule where
  toJSON StartMonitoringSchedule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("MonitoringScheduleName" Core..= monitoringScheduleName)
          ]
      )

instance Core.AWSRequest StartMonitoringSchedule where
  type Rs StartMonitoringSchedule = StartMonitoringScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.StartMonitoringSchedule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StartMonitoringScheduleResponse'

-- | /See:/ 'mkStartMonitoringScheduleResponse' smart constructor.
data StartMonitoringScheduleResponse = StartMonitoringScheduleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMonitoringScheduleResponse' value with any optional fields omitted.
mkStartMonitoringScheduleResponse ::
  StartMonitoringScheduleResponse
mkStartMonitoringScheduleResponse =
  StartMonitoringScheduleResponse'
