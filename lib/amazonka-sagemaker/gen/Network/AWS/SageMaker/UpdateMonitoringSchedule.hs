{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created schedule.
module Network.AWS.SageMaker.UpdateMonitoringSchedule
  ( -- * Creating a request
    UpdateMonitoringSchedule (..),
    mkUpdateMonitoringSchedule,

    -- ** Request lenses
    umsMonitoringScheduleName,
    umsMonitoringScheduleConfig,

    -- * Destructuring the response
    UpdateMonitoringScheduleResponse (..),
    mkUpdateMonitoringScheduleResponse,

    -- ** Response lenses
    umsrrsMonitoringScheduleArn,
    umsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateMonitoringSchedule' smart constructor.
data UpdateMonitoringSchedule = UpdateMonitoringSchedule'
  { -- | The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
    monitoringScheduleName :: Types.MonitoringScheduleName,
    -- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
    monitoringScheduleConfig :: Types.MonitoringScheduleConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMonitoringSchedule' value with any optional fields omitted.
mkUpdateMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Types.MonitoringScheduleName ->
  -- | 'monitoringScheduleConfig'
  Types.MonitoringScheduleConfig ->
  UpdateMonitoringSchedule
mkUpdateMonitoringSchedule
  monitoringScheduleName
  monitoringScheduleConfig =
    UpdateMonitoringSchedule'
      { monitoringScheduleName,
        monitoringScheduleConfig
      }

-- | The name of the monitoring schedule. The name must be unique within an AWS Region within an AWS account.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsMonitoringScheduleName :: Lens.Lens' UpdateMonitoringSchedule Types.MonitoringScheduleName
umsMonitoringScheduleName = Lens.field @"monitoringScheduleName"
{-# DEPRECATED umsMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
--
-- /Note:/ Consider using 'monitoringScheduleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsMonitoringScheduleConfig :: Lens.Lens' UpdateMonitoringSchedule Types.MonitoringScheduleConfig
umsMonitoringScheduleConfig = Lens.field @"monitoringScheduleConfig"
{-# DEPRECATED umsMonitoringScheduleConfig "Use generic-lens or generic-optics with 'monitoringScheduleConfig' instead." #-}

instance Core.FromJSON UpdateMonitoringSchedule where
  toJSON UpdateMonitoringSchedule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("MonitoringScheduleName" Core..= monitoringScheduleName),
            Core.Just
              ("MonitoringScheduleConfig" Core..= monitoringScheduleConfig)
          ]
      )

instance Core.AWSRequest UpdateMonitoringSchedule where
  type Rs UpdateMonitoringSchedule = UpdateMonitoringScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateMonitoringSchedule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMonitoringScheduleResponse'
            Core.<$> (x Core..: "MonitoringScheduleArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateMonitoringScheduleResponse' smart constructor.
data UpdateMonitoringScheduleResponse = UpdateMonitoringScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Types.MonitoringScheduleArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMonitoringScheduleResponse' value with any optional fields omitted.
mkUpdateMonitoringScheduleResponse ::
  -- | 'monitoringScheduleArn'
  Types.MonitoringScheduleArn ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateMonitoringScheduleResponse
mkUpdateMonitoringScheduleResponse
  monitoringScheduleArn
  responseStatus =
    UpdateMonitoringScheduleResponse'
      { monitoringScheduleArn,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsrrsMonitoringScheduleArn :: Lens.Lens' UpdateMonitoringScheduleResponse Types.MonitoringScheduleArn
umsrrsMonitoringScheduleArn = Lens.field @"monitoringScheduleArn"
{-# DEPRECATED umsrrsMonitoringScheduleArn "Use generic-lens or generic-optics with 'monitoringScheduleArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umsrrsResponseStatus :: Lens.Lens' UpdateMonitoringScheduleResponse Core.Int
umsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
