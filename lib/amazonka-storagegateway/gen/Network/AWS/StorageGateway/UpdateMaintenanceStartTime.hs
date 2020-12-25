{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateMaintenanceStartTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway's weekly maintenance start time information, including day and time of the week. The maintenance time is the time in your gateway's time zone.
module Network.AWS.StorageGateway.UpdateMaintenanceStartTime
  ( -- * Creating a request
    UpdateMaintenanceStartTime (..),
    mkUpdateMaintenanceStartTime,

    -- ** Request lenses
    umstGatewayARN,
    umstHourOfDay,
    umstMinuteOfHour,
    umstDayOfMonth,
    umstDayOfWeek,

    -- * Destructuring the response
    UpdateMaintenanceStartTimeResponse (..),
    mkUpdateMaintenanceStartTimeResponse,

    -- ** Response lenses
    umstrrsGatewayARN,
    umstrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the following fields:
--
--
--     * 'UpdateMaintenanceStartTimeInput$DayOfMonth'
--
--
--     * 'UpdateMaintenanceStartTimeInput$DayOfWeek'
--
--
--     * 'UpdateMaintenanceStartTimeInput$HourOfDay'
--
--
--     * 'UpdateMaintenanceStartTimeInput$MinuteOfHour'
--
--
--
-- /See:/ 'mkUpdateMaintenanceStartTime' smart constructor.
data UpdateMaintenanceStartTime = UpdateMaintenanceStartTime'
  { gatewayARN :: Types.GatewayARN,
    -- | The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (00 to 23). The hour of the day is in the time zone of the gateway.
    hourOfDay :: Core.Natural,
    -- | The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (00 to 59). The minute of the hour is in the time zone of the gateway.
    minuteOfHour :: Core.Natural,
    -- | The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
    dayOfMonth :: Core.Maybe Core.Natural,
    -- | The day of the week component of the maintenance start time week represented as an ordinal number from 0 to 6, where 0 represents Sunday and 6 Saturday.
    dayOfWeek :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMaintenanceStartTime' value with any optional fields omitted.
mkUpdateMaintenanceStartTime ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'hourOfDay'
  Core.Natural ->
  -- | 'minuteOfHour'
  Core.Natural ->
  UpdateMaintenanceStartTime
mkUpdateMaintenanceStartTime gatewayARN hourOfDay minuteOfHour =
  UpdateMaintenanceStartTime'
    { gatewayARN,
      hourOfDay,
      minuteOfHour,
      dayOfMonth = Core.Nothing,
      dayOfWeek = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstGatewayARN :: Lens.Lens' UpdateMaintenanceStartTime Types.GatewayARN
umstGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED umstGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (00 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'hourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstHourOfDay :: Lens.Lens' UpdateMaintenanceStartTime Core.Natural
umstHourOfDay = Lens.field @"hourOfDay"
{-# DEPRECATED umstHourOfDay "Use generic-lens or generic-optics with 'hourOfDay' instead." #-}

-- | The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (00 to 59). The minute of the hour is in the time zone of the gateway.
--
-- /Note:/ Consider using 'minuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstMinuteOfHour :: Lens.Lens' UpdateMaintenanceStartTime Core.Natural
umstMinuteOfHour = Lens.field @"minuteOfHour"
{-# DEPRECATED umstMinuteOfHour "Use generic-lens or generic-optics with 'minuteOfHour' instead." #-}

-- | The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstDayOfMonth :: Lens.Lens' UpdateMaintenanceStartTime (Core.Maybe Core.Natural)
umstDayOfMonth = Lens.field @"dayOfMonth"
{-# DEPRECATED umstDayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead." #-}

-- | The day of the week component of the maintenance start time week represented as an ordinal number from 0 to 6, where 0 represents Sunday and 6 Saturday.
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstDayOfWeek :: Lens.Lens' UpdateMaintenanceStartTime (Core.Maybe Core.Natural)
umstDayOfWeek = Lens.field @"dayOfWeek"
{-# DEPRECATED umstDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

instance Core.FromJSON UpdateMaintenanceStartTime where
  toJSON UpdateMaintenanceStartTime {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("HourOfDay" Core..= hourOfDay),
            Core.Just ("MinuteOfHour" Core..= minuteOfHour),
            ("DayOfMonth" Core..=) Core.<$> dayOfMonth,
            ("DayOfWeek" Core..=) Core.<$> dayOfWeek
          ]
      )

instance Core.AWSRequest UpdateMaintenanceStartTime where
  type
    Rs UpdateMaintenanceStartTime =
      UpdateMaintenanceStartTimeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.UpdateMaintenanceStartTime"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMaintenanceStartTimeResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway whose maintenance start time is updated.
--
-- /See:/ 'mkUpdateMaintenanceStartTimeResponse' smart constructor.
data UpdateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMaintenanceStartTimeResponse' value with any optional fields omitted.
mkUpdateMaintenanceStartTimeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMaintenanceStartTimeResponse
mkUpdateMaintenanceStartTimeResponse responseStatus =
  UpdateMaintenanceStartTimeResponse'
    { gatewayARN = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstrrsGatewayARN :: Lens.Lens' UpdateMaintenanceStartTimeResponse (Core.Maybe Types.GatewayARN)
umstrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED umstrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstrrsResponseStatus :: Lens.Lens' UpdateMaintenanceStartTimeResponse Core.Int
umstrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umstrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
