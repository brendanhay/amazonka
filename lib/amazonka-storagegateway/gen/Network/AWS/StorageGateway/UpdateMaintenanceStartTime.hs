{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateMaintenanceStartTime (..)
    , mkUpdateMaintenanceStartTime
    -- ** Request lenses
    , umstGatewayARN
    , umstHourOfDay
    , umstMinuteOfHour
    , umstDayOfMonth
    , umstDayOfWeek

    -- * Destructuring the response
    , UpdateMaintenanceStartTimeResponse (..)
    , mkUpdateMaintenanceStartTimeResponse
    -- ** Response lenses
    , umstrrsGatewayARN
    , umstrrsResponseStatus
    ) where

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
  { gatewayARN :: Types.GatewayARN
  , hourOfDay :: Core.Natural
    -- ^ The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (00 to 23). The hour of the day is in the time zone of the gateway.
  , minuteOfHour :: Core.Natural
    -- ^ The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (00 to 59). The minute of the hour is in the time zone of the gateway.
  , dayOfMonth :: Core.Maybe Core.Natural
    -- ^ The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
  , dayOfWeek :: Core.Maybe Core.Natural
    -- ^ The day of the week component of the maintenance start time week represented as an ordinal number from 0 to 6, where 0 represents Sunday and 6 Saturday.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMaintenanceStartTime' value with any optional fields omitted.
mkUpdateMaintenanceStartTime
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Core.Natural -- ^ 'hourOfDay'
    -> Core.Natural -- ^ 'minuteOfHour'
    -> UpdateMaintenanceStartTime
mkUpdateMaintenanceStartTime gatewayARN hourOfDay minuteOfHour
  = UpdateMaintenanceStartTime'{gatewayARN, hourOfDay, minuteOfHour,
                                dayOfMonth = Core.Nothing, dayOfWeek = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstGatewayARN :: Lens.Lens' UpdateMaintenanceStartTime Types.GatewayARN
umstGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE umstGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (00 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'hourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstHourOfDay :: Lens.Lens' UpdateMaintenanceStartTime Core.Natural
umstHourOfDay = Lens.field @"hourOfDay"
{-# INLINEABLE umstHourOfDay #-}
{-# DEPRECATED hourOfDay "Use generic-lens or generic-optics with 'hourOfDay' instead"  #-}

-- | The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (00 to 59). The minute of the hour is in the time zone of the gateway.
--
-- /Note:/ Consider using 'minuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstMinuteOfHour :: Lens.Lens' UpdateMaintenanceStartTime Core.Natural
umstMinuteOfHour = Lens.field @"minuteOfHour"
{-# INLINEABLE umstMinuteOfHour #-}
{-# DEPRECATED minuteOfHour "Use generic-lens or generic-optics with 'minuteOfHour' instead"  #-}

-- | The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstDayOfMonth :: Lens.Lens' UpdateMaintenanceStartTime (Core.Maybe Core.Natural)
umstDayOfMonth = Lens.field @"dayOfMonth"
{-# INLINEABLE umstDayOfMonth #-}
{-# DEPRECATED dayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead"  #-}

-- | The day of the week component of the maintenance start time week represented as an ordinal number from 0 to 6, where 0 represents Sunday and 6 Saturday.
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstDayOfWeek :: Lens.Lens' UpdateMaintenanceStartTime (Core.Maybe Core.Natural)
umstDayOfWeek = Lens.field @"dayOfWeek"
{-# INLINEABLE umstDayOfWeek #-}
{-# DEPRECATED dayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead"  #-}

instance Core.ToQuery UpdateMaintenanceStartTime where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMaintenanceStartTime where
        toHeaders UpdateMaintenanceStartTime{..}
          = Core.pure
              ("X-Amz-Target",
               "StorageGateway_20130630.UpdateMaintenanceStartTime")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateMaintenanceStartTime where
        toJSON UpdateMaintenanceStartTime{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("HourOfDay" Core..= hourOfDay),
                  Core.Just ("MinuteOfHour" Core..= minuteOfHour),
                  ("DayOfMonth" Core..=) Core.<$> dayOfMonth,
                  ("DayOfWeek" Core..=) Core.<$> dayOfWeek])

instance Core.AWSRequest UpdateMaintenanceStartTime where
        type Rs UpdateMaintenanceStartTime =
             UpdateMaintenanceStartTimeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateMaintenanceStartTimeResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway whose maintenance start time is updated.
--
-- /See:/ 'mkUpdateMaintenanceStartTimeResponse' smart constructor.
data UpdateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMaintenanceStartTimeResponse' value with any optional fields omitted.
mkUpdateMaintenanceStartTimeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateMaintenanceStartTimeResponse
mkUpdateMaintenanceStartTimeResponse responseStatus
  = UpdateMaintenanceStartTimeResponse'{gatewayARN = Core.Nothing,
                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstrrsGatewayARN :: Lens.Lens' UpdateMaintenanceStartTimeResponse (Core.Maybe Types.GatewayARN)
umstrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE umstrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstrrsResponseStatus :: Lens.Lens' UpdateMaintenanceStartTimeResponse Core.Int
umstrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE umstrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
