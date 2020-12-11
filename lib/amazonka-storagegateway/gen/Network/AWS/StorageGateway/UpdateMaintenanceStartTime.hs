{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    umstDayOfMonth,
    umstDayOfWeek,
    umstGatewayARN,
    umstHourOfDay,
    umstMinuteOfHour,

    -- * Destructuring the response
    UpdateMaintenanceStartTimeResponse (..),
    mkUpdateMaintenanceStartTimeResponse,

    -- ** Response lenses
    umstrsGatewayARN,
    umstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

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
  { dayOfMonth ::
      Lude.Maybe Lude.Natural,
    dayOfWeek :: Lude.Maybe Lude.Natural,
    gatewayARN :: Lude.Text,
    hourOfDay :: Lude.Natural,
    minuteOfHour :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMaintenanceStartTime' with the minimum fields required to make a request.
--
-- * 'dayOfMonth' - The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
-- * 'dayOfWeek' - The day of the week component of the maintenance start time week represented as an ordinal number from 0 to 6, where 0 represents Sunday and 6 Saturday.
-- * 'gatewayARN' - Undocumented field.
-- * 'hourOfDay' - The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (00 to 23). The hour of the day is in the time zone of the gateway.
-- * 'minuteOfHour' - The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (00 to 59). The minute of the hour is in the time zone of the gateway.
mkUpdateMaintenanceStartTime ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'hourOfDay'
  Lude.Natural ->
  -- | 'minuteOfHour'
  Lude.Natural ->
  UpdateMaintenanceStartTime
mkUpdateMaintenanceStartTime
  pGatewayARN_
  pHourOfDay_
  pMinuteOfHour_ =
    UpdateMaintenanceStartTime'
      { dayOfMonth = Lude.Nothing,
        dayOfWeek = Lude.Nothing,
        gatewayARN = pGatewayARN_,
        hourOfDay = pHourOfDay_,
        minuteOfHour = pMinuteOfHour_
      }

-- | The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstDayOfMonth :: Lens.Lens' UpdateMaintenanceStartTime (Lude.Maybe Lude.Natural)
umstDayOfMonth = Lens.lens (dayOfMonth :: UpdateMaintenanceStartTime -> Lude.Maybe Lude.Natural) (\s a -> s {dayOfMonth = a} :: UpdateMaintenanceStartTime)
{-# DEPRECATED umstDayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead." #-}

-- | The day of the week component of the maintenance start time week represented as an ordinal number from 0 to 6, where 0 represents Sunday and 6 Saturday.
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstDayOfWeek :: Lens.Lens' UpdateMaintenanceStartTime (Lude.Maybe Lude.Natural)
umstDayOfWeek = Lens.lens (dayOfWeek :: UpdateMaintenanceStartTime -> Lude.Maybe Lude.Natural) (\s a -> s {dayOfWeek = a} :: UpdateMaintenanceStartTime)
{-# DEPRECATED umstDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstGatewayARN :: Lens.Lens' UpdateMaintenanceStartTime Lude.Text
umstGatewayARN = Lens.lens (gatewayARN :: UpdateMaintenanceStartTime -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateMaintenanceStartTime)
{-# DEPRECATED umstGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (00 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'hourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstHourOfDay :: Lens.Lens' UpdateMaintenanceStartTime Lude.Natural
umstHourOfDay = Lens.lens (hourOfDay :: UpdateMaintenanceStartTime -> Lude.Natural) (\s a -> s {hourOfDay = a} :: UpdateMaintenanceStartTime)
{-# DEPRECATED umstHourOfDay "Use generic-lens or generic-optics with 'hourOfDay' instead." #-}

-- | The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (00 to 59). The minute of the hour is in the time zone of the gateway.
--
-- /Note:/ Consider using 'minuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstMinuteOfHour :: Lens.Lens' UpdateMaintenanceStartTime Lude.Natural
umstMinuteOfHour = Lens.lens (minuteOfHour :: UpdateMaintenanceStartTime -> Lude.Natural) (\s a -> s {minuteOfHour = a} :: UpdateMaintenanceStartTime)
{-# DEPRECATED umstMinuteOfHour "Use generic-lens or generic-optics with 'minuteOfHour' instead." #-}

instance Lude.AWSRequest UpdateMaintenanceStartTime where
  type
    Rs UpdateMaintenanceStartTime =
      UpdateMaintenanceStartTimeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMaintenanceStartTimeResponse'
            Lude.<$> (x Lude..?> "GatewayARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMaintenanceStartTime where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateMaintenanceStartTime" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMaintenanceStartTime where
  toJSON UpdateMaintenanceStartTime' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DayOfMonth" Lude..=) Lude.<$> dayOfMonth,
            ("DayOfWeek" Lude..=) Lude.<$> dayOfWeek,
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("HourOfDay" Lude..= hourOfDay),
            Lude.Just ("MinuteOfHour" Lude..= minuteOfHour)
          ]
      )

instance Lude.ToPath UpdateMaintenanceStartTime where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMaintenanceStartTime where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway whose maintenance start time is updated.
--
-- /See:/ 'mkUpdateMaintenanceStartTimeResponse' smart constructor.
data UpdateMaintenanceStartTimeResponse = UpdateMaintenanceStartTimeResponse'
  { gatewayARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMaintenanceStartTimeResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateMaintenanceStartTimeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMaintenanceStartTimeResponse
mkUpdateMaintenanceStartTimeResponse pResponseStatus_ =
  UpdateMaintenanceStartTimeResponse'
    { gatewayARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstrsGatewayARN :: Lens.Lens' UpdateMaintenanceStartTimeResponse (Lude.Maybe Lude.Text)
umstrsGatewayARN = Lens.lens (gatewayARN :: UpdateMaintenanceStartTimeResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateMaintenanceStartTimeResponse)
{-# DEPRECATED umstrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umstrsResponseStatus :: Lens.Lens' UpdateMaintenanceStartTimeResponse Lude.Int
umstrsResponseStatus = Lens.lens (responseStatus :: UpdateMaintenanceStartTimeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMaintenanceStartTimeResponse)
{-# DEPRECATED umstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
