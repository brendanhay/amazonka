{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeMaintenanceStartTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns your gateway's weekly maintenance start time including the day and time of the week. Note that values are in terms of the gateway's time zone.
module Network.AWS.StorageGateway.DescribeMaintenanceStartTime
  ( -- * Creating a request
    DescribeMaintenanceStartTime (..),
    mkDescribeMaintenanceStartTime,

    -- ** Request lenses
    dmstGatewayARN,

    -- * Destructuring the response
    DescribeMaintenanceStartTimeResponse (..),
    mkDescribeMaintenanceStartTimeResponse,

    -- ** Response lenses
    dmstrsGatewayARN,
    dmstrsMinuteOfHour,
    dmstrsDayOfMonth,
    dmstrsHourOfDay,
    dmstrsTimezone,
    dmstrsDayOfWeek,
    dmstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'mkDescribeMaintenanceStartTime' smart constructor.
newtype DescribeMaintenanceStartTime = DescribeMaintenanceStartTime'
  { gatewayARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMaintenanceStartTime' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
mkDescribeMaintenanceStartTime ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeMaintenanceStartTime
mkDescribeMaintenanceStartTime pGatewayARN_ =
  DescribeMaintenanceStartTime' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmstGatewayARN :: Lens.Lens' DescribeMaintenanceStartTime Lude.Text
dmstGatewayARN = Lens.lens (gatewayARN :: DescribeMaintenanceStartTime -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeMaintenanceStartTime)
{-# DEPRECATED dmstGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeMaintenanceStartTime where
  type
    Rs DescribeMaintenanceStartTime =
      DescribeMaintenanceStartTimeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMaintenanceStartTimeResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "MinuteOfHour")
            Lude.<*> (x Lude..?> "DayOfMonth")
            Lude.<*> (x Lude..?> "HourOfDay")
            Lude.<*> (x Lude..?> "Timezone")
            Lude.<*> (x Lude..?> "DayOfWeek")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMaintenanceStartTime where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeMaintenanceStartTime" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeMaintenanceStartTime where
  toJSON DescribeMaintenanceStartTime' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeMaintenanceStartTime where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeMaintenanceStartTime where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
--
--     * 'DescribeMaintenanceStartTimeOutput$DayOfMonth'
--
--
--     * 'DescribeMaintenanceStartTimeOutput$DayOfWeek'
--
--
--     * 'DescribeMaintenanceStartTimeOutput$HourOfDay'
--
--
--     * 'DescribeMaintenanceStartTimeOutput$MinuteOfHour'
--
--
--     * 'DescribeMaintenanceStartTimeOutput$Timezone'
--
--
--
-- /See:/ 'mkDescribeMaintenanceStartTimeResponse' smart constructor.
data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse'
  { gatewayARN ::
      Lude.Maybe
        Lude.Text,
    minuteOfHour ::
      Lude.Maybe
        Lude.Natural,
    dayOfMonth ::
      Lude.Maybe
        Lude.Natural,
    hourOfDay ::
      Lude.Maybe
        Lude.Natural,
    timezone ::
      Lude.Maybe
        Lude.Text,
    dayOfWeek ::
      Lude.Maybe
        Lude.Natural,
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

-- | Creates a value of 'DescribeMaintenanceStartTimeResponse' with the minimum fields required to make a request.
--
-- * 'dayOfMonth' - The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
-- * 'dayOfWeek' - An ordinal number between 0 and 6 that represents the day of the week, where 0 represents Sunday and 6 represents Saturday. The day of week is in the time zone of the gateway.
-- * 'gatewayARN' - Undocumented field.
-- * 'hourOfDay' - The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
-- * 'minuteOfHour' - The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (0 to 59). The minute of the hour is in the time zone of the gateway.
-- * 'responseStatus' - The response status code.
-- * 'timezone' - A value that indicates the time zone that is set for the gateway. The start time and day of week specified should be in the time zone of the gateway.
mkDescribeMaintenanceStartTimeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMaintenanceStartTimeResponse
mkDescribeMaintenanceStartTimeResponse pResponseStatus_ =
  DescribeMaintenanceStartTimeResponse'
    { gatewayARN = Lude.Nothing,
      minuteOfHour = Lude.Nothing,
      dayOfMonth = Lude.Nothing,
      hourOfDay = Lude.Nothing,
      timezone = Lude.Nothing,
      dayOfWeek = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmstrsGatewayARN :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Lude.Maybe Lude.Text)
dmstrsGatewayARN = Lens.lens (gatewayARN :: DescribeMaintenanceStartTimeResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeMaintenanceStartTimeResponse)
{-# DEPRECATED dmstrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The minute component of the maintenance start time represented as /mm/ , where /mm/ is the minute (0 to 59). The minute of the hour is in the time zone of the gateway.
--
-- /Note:/ Consider using 'minuteOfHour' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmstrsMinuteOfHour :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Lude.Maybe Lude.Natural)
dmstrsMinuteOfHour = Lens.lens (minuteOfHour :: DescribeMaintenanceStartTimeResponse -> Lude.Maybe Lude.Natural) (\s a -> s {minuteOfHour = a} :: DescribeMaintenanceStartTimeResponse)
{-# DEPRECATED dmstrsMinuteOfHour "Use generic-lens or generic-optics with 'minuteOfHour' instead." #-}

-- | The day of the month component of the maintenance start time represented as an ordinal number from 1 to 28, where 1 represents the first day of the month and 28 represents the last day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmstrsDayOfMonth :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Lude.Maybe Lude.Natural)
dmstrsDayOfMonth = Lens.lens (dayOfMonth :: DescribeMaintenanceStartTimeResponse -> Lude.Maybe Lude.Natural) (\s a -> s {dayOfMonth = a} :: DescribeMaintenanceStartTimeResponse)
{-# DEPRECATED dmstrsDayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead." #-}

-- | The hour component of the maintenance start time represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'hourOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmstrsHourOfDay :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Lude.Maybe Lude.Natural)
dmstrsHourOfDay = Lens.lens (hourOfDay :: DescribeMaintenanceStartTimeResponse -> Lude.Maybe Lude.Natural) (\s a -> s {hourOfDay = a} :: DescribeMaintenanceStartTimeResponse)
{-# DEPRECATED dmstrsHourOfDay "Use generic-lens or generic-optics with 'hourOfDay' instead." #-}

-- | A value that indicates the time zone that is set for the gateway. The start time and day of week specified should be in the time zone of the gateway.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmstrsTimezone :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Lude.Maybe Lude.Text)
dmstrsTimezone = Lens.lens (timezone :: DescribeMaintenanceStartTimeResponse -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: DescribeMaintenanceStartTimeResponse)
{-# DEPRECATED dmstrsTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | An ordinal number between 0 and 6 that represents the day of the week, where 0 represents Sunday and 6 represents Saturday. The day of week is in the time zone of the gateway.
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmstrsDayOfWeek :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Lude.Maybe Lude.Natural)
dmstrsDayOfWeek = Lens.lens (dayOfWeek :: DescribeMaintenanceStartTimeResponse -> Lude.Maybe Lude.Natural) (\s a -> s {dayOfWeek = a} :: DescribeMaintenanceStartTimeResponse)
{-# DEPRECATED dmstrsDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmstrsResponseStatus :: Lens.Lens' DescribeMaintenanceStartTimeResponse Lude.Int
dmstrsResponseStatus = Lens.lens (responseStatus :: DescribeMaintenanceStartTimeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMaintenanceStartTimeResponse)
{-# DEPRECATED dmstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
