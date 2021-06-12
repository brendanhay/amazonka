{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeMaintenanceStartTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns your gateway\'s weekly maintenance start time including the day
-- and time of the week. Note that values are in terms of the gateway\'s
-- time zone.
module Network.AWS.StorageGateway.DescribeMaintenanceStartTime
  ( -- * Creating a Request
    DescribeMaintenanceStartTime (..),
    newDescribeMaintenanceStartTime,

    -- * Request Lenses
    describeMaintenanceStartTime_gatewayARN,

    -- * Destructuring the Response
    DescribeMaintenanceStartTimeResponse (..),
    newDescribeMaintenanceStartTimeResponse,

    -- * Response Lenses
    describeMaintenanceStartTimeResponse_dayOfWeek,
    describeMaintenanceStartTimeResponse_dayOfMonth,
    describeMaintenanceStartTimeResponse_minuteOfHour,
    describeMaintenanceStartTimeResponse_timezone,
    describeMaintenanceStartTimeResponse_gatewayARN,
    describeMaintenanceStartTimeResponse_hourOfDay,
    describeMaintenanceStartTimeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'newDescribeMaintenanceStartTime' smart constructor.
data DescribeMaintenanceStartTime = DescribeMaintenanceStartTime'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceStartTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeMaintenanceStartTime_gatewayARN' - Undocumented member.
newDescribeMaintenanceStartTime ::
  -- | 'gatewayARN'
  Core.Text ->
  DescribeMaintenanceStartTime
newDescribeMaintenanceStartTime pGatewayARN_ =
  DescribeMaintenanceStartTime'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeMaintenanceStartTime_gatewayARN :: Lens.Lens' DescribeMaintenanceStartTime Core.Text
describeMaintenanceStartTime_gatewayARN = Lens.lens (\DescribeMaintenanceStartTime' {gatewayARN} -> gatewayARN) (\s@DescribeMaintenanceStartTime' {} a -> s {gatewayARN = a} :: DescribeMaintenanceStartTime)

instance Core.AWSRequest DescribeMaintenanceStartTime where
  type
    AWSResponse DescribeMaintenanceStartTime =
      DescribeMaintenanceStartTimeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceStartTimeResponse'
            Core.<$> (x Core..?> "DayOfWeek")
            Core.<*> (x Core..?> "DayOfMonth")
            Core.<*> (x Core..?> "MinuteOfHour")
            Core.<*> (x Core..?> "Timezone")
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (x Core..?> "HourOfDay")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMaintenanceStartTime

instance Core.NFData DescribeMaintenanceStartTime

instance Core.ToHeaders DescribeMaintenanceStartTime where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeMaintenanceStartTime" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeMaintenanceStartTime where
  toJSON DescribeMaintenanceStartTime' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath DescribeMaintenanceStartTime where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMaintenanceStartTime where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the following fields:
--
-- -   DescribeMaintenanceStartTimeOutput$DayOfMonth
--
-- -   DescribeMaintenanceStartTimeOutput$DayOfWeek
--
-- -   DescribeMaintenanceStartTimeOutput$HourOfDay
--
-- -   DescribeMaintenanceStartTimeOutput$MinuteOfHour
--
-- -   DescribeMaintenanceStartTimeOutput$Timezone
--
-- /See:/ 'newDescribeMaintenanceStartTimeResponse' smart constructor.
data DescribeMaintenanceStartTimeResponse = DescribeMaintenanceStartTimeResponse'
  { -- | An ordinal number between 0 and 6 that represents the day of the week,
    -- where 0 represents Sunday and 6 represents Saturday. The day of week is
    -- in the time zone of the gateway.
    dayOfWeek :: Core.Maybe Core.Natural,
    -- | The day of the month component of the maintenance start time represented
    -- as an ordinal number from 1 to 28, where 1 represents the first day of
    -- the month and 28 represents the last day of the month.
    dayOfMonth :: Core.Maybe Core.Natural,
    -- | The minute component of the maintenance start time represented as /mm/,
    -- where /mm/ is the minute (0 to 59). The minute of the hour is in the
    -- time zone of the gateway.
    minuteOfHour :: Core.Maybe Core.Natural,
    -- | A value that indicates the time zone that is set for the gateway. The
    -- start time and day of week specified should be in the time zone of the
    -- gateway.
    timezone :: Core.Maybe Core.Text,
    gatewayARN :: Core.Maybe Core.Text,
    -- | The hour component of the maintenance start time represented as /hh/,
    -- where /hh/ is the hour (0 to 23). The hour of the day is in the time
    -- zone of the gateway.
    hourOfDay :: Core.Maybe Core.Natural,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceStartTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfWeek', 'describeMaintenanceStartTimeResponse_dayOfWeek' - An ordinal number between 0 and 6 that represents the day of the week,
-- where 0 represents Sunday and 6 represents Saturday. The day of week is
-- in the time zone of the gateway.
--
-- 'dayOfMonth', 'describeMaintenanceStartTimeResponse_dayOfMonth' - The day of the month component of the maintenance start time represented
-- as an ordinal number from 1 to 28, where 1 represents the first day of
-- the month and 28 represents the last day of the month.
--
-- 'minuteOfHour', 'describeMaintenanceStartTimeResponse_minuteOfHour' - The minute component of the maintenance start time represented as /mm/,
-- where /mm/ is the minute (0 to 59). The minute of the hour is in the
-- time zone of the gateway.
--
-- 'timezone', 'describeMaintenanceStartTimeResponse_timezone' - A value that indicates the time zone that is set for the gateway. The
-- start time and day of week specified should be in the time zone of the
-- gateway.
--
-- 'gatewayARN', 'describeMaintenanceStartTimeResponse_gatewayARN' - Undocumented member.
--
-- 'hourOfDay', 'describeMaintenanceStartTimeResponse_hourOfDay' - The hour component of the maintenance start time represented as /hh/,
-- where /hh/ is the hour (0 to 23). The hour of the day is in the time
-- zone of the gateway.
--
-- 'httpStatus', 'describeMaintenanceStartTimeResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceStartTimeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMaintenanceStartTimeResponse
newDescribeMaintenanceStartTimeResponse pHttpStatus_ =
  DescribeMaintenanceStartTimeResponse'
    { dayOfWeek =
        Core.Nothing,
      dayOfMonth = Core.Nothing,
      minuteOfHour = Core.Nothing,
      timezone = Core.Nothing,
      gatewayARN = Core.Nothing,
      hourOfDay = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ordinal number between 0 and 6 that represents the day of the week,
-- where 0 represents Sunday and 6 represents Saturday. The day of week is
-- in the time zone of the gateway.
describeMaintenanceStartTimeResponse_dayOfWeek :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Core.Maybe Core.Natural)
describeMaintenanceStartTimeResponse_dayOfWeek = Lens.lens (\DescribeMaintenanceStartTimeResponse' {dayOfWeek} -> dayOfWeek) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {dayOfWeek = a} :: DescribeMaintenanceStartTimeResponse)

-- | The day of the month component of the maintenance start time represented
-- as an ordinal number from 1 to 28, where 1 represents the first day of
-- the month and 28 represents the last day of the month.
describeMaintenanceStartTimeResponse_dayOfMonth :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Core.Maybe Core.Natural)
describeMaintenanceStartTimeResponse_dayOfMonth = Lens.lens (\DescribeMaintenanceStartTimeResponse' {dayOfMonth} -> dayOfMonth) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {dayOfMonth = a} :: DescribeMaintenanceStartTimeResponse)

-- | The minute component of the maintenance start time represented as /mm/,
-- where /mm/ is the minute (0 to 59). The minute of the hour is in the
-- time zone of the gateway.
describeMaintenanceStartTimeResponse_minuteOfHour :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Core.Maybe Core.Natural)
describeMaintenanceStartTimeResponse_minuteOfHour = Lens.lens (\DescribeMaintenanceStartTimeResponse' {minuteOfHour} -> minuteOfHour) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {minuteOfHour = a} :: DescribeMaintenanceStartTimeResponse)

-- | A value that indicates the time zone that is set for the gateway. The
-- start time and day of week specified should be in the time zone of the
-- gateway.
describeMaintenanceStartTimeResponse_timezone :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Core.Maybe Core.Text)
describeMaintenanceStartTimeResponse_timezone = Lens.lens (\DescribeMaintenanceStartTimeResponse' {timezone} -> timezone) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {timezone = a} :: DescribeMaintenanceStartTimeResponse)

-- | Undocumented member.
describeMaintenanceStartTimeResponse_gatewayARN :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Core.Maybe Core.Text)
describeMaintenanceStartTimeResponse_gatewayARN = Lens.lens (\DescribeMaintenanceStartTimeResponse' {gatewayARN} -> gatewayARN) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {gatewayARN = a} :: DescribeMaintenanceStartTimeResponse)

-- | The hour component of the maintenance start time represented as /hh/,
-- where /hh/ is the hour (0 to 23). The hour of the day is in the time
-- zone of the gateway.
describeMaintenanceStartTimeResponse_hourOfDay :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Core.Maybe Core.Natural)
describeMaintenanceStartTimeResponse_hourOfDay = Lens.lens (\DescribeMaintenanceStartTimeResponse' {hourOfDay} -> hourOfDay) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {hourOfDay = a} :: DescribeMaintenanceStartTimeResponse)

-- | The response's http status code.
describeMaintenanceStartTimeResponse_httpStatus :: Lens.Lens' DescribeMaintenanceStartTimeResponse Core.Int
describeMaintenanceStartTimeResponse_httpStatus = Lens.lens (\DescribeMaintenanceStartTimeResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceStartTimeResponse)

instance
  Core.NFData
    DescribeMaintenanceStartTimeResponse
