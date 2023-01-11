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
-- Module      : Amazonka.StorageGateway.DescribeMaintenanceStartTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns your gateway\'s weekly maintenance start time including the day
-- and time of the week. Note that values are in terms of the gateway\'s
-- time zone.
module Amazonka.StorageGateway.DescribeMaintenanceStartTime
  ( -- * Creating a Request
    DescribeMaintenanceStartTime (..),
    newDescribeMaintenanceStartTime,

    -- * Request Lenses
    describeMaintenanceStartTime_gatewayARN,

    -- * Destructuring the Response
    DescribeMaintenanceStartTimeResponse (..),
    newDescribeMaintenanceStartTimeResponse,

    -- * Response Lenses
    describeMaintenanceStartTimeResponse_dayOfMonth,
    describeMaintenanceStartTimeResponse_dayOfWeek,
    describeMaintenanceStartTimeResponse_gatewayARN,
    describeMaintenanceStartTimeResponse_hourOfDay,
    describeMaintenanceStartTimeResponse_minuteOfHour,
    describeMaintenanceStartTimeResponse_timezone,
    describeMaintenanceStartTimeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'newDescribeMaintenanceStartTime' smart constructor.
data DescribeMaintenanceStartTime = DescribeMaintenanceStartTime'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeMaintenanceStartTime
newDescribeMaintenanceStartTime pGatewayARN_ =
  DescribeMaintenanceStartTime'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeMaintenanceStartTime_gatewayARN :: Lens.Lens' DescribeMaintenanceStartTime Prelude.Text
describeMaintenanceStartTime_gatewayARN = Lens.lens (\DescribeMaintenanceStartTime' {gatewayARN} -> gatewayARN) (\s@DescribeMaintenanceStartTime' {} a -> s {gatewayARN = a} :: DescribeMaintenanceStartTime)

instance Core.AWSRequest DescribeMaintenanceStartTime where
  type
    AWSResponse DescribeMaintenanceStartTime =
      DescribeMaintenanceStartTimeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceStartTimeResponse'
            Prelude.<$> (x Data..?> "DayOfMonth")
            Prelude.<*> (x Data..?> "DayOfWeek")
            Prelude.<*> (x Data..?> "GatewayARN")
            Prelude.<*> (x Data..?> "HourOfDay")
            Prelude.<*> (x Data..?> "MinuteOfHour")
            Prelude.<*> (x Data..?> "Timezone")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMaintenanceStartTime
  where
  hashWithSalt _salt DescribeMaintenanceStartTime' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData DescribeMaintenanceStartTime where
  rnf DescribeMaintenanceStartTime' {..} =
    Prelude.rnf gatewayARN

instance Data.ToHeaders DescribeMaintenanceStartTime where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeMaintenanceStartTime" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMaintenanceStartTime where
  toJSON DescribeMaintenanceStartTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Data..= gatewayARN)]
      )

instance Data.ToPath DescribeMaintenanceStartTime where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMaintenanceStartTime where
  toQuery = Prelude.const Prelude.mempty

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
  { -- | The day of the month component of the maintenance start time represented
    -- as an ordinal number from 1 to 28, where 1 represents the first day of
    -- the month and 28 represents the last day of the month.
    dayOfMonth :: Prelude.Maybe Prelude.Natural,
    -- | An ordinal number between 0 and 6 that represents the day of the week,
    -- where 0 represents Sunday and 6 represents Saturday. The day of week is
    -- in the time zone of the gateway.
    dayOfWeek :: Prelude.Maybe Prelude.Natural,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The hour component of the maintenance start time represented as /hh/,
    -- where /hh/ is the hour (0 to 23). The hour of the day is in the time
    -- zone of the gateway.
    hourOfDay :: Prelude.Maybe Prelude.Natural,
    -- | The minute component of the maintenance start time represented as /mm/,
    -- where /mm/ is the minute (0 to 59). The minute of the hour is in the
    -- time zone of the gateway.
    minuteOfHour :: Prelude.Maybe Prelude.Natural,
    -- | A value that indicates the time zone that is set for the gateway. The
    -- start time and day of week specified should be in the time zone of the
    -- gateway.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMaintenanceStartTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfMonth', 'describeMaintenanceStartTimeResponse_dayOfMonth' - The day of the month component of the maintenance start time represented
-- as an ordinal number from 1 to 28, where 1 represents the first day of
-- the month and 28 represents the last day of the month.
--
-- 'dayOfWeek', 'describeMaintenanceStartTimeResponse_dayOfWeek' - An ordinal number between 0 and 6 that represents the day of the week,
-- where 0 represents Sunday and 6 represents Saturday. The day of week is
-- in the time zone of the gateway.
--
-- 'gatewayARN', 'describeMaintenanceStartTimeResponse_gatewayARN' - Undocumented member.
--
-- 'hourOfDay', 'describeMaintenanceStartTimeResponse_hourOfDay' - The hour component of the maintenance start time represented as /hh/,
-- where /hh/ is the hour (0 to 23). The hour of the day is in the time
-- zone of the gateway.
--
-- 'minuteOfHour', 'describeMaintenanceStartTimeResponse_minuteOfHour' - The minute component of the maintenance start time represented as /mm/,
-- where /mm/ is the minute (0 to 59). The minute of the hour is in the
-- time zone of the gateway.
--
-- 'timezone', 'describeMaintenanceStartTimeResponse_timezone' - A value that indicates the time zone that is set for the gateway. The
-- start time and day of week specified should be in the time zone of the
-- gateway.
--
-- 'httpStatus', 'describeMaintenanceStartTimeResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceStartTimeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMaintenanceStartTimeResponse
newDescribeMaintenanceStartTimeResponse pHttpStatus_ =
  DescribeMaintenanceStartTimeResponse'
    { dayOfMonth =
        Prelude.Nothing,
      dayOfWeek = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      hourOfDay = Prelude.Nothing,
      minuteOfHour = Prelude.Nothing,
      timezone = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The day of the month component of the maintenance start time represented
-- as an ordinal number from 1 to 28, where 1 represents the first day of
-- the month and 28 represents the last day of the month.
describeMaintenanceStartTimeResponse_dayOfMonth :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Prelude.Maybe Prelude.Natural)
describeMaintenanceStartTimeResponse_dayOfMonth = Lens.lens (\DescribeMaintenanceStartTimeResponse' {dayOfMonth} -> dayOfMonth) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {dayOfMonth = a} :: DescribeMaintenanceStartTimeResponse)

-- | An ordinal number between 0 and 6 that represents the day of the week,
-- where 0 represents Sunday and 6 represents Saturday. The day of week is
-- in the time zone of the gateway.
describeMaintenanceStartTimeResponse_dayOfWeek :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Prelude.Maybe Prelude.Natural)
describeMaintenanceStartTimeResponse_dayOfWeek = Lens.lens (\DescribeMaintenanceStartTimeResponse' {dayOfWeek} -> dayOfWeek) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {dayOfWeek = a} :: DescribeMaintenanceStartTimeResponse)

-- | Undocumented member.
describeMaintenanceStartTimeResponse_gatewayARN :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceStartTimeResponse_gatewayARN = Lens.lens (\DescribeMaintenanceStartTimeResponse' {gatewayARN} -> gatewayARN) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {gatewayARN = a} :: DescribeMaintenanceStartTimeResponse)

-- | The hour component of the maintenance start time represented as /hh/,
-- where /hh/ is the hour (0 to 23). The hour of the day is in the time
-- zone of the gateway.
describeMaintenanceStartTimeResponse_hourOfDay :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Prelude.Maybe Prelude.Natural)
describeMaintenanceStartTimeResponse_hourOfDay = Lens.lens (\DescribeMaintenanceStartTimeResponse' {hourOfDay} -> hourOfDay) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {hourOfDay = a} :: DescribeMaintenanceStartTimeResponse)

-- | The minute component of the maintenance start time represented as /mm/,
-- where /mm/ is the minute (0 to 59). The minute of the hour is in the
-- time zone of the gateway.
describeMaintenanceStartTimeResponse_minuteOfHour :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Prelude.Maybe Prelude.Natural)
describeMaintenanceStartTimeResponse_minuteOfHour = Lens.lens (\DescribeMaintenanceStartTimeResponse' {minuteOfHour} -> minuteOfHour) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {minuteOfHour = a} :: DescribeMaintenanceStartTimeResponse)

-- | A value that indicates the time zone that is set for the gateway. The
-- start time and day of week specified should be in the time zone of the
-- gateway.
describeMaintenanceStartTimeResponse_timezone :: Lens.Lens' DescribeMaintenanceStartTimeResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceStartTimeResponse_timezone = Lens.lens (\DescribeMaintenanceStartTimeResponse' {timezone} -> timezone) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {timezone = a} :: DescribeMaintenanceStartTimeResponse)

-- | The response's http status code.
describeMaintenanceStartTimeResponse_httpStatus :: Lens.Lens' DescribeMaintenanceStartTimeResponse Prelude.Int
describeMaintenanceStartTimeResponse_httpStatus = Lens.lens (\DescribeMaintenanceStartTimeResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceStartTimeResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceStartTimeResponse)

instance
  Prelude.NFData
    DescribeMaintenanceStartTimeResponse
  where
  rnf DescribeMaintenanceStartTimeResponse' {..} =
    Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf hourOfDay
      `Prelude.seq` Prelude.rnf minuteOfHour
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf httpStatus
