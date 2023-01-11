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
-- Module      : Amazonka.BackupGateway.PutMaintenanceStartTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the maintenance start time for a gateway.
module Amazonka.BackupGateway.PutMaintenanceStartTime
  ( -- * Creating a Request
    PutMaintenanceStartTime (..),
    newPutMaintenanceStartTime,

    -- * Request Lenses
    putMaintenanceStartTime_dayOfMonth,
    putMaintenanceStartTime_dayOfWeek,
    putMaintenanceStartTime_gatewayArn,
    putMaintenanceStartTime_hourOfDay,
    putMaintenanceStartTime_minuteOfHour,

    -- * Destructuring the Response
    PutMaintenanceStartTimeResponse (..),
    newPutMaintenanceStartTimeResponse,

    -- * Response Lenses
    putMaintenanceStartTimeResponse_gatewayArn,
    putMaintenanceStartTimeResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMaintenanceStartTime' smart constructor.
data PutMaintenanceStartTime = PutMaintenanceStartTime'
  { -- | The day of the month start maintenance on a gateway.
    --
    -- Valid values range from @Sunday@ to @Saturday@.
    dayOfMonth :: Prelude.Maybe Prelude.Natural,
    -- | The day of the week to start maintenance on a gateway.
    dayOfWeek :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) for the gateway, used to specify its
    -- maintenance start time.
    gatewayArn :: Prelude.Text,
    -- | The hour of the day to start maintenance on a gateway.
    hourOfDay :: Prelude.Natural,
    -- | The minute of the hour to start maintenance on a gateway.
    minuteOfHour :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMaintenanceStartTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dayOfMonth', 'putMaintenanceStartTime_dayOfMonth' - The day of the month start maintenance on a gateway.
--
-- Valid values range from @Sunday@ to @Saturday@.
--
-- 'dayOfWeek', 'putMaintenanceStartTime_dayOfWeek' - The day of the week to start maintenance on a gateway.
--
-- 'gatewayArn', 'putMaintenanceStartTime_gatewayArn' - The Amazon Resource Name (ARN) for the gateway, used to specify its
-- maintenance start time.
--
-- 'hourOfDay', 'putMaintenanceStartTime_hourOfDay' - The hour of the day to start maintenance on a gateway.
--
-- 'minuteOfHour', 'putMaintenanceStartTime_minuteOfHour' - The minute of the hour to start maintenance on a gateway.
newPutMaintenanceStartTime ::
  -- | 'gatewayArn'
  Prelude.Text ->
  -- | 'hourOfDay'
  Prelude.Natural ->
  -- | 'minuteOfHour'
  Prelude.Natural ->
  PutMaintenanceStartTime
newPutMaintenanceStartTime
  pGatewayArn_
  pHourOfDay_
  pMinuteOfHour_ =
    PutMaintenanceStartTime'
      { dayOfMonth =
          Prelude.Nothing,
        dayOfWeek = Prelude.Nothing,
        gatewayArn = pGatewayArn_,
        hourOfDay = pHourOfDay_,
        minuteOfHour = pMinuteOfHour_
      }

-- | The day of the month start maintenance on a gateway.
--
-- Valid values range from @Sunday@ to @Saturday@.
putMaintenanceStartTime_dayOfMonth :: Lens.Lens' PutMaintenanceStartTime (Prelude.Maybe Prelude.Natural)
putMaintenanceStartTime_dayOfMonth = Lens.lens (\PutMaintenanceStartTime' {dayOfMonth} -> dayOfMonth) (\s@PutMaintenanceStartTime' {} a -> s {dayOfMonth = a} :: PutMaintenanceStartTime)

-- | The day of the week to start maintenance on a gateway.
putMaintenanceStartTime_dayOfWeek :: Lens.Lens' PutMaintenanceStartTime (Prelude.Maybe Prelude.Natural)
putMaintenanceStartTime_dayOfWeek = Lens.lens (\PutMaintenanceStartTime' {dayOfWeek} -> dayOfWeek) (\s@PutMaintenanceStartTime' {} a -> s {dayOfWeek = a} :: PutMaintenanceStartTime)

-- | The Amazon Resource Name (ARN) for the gateway, used to specify its
-- maintenance start time.
putMaintenanceStartTime_gatewayArn :: Lens.Lens' PutMaintenanceStartTime Prelude.Text
putMaintenanceStartTime_gatewayArn = Lens.lens (\PutMaintenanceStartTime' {gatewayArn} -> gatewayArn) (\s@PutMaintenanceStartTime' {} a -> s {gatewayArn = a} :: PutMaintenanceStartTime)

-- | The hour of the day to start maintenance on a gateway.
putMaintenanceStartTime_hourOfDay :: Lens.Lens' PutMaintenanceStartTime Prelude.Natural
putMaintenanceStartTime_hourOfDay = Lens.lens (\PutMaintenanceStartTime' {hourOfDay} -> hourOfDay) (\s@PutMaintenanceStartTime' {} a -> s {hourOfDay = a} :: PutMaintenanceStartTime)

-- | The minute of the hour to start maintenance on a gateway.
putMaintenanceStartTime_minuteOfHour :: Lens.Lens' PutMaintenanceStartTime Prelude.Natural
putMaintenanceStartTime_minuteOfHour = Lens.lens (\PutMaintenanceStartTime' {minuteOfHour} -> minuteOfHour) (\s@PutMaintenanceStartTime' {} a -> s {minuteOfHour = a} :: PutMaintenanceStartTime)

instance Core.AWSRequest PutMaintenanceStartTime where
  type
    AWSResponse PutMaintenanceStartTime =
      PutMaintenanceStartTimeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutMaintenanceStartTimeResponse'
            Prelude.<$> (x Data..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutMaintenanceStartTime where
  hashWithSalt _salt PutMaintenanceStartTime' {..} =
    _salt `Prelude.hashWithSalt` dayOfMonth
      `Prelude.hashWithSalt` dayOfWeek
      `Prelude.hashWithSalt` gatewayArn
      `Prelude.hashWithSalt` hourOfDay
      `Prelude.hashWithSalt` minuteOfHour

instance Prelude.NFData PutMaintenanceStartTime where
  rnf PutMaintenanceStartTime' {..} =
    Prelude.rnf dayOfMonth
      `Prelude.seq` Prelude.rnf dayOfWeek
      `Prelude.seq` Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf hourOfDay
      `Prelude.seq` Prelude.rnf minuteOfHour

instance Data.ToHeaders PutMaintenanceStartTime where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.PutMaintenanceStartTime" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutMaintenanceStartTime where
  toJSON PutMaintenanceStartTime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DayOfMonth" Data..=) Prelude.<$> dayOfMonth,
            ("DayOfWeek" Data..=) Prelude.<$> dayOfWeek,
            Prelude.Just ("GatewayArn" Data..= gatewayArn),
            Prelude.Just ("HourOfDay" Data..= hourOfDay),
            Prelude.Just ("MinuteOfHour" Data..= minuteOfHour)
          ]
      )

instance Data.ToPath PutMaintenanceStartTime where
  toPath = Prelude.const "/"

instance Data.ToQuery PutMaintenanceStartTime where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutMaintenanceStartTimeResponse' smart constructor.
data PutMaintenanceStartTimeResponse = PutMaintenanceStartTimeResponse'
  { -- | The Amazon Resource Name (ARN) of a gateway for which you set the
    -- maintenance start time.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMaintenanceStartTimeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'putMaintenanceStartTimeResponse_gatewayArn' - The Amazon Resource Name (ARN) of a gateway for which you set the
-- maintenance start time.
--
-- 'httpStatus', 'putMaintenanceStartTimeResponse_httpStatus' - The response's http status code.
newPutMaintenanceStartTimeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutMaintenanceStartTimeResponse
newPutMaintenanceStartTimeResponse pHttpStatus_ =
  PutMaintenanceStartTimeResponse'
    { gatewayArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of a gateway for which you set the
-- maintenance start time.
putMaintenanceStartTimeResponse_gatewayArn :: Lens.Lens' PutMaintenanceStartTimeResponse (Prelude.Maybe Prelude.Text)
putMaintenanceStartTimeResponse_gatewayArn = Lens.lens (\PutMaintenanceStartTimeResponse' {gatewayArn} -> gatewayArn) (\s@PutMaintenanceStartTimeResponse' {} a -> s {gatewayArn = a} :: PutMaintenanceStartTimeResponse)

-- | The response's http status code.
putMaintenanceStartTimeResponse_httpStatus :: Lens.Lens' PutMaintenanceStartTimeResponse Prelude.Int
putMaintenanceStartTimeResponse_httpStatus = Lens.lens (\PutMaintenanceStartTimeResponse' {httpStatus} -> httpStatus) (\s@PutMaintenanceStartTimeResponse' {} a -> s {httpStatus = a} :: PutMaintenanceStartTimeResponse)

instance
  Prelude.NFData
    PutMaintenanceStartTimeResponse
  where
  rnf PutMaintenanceStartTimeResponse' {..} =
    Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf httpStatus
