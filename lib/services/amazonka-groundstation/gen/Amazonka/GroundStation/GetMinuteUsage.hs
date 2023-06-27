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
-- Module      : Amazonka.GroundStation.GetMinuteUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of reserved minutes used by account.
module Amazonka.GroundStation.GetMinuteUsage
  ( -- * Creating a Request
    GetMinuteUsage (..),
    newGetMinuteUsage,

    -- * Request Lenses
    getMinuteUsage_month,
    getMinuteUsage_year,

    -- * Destructuring the Response
    GetMinuteUsageResponse (..),
    newGetMinuteUsageResponse,

    -- * Response Lenses
    getMinuteUsageResponse_estimatedMinutesRemaining,
    getMinuteUsageResponse_isReservedMinutesCustomer,
    getMinuteUsageResponse_totalReservedMinuteAllocation,
    getMinuteUsageResponse_totalScheduledMinutes,
    getMinuteUsageResponse_upcomingMinutesScheduled,
    getMinuteUsageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newGetMinuteUsage' smart constructor.
data GetMinuteUsage = GetMinuteUsage'
  { -- | The month being requested, with a value of 1-12.
    month :: Prelude.Natural,
    -- | The year being requested, in the format of YYYY.
    year :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMinuteUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'month', 'getMinuteUsage_month' - The month being requested, with a value of 1-12.
--
-- 'year', 'getMinuteUsage_year' - The year being requested, in the format of YYYY.
newGetMinuteUsage ::
  -- | 'month'
  Prelude.Natural ->
  -- | 'year'
  Prelude.Natural ->
  GetMinuteUsage
newGetMinuteUsage pMonth_ pYear_ =
  GetMinuteUsage' {month = pMonth_, year = pYear_}

-- | The month being requested, with a value of 1-12.
getMinuteUsage_month :: Lens.Lens' GetMinuteUsage Prelude.Natural
getMinuteUsage_month = Lens.lens (\GetMinuteUsage' {month} -> month) (\s@GetMinuteUsage' {} a -> s {month = a} :: GetMinuteUsage)

-- | The year being requested, in the format of YYYY.
getMinuteUsage_year :: Lens.Lens' GetMinuteUsage Prelude.Natural
getMinuteUsage_year = Lens.lens (\GetMinuteUsage' {year} -> year) (\s@GetMinuteUsage' {} a -> s {year = a} :: GetMinuteUsage)

instance Core.AWSRequest GetMinuteUsage where
  type
    AWSResponse GetMinuteUsage =
      GetMinuteUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMinuteUsageResponse'
            Prelude.<$> (x Data..?> "estimatedMinutesRemaining")
            Prelude.<*> (x Data..?> "isReservedMinutesCustomer")
            Prelude.<*> (x Data..?> "totalReservedMinuteAllocation")
            Prelude.<*> (x Data..?> "totalScheduledMinutes")
            Prelude.<*> (x Data..?> "upcomingMinutesScheduled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMinuteUsage where
  hashWithSalt _salt GetMinuteUsage' {..} =
    _salt
      `Prelude.hashWithSalt` month
      `Prelude.hashWithSalt` year

instance Prelude.NFData GetMinuteUsage where
  rnf GetMinuteUsage' {..} =
    Prelude.rnf month `Prelude.seq` Prelude.rnf year

instance Data.ToHeaders GetMinuteUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMinuteUsage where
  toJSON GetMinuteUsage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("month" Data..= month),
            Prelude.Just ("year" Data..= year)
          ]
      )

instance Data.ToPath GetMinuteUsage where
  toPath = Prelude.const "/minute-usage"

instance Data.ToQuery GetMinuteUsage where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetMinuteUsageResponse' smart constructor.
data GetMinuteUsageResponse = GetMinuteUsageResponse'
  { -- | Estimated number of minutes remaining for an account, specific to the
    -- month being requested.
    estimatedMinutesRemaining :: Prelude.Maybe Prelude.Int,
    -- | Returns whether or not an account has signed up for the reserved minutes
    -- pricing plan, specific to the month being requested.
    isReservedMinutesCustomer :: Prelude.Maybe Prelude.Bool,
    -- | Total number of reserved minutes allocated, specific to the month being
    -- requested.
    totalReservedMinuteAllocation :: Prelude.Maybe Prelude.Int,
    -- | Total scheduled minutes for an account, specific to the month being
    -- requested.
    totalScheduledMinutes :: Prelude.Maybe Prelude.Int,
    -- | Upcoming minutes scheduled for an account, specific to the month being
    -- requested.
    upcomingMinutesScheduled :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMinuteUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMinutesRemaining', 'getMinuteUsageResponse_estimatedMinutesRemaining' - Estimated number of minutes remaining for an account, specific to the
-- month being requested.
--
-- 'isReservedMinutesCustomer', 'getMinuteUsageResponse_isReservedMinutesCustomer' - Returns whether or not an account has signed up for the reserved minutes
-- pricing plan, specific to the month being requested.
--
-- 'totalReservedMinuteAllocation', 'getMinuteUsageResponse_totalReservedMinuteAllocation' - Total number of reserved minutes allocated, specific to the month being
-- requested.
--
-- 'totalScheduledMinutes', 'getMinuteUsageResponse_totalScheduledMinutes' - Total scheduled minutes for an account, specific to the month being
-- requested.
--
-- 'upcomingMinutesScheduled', 'getMinuteUsageResponse_upcomingMinutesScheduled' - Upcoming minutes scheduled for an account, specific to the month being
-- requested.
--
-- 'httpStatus', 'getMinuteUsageResponse_httpStatus' - The response's http status code.
newGetMinuteUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMinuteUsageResponse
newGetMinuteUsageResponse pHttpStatus_ =
  GetMinuteUsageResponse'
    { estimatedMinutesRemaining =
        Prelude.Nothing,
      isReservedMinutesCustomer = Prelude.Nothing,
      totalReservedMinuteAllocation = Prelude.Nothing,
      totalScheduledMinutes = Prelude.Nothing,
      upcomingMinutesScheduled = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Estimated number of minutes remaining for an account, specific to the
-- month being requested.
getMinuteUsageResponse_estimatedMinutesRemaining :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Int)
getMinuteUsageResponse_estimatedMinutesRemaining = Lens.lens (\GetMinuteUsageResponse' {estimatedMinutesRemaining} -> estimatedMinutesRemaining) (\s@GetMinuteUsageResponse' {} a -> s {estimatedMinutesRemaining = a} :: GetMinuteUsageResponse)

-- | Returns whether or not an account has signed up for the reserved minutes
-- pricing plan, specific to the month being requested.
getMinuteUsageResponse_isReservedMinutesCustomer :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Bool)
getMinuteUsageResponse_isReservedMinutesCustomer = Lens.lens (\GetMinuteUsageResponse' {isReservedMinutesCustomer} -> isReservedMinutesCustomer) (\s@GetMinuteUsageResponse' {} a -> s {isReservedMinutesCustomer = a} :: GetMinuteUsageResponse)

-- | Total number of reserved minutes allocated, specific to the month being
-- requested.
getMinuteUsageResponse_totalReservedMinuteAllocation :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Int)
getMinuteUsageResponse_totalReservedMinuteAllocation = Lens.lens (\GetMinuteUsageResponse' {totalReservedMinuteAllocation} -> totalReservedMinuteAllocation) (\s@GetMinuteUsageResponse' {} a -> s {totalReservedMinuteAllocation = a} :: GetMinuteUsageResponse)

-- | Total scheduled minutes for an account, specific to the month being
-- requested.
getMinuteUsageResponse_totalScheduledMinutes :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Int)
getMinuteUsageResponse_totalScheduledMinutes = Lens.lens (\GetMinuteUsageResponse' {totalScheduledMinutes} -> totalScheduledMinutes) (\s@GetMinuteUsageResponse' {} a -> s {totalScheduledMinutes = a} :: GetMinuteUsageResponse)

-- | Upcoming minutes scheduled for an account, specific to the month being
-- requested.
getMinuteUsageResponse_upcomingMinutesScheduled :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Int)
getMinuteUsageResponse_upcomingMinutesScheduled = Lens.lens (\GetMinuteUsageResponse' {upcomingMinutesScheduled} -> upcomingMinutesScheduled) (\s@GetMinuteUsageResponse' {} a -> s {upcomingMinutesScheduled = a} :: GetMinuteUsageResponse)

-- | The response's http status code.
getMinuteUsageResponse_httpStatus :: Lens.Lens' GetMinuteUsageResponse Prelude.Int
getMinuteUsageResponse_httpStatus = Lens.lens (\GetMinuteUsageResponse' {httpStatus} -> httpStatus) (\s@GetMinuteUsageResponse' {} a -> s {httpStatus = a} :: GetMinuteUsageResponse)

instance Prelude.NFData GetMinuteUsageResponse where
  rnf GetMinuteUsageResponse' {..} =
    Prelude.rnf estimatedMinutesRemaining
      `Prelude.seq` Prelude.rnf isReservedMinutesCustomer
      `Prelude.seq` Prelude.rnf totalReservedMinuteAllocation
      `Prelude.seq` Prelude.rnf totalScheduledMinutes
      `Prelude.seq` Prelude.rnf upcomingMinutesScheduled
      `Prelude.seq` Prelude.rnf httpStatus
