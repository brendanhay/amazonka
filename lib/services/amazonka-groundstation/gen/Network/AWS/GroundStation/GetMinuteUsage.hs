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
-- Module      : Network.AWS.GroundStation.GetMinuteUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of minutes used by account.
module Network.AWS.GroundStation.GetMinuteUsage
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
    getMinuteUsageResponse_isReservedMinutesCustomer,
    getMinuteUsageResponse_upcomingMinutesScheduled,
    getMinuteUsageResponse_totalScheduledMinutes,
    getMinuteUsageResponse_totalReservedMinuteAllocation,
    getMinuteUsageResponse_estimatedMinutesRemaining,
    getMinuteUsageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GroundStation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newGetMinuteUsage' smart constructor.
data GetMinuteUsage = GetMinuteUsage'
  { -- | The month being requested, with a value of 1-12.
    month :: Prelude.Int,
    -- | The year being requested, in the format of YYYY.
    year :: Prelude.Int
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
  Prelude.Int ->
  -- | 'year'
  Prelude.Int ->
  GetMinuteUsage
newGetMinuteUsage pMonth_ pYear_ =
  GetMinuteUsage' {month = pMonth_, year = pYear_}

-- | The month being requested, with a value of 1-12.
getMinuteUsage_month :: Lens.Lens' GetMinuteUsage Prelude.Int
getMinuteUsage_month = Lens.lens (\GetMinuteUsage' {month} -> month) (\s@GetMinuteUsage' {} a -> s {month = a} :: GetMinuteUsage)

-- | The year being requested, in the format of YYYY.
getMinuteUsage_year :: Lens.Lens' GetMinuteUsage Prelude.Int
getMinuteUsage_year = Lens.lens (\GetMinuteUsage' {year} -> year) (\s@GetMinuteUsage' {} a -> s {year = a} :: GetMinuteUsage)

instance Core.AWSRequest GetMinuteUsage where
  type
    AWSResponse GetMinuteUsage =
      GetMinuteUsageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMinuteUsageResponse'
            Prelude.<$> (x Core..?> "isReservedMinutesCustomer")
            Prelude.<*> (x Core..?> "upcomingMinutesScheduled")
            Prelude.<*> (x Core..?> "totalScheduledMinutes")
            Prelude.<*> (x Core..?> "totalReservedMinuteAllocation")
            Prelude.<*> (x Core..?> "estimatedMinutesRemaining")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMinuteUsage

instance Prelude.NFData GetMinuteUsage

instance Core.ToHeaders GetMinuteUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMinuteUsage where
  toJSON GetMinuteUsage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("month" Core..= month),
            Prelude.Just ("year" Core..= year)
          ]
      )

instance Core.ToPath GetMinuteUsage where
  toPath = Prelude.const "/minute-usage"

instance Core.ToQuery GetMinuteUsage where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetMinuteUsageResponse' smart constructor.
data GetMinuteUsageResponse = GetMinuteUsageResponse'
  { -- | Returns whether or not an account has signed up for the reserved minutes
    -- pricing plan, specific to the month being requested.
    isReservedMinutesCustomer :: Prelude.Maybe Prelude.Bool,
    -- | Upcoming minutes scheduled for an account, specific to the month being
    -- requested.
    upcomingMinutesScheduled :: Prelude.Maybe Prelude.Int,
    -- | Total scheduled minutes for an account, specific to the month being
    -- requested.
    totalScheduledMinutes :: Prelude.Maybe Prelude.Int,
    -- | Total number of reserved minutes allocated, specific to the month being
    -- requested.
    totalReservedMinuteAllocation :: Prelude.Maybe Prelude.Int,
    -- | Estimated number of minutes remaining for an account, specific to the
    -- month being requested.
    estimatedMinutesRemaining :: Prelude.Maybe Prelude.Int,
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
-- 'isReservedMinutesCustomer', 'getMinuteUsageResponse_isReservedMinutesCustomer' - Returns whether or not an account has signed up for the reserved minutes
-- pricing plan, specific to the month being requested.
--
-- 'upcomingMinutesScheduled', 'getMinuteUsageResponse_upcomingMinutesScheduled' - Upcoming minutes scheduled for an account, specific to the month being
-- requested.
--
-- 'totalScheduledMinutes', 'getMinuteUsageResponse_totalScheduledMinutes' - Total scheduled minutes for an account, specific to the month being
-- requested.
--
-- 'totalReservedMinuteAllocation', 'getMinuteUsageResponse_totalReservedMinuteAllocation' - Total number of reserved minutes allocated, specific to the month being
-- requested.
--
-- 'estimatedMinutesRemaining', 'getMinuteUsageResponse_estimatedMinutesRemaining' - Estimated number of minutes remaining for an account, specific to the
-- month being requested.
--
-- 'httpStatus', 'getMinuteUsageResponse_httpStatus' - The response's http status code.
newGetMinuteUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMinuteUsageResponse
newGetMinuteUsageResponse pHttpStatus_ =
  GetMinuteUsageResponse'
    { isReservedMinutesCustomer =
        Prelude.Nothing,
      upcomingMinutesScheduled = Prelude.Nothing,
      totalScheduledMinutes = Prelude.Nothing,
      totalReservedMinuteAllocation = Prelude.Nothing,
      estimatedMinutesRemaining = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns whether or not an account has signed up for the reserved minutes
-- pricing plan, specific to the month being requested.
getMinuteUsageResponse_isReservedMinutesCustomer :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Bool)
getMinuteUsageResponse_isReservedMinutesCustomer = Lens.lens (\GetMinuteUsageResponse' {isReservedMinutesCustomer} -> isReservedMinutesCustomer) (\s@GetMinuteUsageResponse' {} a -> s {isReservedMinutesCustomer = a} :: GetMinuteUsageResponse)

-- | Upcoming minutes scheduled for an account, specific to the month being
-- requested.
getMinuteUsageResponse_upcomingMinutesScheduled :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Int)
getMinuteUsageResponse_upcomingMinutesScheduled = Lens.lens (\GetMinuteUsageResponse' {upcomingMinutesScheduled} -> upcomingMinutesScheduled) (\s@GetMinuteUsageResponse' {} a -> s {upcomingMinutesScheduled = a} :: GetMinuteUsageResponse)

-- | Total scheduled minutes for an account, specific to the month being
-- requested.
getMinuteUsageResponse_totalScheduledMinutes :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Int)
getMinuteUsageResponse_totalScheduledMinutes = Lens.lens (\GetMinuteUsageResponse' {totalScheduledMinutes} -> totalScheduledMinutes) (\s@GetMinuteUsageResponse' {} a -> s {totalScheduledMinutes = a} :: GetMinuteUsageResponse)

-- | Total number of reserved minutes allocated, specific to the month being
-- requested.
getMinuteUsageResponse_totalReservedMinuteAllocation :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Int)
getMinuteUsageResponse_totalReservedMinuteAllocation = Lens.lens (\GetMinuteUsageResponse' {totalReservedMinuteAllocation} -> totalReservedMinuteAllocation) (\s@GetMinuteUsageResponse' {} a -> s {totalReservedMinuteAllocation = a} :: GetMinuteUsageResponse)

-- | Estimated number of minutes remaining for an account, specific to the
-- month being requested.
getMinuteUsageResponse_estimatedMinutesRemaining :: Lens.Lens' GetMinuteUsageResponse (Prelude.Maybe Prelude.Int)
getMinuteUsageResponse_estimatedMinutesRemaining = Lens.lens (\GetMinuteUsageResponse' {estimatedMinutesRemaining} -> estimatedMinutesRemaining) (\s@GetMinuteUsageResponse' {} a -> s {estimatedMinutesRemaining = a} :: GetMinuteUsageResponse)

-- | The response's http status code.
getMinuteUsageResponse_httpStatus :: Lens.Lens' GetMinuteUsageResponse Prelude.Int
getMinuteUsageResponse_httpStatus = Lens.lens (\GetMinuteUsageResponse' {httpStatus} -> httpStatus) (\s@GetMinuteUsageResponse' {} a -> s {httpStatus = a} :: GetMinuteUsageResponse)

instance Prelude.NFData GetMinuteUsageResponse
