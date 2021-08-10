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
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Savings Plans utilization for your account across date
-- ranges with daily or monthly granularity. Management account in an
-- organization have access to member accounts. You can use
-- @GetDimensionValues@ in @SAVINGS_PLANS@ to determine the possible
-- dimension values.
--
-- You cannot group by any dimension values for
-- @GetSavingsPlansUtilization@.
module Network.AWS.CostExplorer.GetSavingsPlansUtilization
  ( -- * Creating a Request
    GetSavingsPlansUtilization (..),
    newGetSavingsPlansUtilization,

    -- * Request Lenses
    getSavingsPlansUtilization_granularity,
    getSavingsPlansUtilization_sortBy,
    getSavingsPlansUtilization_filter,
    getSavingsPlansUtilization_timePeriod,

    -- * Destructuring the Response
    GetSavingsPlansUtilizationResponse (..),
    newGetSavingsPlansUtilizationResponse,

    -- * Response Lenses
    getSavingsPlansUtilizationResponse_savingsPlansUtilizationsByTime,
    getSavingsPlansUtilizationResponse_httpStatus,
    getSavingsPlansUtilizationResponse_total,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSavingsPlansUtilization' smart constructor.
data GetSavingsPlansUtilization = GetSavingsPlansUtilization'
  { -- | The granularity of the Amazon Web Services utillization data for your
    -- Savings Plans.
    --
    -- The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and
    -- @MONTHLY@ granularities.
    granularity :: Prelude.Maybe Granularity,
    -- | The value by which you want to sort the data.
    --
    -- The following values are supported for @Key@:
    --
    -- -   @UtilizationPercentage@
    --
    -- -   @TotalCommitment@
    --
    -- -   @UsedCommitment@
    --
    -- -   @UnusedCommitment@
    --
    -- -   @NetSavings@
    --
    -- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
    sortBy :: Prelude.Maybe SortDefinition,
    -- | Filters Savings Plans utilization coverage data for active Savings Plans
    -- dimensions. You can filter data with the following dimensions:
    --
    -- -   @LINKED_ACCOUNT@
    --
    -- -   @SAVINGS_PLAN_ARN@
    --
    -- -   @SAVINGS_PLANS_TYPE@
    --
    -- -   @REGION@
    --
    -- -   @PAYMENT_OPTION@
    --
    -- -   @INSTANCE_TYPE_FAMILY@
    --
    -- @GetSavingsPlansUtilization@ uses the same
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
    -- object as the other operations, but only @AND@ is supported among each
    -- dimension.
    filter' :: Prelude.Maybe Expression,
    -- | The time period that you want the usage and costs for. The @Start@ date
    -- must be within 13 months. The @End@ date must be after the @Start@ date,
    -- and before the current date. Future dates can\'t be used as an @End@
    -- date.
    timePeriod :: DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSavingsPlansUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'granularity', 'getSavingsPlansUtilization_granularity' - The granularity of the Amazon Web Services utillization data for your
-- Savings Plans.
--
-- The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and
-- @MONTHLY@ granularities.
--
-- 'sortBy', 'getSavingsPlansUtilization_sortBy' - The value by which you want to sort the data.
--
-- The following values are supported for @Key@:
--
-- -   @UtilizationPercentage@
--
-- -   @TotalCommitment@
--
-- -   @UsedCommitment@
--
-- -   @UnusedCommitment@
--
-- -   @NetSavings@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
--
-- 'filter'', 'getSavingsPlansUtilization_filter' - Filters Savings Plans utilization coverage data for active Savings Plans
-- dimensions. You can filter data with the following dimensions:
--
-- -   @LINKED_ACCOUNT@
--
-- -   @SAVINGS_PLAN_ARN@
--
-- -   @SAVINGS_PLANS_TYPE@
--
-- -   @REGION@
--
-- -   @PAYMENT_OPTION@
--
-- -   @INSTANCE_TYPE_FAMILY@
--
-- @GetSavingsPlansUtilization@ uses the same
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object as the other operations, but only @AND@ is supported among each
-- dimension.
--
-- 'timePeriod', 'getSavingsPlansUtilization_timePeriod' - The time period that you want the usage and costs for. The @Start@ date
-- must be within 13 months. The @End@ date must be after the @Start@ date,
-- and before the current date. Future dates can\'t be used as an @End@
-- date.
newGetSavingsPlansUtilization ::
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansUtilization
newGetSavingsPlansUtilization pTimePeriod_ =
  GetSavingsPlansUtilization'
    { granularity =
        Prelude.Nothing,
      sortBy = Prelude.Nothing,
      filter' = Prelude.Nothing,
      timePeriod = pTimePeriod_
    }

-- | The granularity of the Amazon Web Services utillization data for your
-- Savings Plans.
--
-- The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and
-- @MONTHLY@ granularities.
getSavingsPlansUtilization_granularity :: Lens.Lens' GetSavingsPlansUtilization (Prelude.Maybe Granularity)
getSavingsPlansUtilization_granularity = Lens.lens (\GetSavingsPlansUtilization' {granularity} -> granularity) (\s@GetSavingsPlansUtilization' {} a -> s {granularity = a} :: GetSavingsPlansUtilization)

-- | The value by which you want to sort the data.
--
-- The following values are supported for @Key@:
--
-- -   @UtilizationPercentage@
--
-- -   @TotalCommitment@
--
-- -   @UsedCommitment@
--
-- -   @UnusedCommitment@
--
-- -   @NetSavings@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
getSavingsPlansUtilization_sortBy :: Lens.Lens' GetSavingsPlansUtilization (Prelude.Maybe SortDefinition)
getSavingsPlansUtilization_sortBy = Lens.lens (\GetSavingsPlansUtilization' {sortBy} -> sortBy) (\s@GetSavingsPlansUtilization' {} a -> s {sortBy = a} :: GetSavingsPlansUtilization)

-- | Filters Savings Plans utilization coverage data for active Savings Plans
-- dimensions. You can filter data with the following dimensions:
--
-- -   @LINKED_ACCOUNT@
--
-- -   @SAVINGS_PLAN_ARN@
--
-- -   @SAVINGS_PLANS_TYPE@
--
-- -   @REGION@
--
-- -   @PAYMENT_OPTION@
--
-- -   @INSTANCE_TYPE_FAMILY@
--
-- @GetSavingsPlansUtilization@ uses the same
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object as the other operations, but only @AND@ is supported among each
-- dimension.
getSavingsPlansUtilization_filter :: Lens.Lens' GetSavingsPlansUtilization (Prelude.Maybe Expression)
getSavingsPlansUtilization_filter = Lens.lens (\GetSavingsPlansUtilization' {filter'} -> filter') (\s@GetSavingsPlansUtilization' {} a -> s {filter' = a} :: GetSavingsPlansUtilization)

-- | The time period that you want the usage and costs for. The @Start@ date
-- must be within 13 months. The @End@ date must be after the @Start@ date,
-- and before the current date. Future dates can\'t be used as an @End@
-- date.
getSavingsPlansUtilization_timePeriod :: Lens.Lens' GetSavingsPlansUtilization DateInterval
getSavingsPlansUtilization_timePeriod = Lens.lens (\GetSavingsPlansUtilization' {timePeriod} -> timePeriod) (\s@GetSavingsPlansUtilization' {} a -> s {timePeriod = a} :: GetSavingsPlansUtilization)

instance Core.AWSRequest GetSavingsPlansUtilization where
  type
    AWSResponse GetSavingsPlansUtilization =
      GetSavingsPlansUtilizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSavingsPlansUtilizationResponse'
            Prelude.<$> ( x Core..?> "SavingsPlansUtilizationsByTime"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Total")
      )

instance Prelude.Hashable GetSavingsPlansUtilization

instance Prelude.NFData GetSavingsPlansUtilization

instance Core.ToHeaders GetSavingsPlansUtilization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetSavingsPlansUtilization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetSavingsPlansUtilization where
  toJSON GetSavingsPlansUtilization' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Granularity" Core..=) Prelude.<$> granularity,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("Filter" Core..=) Prelude.<$> filter',
            Prelude.Just ("TimePeriod" Core..= timePeriod)
          ]
      )

instance Core.ToPath GetSavingsPlansUtilization where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSavingsPlansUtilization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSavingsPlansUtilizationResponse' smart constructor.
data GetSavingsPlansUtilizationResponse = GetSavingsPlansUtilizationResponse'
  { -- | The amount of cost\/commitment you used your Savings Plans. This allows
    -- you to specify date ranges.
    savingsPlansUtilizationsByTime :: Prelude.Maybe [SavingsPlansUtilizationByTime],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The total amount of cost\/commitment that you used your Savings Plans,
    -- regardless of date ranges.
    total :: SavingsPlansUtilizationAggregates
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSavingsPlansUtilizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'savingsPlansUtilizationsByTime', 'getSavingsPlansUtilizationResponse_savingsPlansUtilizationsByTime' - The amount of cost\/commitment you used your Savings Plans. This allows
-- you to specify date ranges.
--
-- 'httpStatus', 'getSavingsPlansUtilizationResponse_httpStatus' - The response's http status code.
--
-- 'total', 'getSavingsPlansUtilizationResponse_total' - The total amount of cost\/commitment that you used your Savings Plans,
-- regardless of date ranges.
newGetSavingsPlansUtilizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'total'
  SavingsPlansUtilizationAggregates ->
  GetSavingsPlansUtilizationResponse
newGetSavingsPlansUtilizationResponse
  pHttpStatus_
  pTotal_ =
    GetSavingsPlansUtilizationResponse'
      { savingsPlansUtilizationsByTime =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        total = pTotal_
      }

-- | The amount of cost\/commitment you used your Savings Plans. This allows
-- you to specify date ranges.
getSavingsPlansUtilizationResponse_savingsPlansUtilizationsByTime :: Lens.Lens' GetSavingsPlansUtilizationResponse (Prelude.Maybe [SavingsPlansUtilizationByTime])
getSavingsPlansUtilizationResponse_savingsPlansUtilizationsByTime = Lens.lens (\GetSavingsPlansUtilizationResponse' {savingsPlansUtilizationsByTime} -> savingsPlansUtilizationsByTime) (\s@GetSavingsPlansUtilizationResponse' {} a -> s {savingsPlansUtilizationsByTime = a} :: GetSavingsPlansUtilizationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSavingsPlansUtilizationResponse_httpStatus :: Lens.Lens' GetSavingsPlansUtilizationResponse Prelude.Int
getSavingsPlansUtilizationResponse_httpStatus = Lens.lens (\GetSavingsPlansUtilizationResponse' {httpStatus} -> httpStatus) (\s@GetSavingsPlansUtilizationResponse' {} a -> s {httpStatus = a} :: GetSavingsPlansUtilizationResponse)

-- | The total amount of cost\/commitment that you used your Savings Plans,
-- regardless of date ranges.
getSavingsPlansUtilizationResponse_total :: Lens.Lens' GetSavingsPlansUtilizationResponse SavingsPlansUtilizationAggregates
getSavingsPlansUtilizationResponse_total = Lens.lens (\GetSavingsPlansUtilizationResponse' {total} -> total) (\s@GetSavingsPlansUtilizationResponse' {} a -> s {total = a} :: GetSavingsPlansUtilizationResponse)

instance
  Prelude.NFData
    GetSavingsPlansUtilizationResponse
