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
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansCoverage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Savings Plans covered for your account. This enables you
-- to see how much of your cost is covered by a Savings Plan. An
-- organization’s management account can see the coverage of the associated
-- member accounts. This supports dimensions, Cost Categories, and nested
-- expressions. For any time period, you can filter data for Savings Plans
-- usage with the following dimensions:
--
-- -   @LINKED_ACCOUNT@
--
-- -   @REGION@
--
-- -   @SERVICE@
--
-- -   @INSTANCE_FAMILY@
--
-- To determine valid values for a dimension, use the @GetDimensionValues@
-- operation.
module Network.AWS.CostExplorer.GetSavingsPlansCoverage
  ( -- * Creating a Request
    GetSavingsPlansCoverage (..),
    newGetSavingsPlansCoverage,

    -- * Request Lenses
    getSavingsPlansCoverage_nextToken,
    getSavingsPlansCoverage_granularity,
    getSavingsPlansCoverage_maxResults,
    getSavingsPlansCoverage_metrics,
    getSavingsPlansCoverage_groupBy,
    getSavingsPlansCoverage_sortBy,
    getSavingsPlansCoverage_filter,
    getSavingsPlansCoverage_timePeriod,

    -- * Destructuring the Response
    GetSavingsPlansCoverageResponse (..),
    newGetSavingsPlansCoverageResponse,

    -- * Response Lenses
    getSavingsPlansCoverageResponse_nextToken,
    getSavingsPlansCoverageResponse_httpStatus,
    getSavingsPlansCoverageResponse_savingsPlansCoverages,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSavingsPlansCoverage' smart constructor.
data GetSavingsPlansCoverage = GetSavingsPlansCoverage'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Core.Maybe Core.Text,
    -- | The granularity of the Amazon Web Services cost data for your Savings
    -- Plans. @Granularity@ can\'t be set if @GroupBy@ is set.
    --
    -- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and
    -- @MONTHLY@ granularities.
    granularity :: Core.Maybe Granularity,
    -- | The number of items to be returned in a response. The default is @20@,
    -- with a minimum value of @1@.
    maxResults :: Core.Maybe Core.Natural,
    -- | The measurement that you want your Savings Plans coverage reported in.
    -- The only valid value is @SpendCoveredBySavingsPlans@.
    metrics :: Core.Maybe [Core.Text],
    -- | You can group the data using the attributes @INSTANCE_FAMILY@, @REGION@,
    -- or @SERVICE@.
    groupBy :: Core.Maybe [GroupDefinition],
    -- | The value by which you want to sort the data.
    --
    -- The following values are supported for @Key@:
    --
    -- -   @SpendCoveredBySavingsPlan@
    --
    -- -   @OnDemandCost@
    --
    -- -   @CoveragePercentage@
    --
    -- -   @TotalCost@
    --
    -- -   @InstanceFamily@
    --
    -- -   @Region@
    --
    -- -   @Service@
    --
    -- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
    sortBy :: Core.Maybe SortDefinition,
    -- | Filters Savings Plans coverage data by dimensions. You can filter data
    -- for Savings Plans usage with the following dimensions:
    --
    -- -   @LINKED_ACCOUNT@
    --
    -- -   @REGION@
    --
    -- -   @SERVICE@
    --
    -- -   @INSTANCE_FAMILY@
    --
    -- @GetSavingsPlansCoverage@ uses the same
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
    -- object as the other operations, but only @AND@ is supported among each
    -- dimension. If there are multiple values for a dimension, they are OR\'d
    -- together.
    --
    -- Cost category is also supported.
    filter' :: Core.Maybe Expression,
    -- | The time period that you want the usage and costs for. The @Start@ date
    -- must be within 13 months. The @End@ date must be after the @Start@ date,
    -- and before the current date. Future dates can\'t be used as an @End@
    -- date.
    timePeriod :: DateInterval
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSavingsPlansCoverage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSavingsPlansCoverage_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'granularity', 'getSavingsPlansCoverage_granularity' - The granularity of the Amazon Web Services cost data for your Savings
-- Plans. @Granularity@ can\'t be set if @GroupBy@ is set.
--
-- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and
-- @MONTHLY@ granularities.
--
-- 'maxResults', 'getSavingsPlansCoverage_maxResults' - The number of items to be returned in a response. The default is @20@,
-- with a minimum value of @1@.
--
-- 'metrics', 'getSavingsPlansCoverage_metrics' - The measurement that you want your Savings Plans coverage reported in.
-- The only valid value is @SpendCoveredBySavingsPlans@.
--
-- 'groupBy', 'getSavingsPlansCoverage_groupBy' - You can group the data using the attributes @INSTANCE_FAMILY@, @REGION@,
-- or @SERVICE@.
--
-- 'sortBy', 'getSavingsPlansCoverage_sortBy' - The value by which you want to sort the data.
--
-- The following values are supported for @Key@:
--
-- -   @SpendCoveredBySavingsPlan@
--
-- -   @OnDemandCost@
--
-- -   @CoveragePercentage@
--
-- -   @TotalCost@
--
-- -   @InstanceFamily@
--
-- -   @Region@
--
-- -   @Service@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
--
-- 'filter'', 'getSavingsPlansCoverage_filter' - Filters Savings Plans coverage data by dimensions. You can filter data
-- for Savings Plans usage with the following dimensions:
--
-- -   @LINKED_ACCOUNT@
--
-- -   @REGION@
--
-- -   @SERVICE@
--
-- -   @INSTANCE_FAMILY@
--
-- @GetSavingsPlansCoverage@ uses the same
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object as the other operations, but only @AND@ is supported among each
-- dimension. If there are multiple values for a dimension, they are OR\'d
-- together.
--
-- Cost category is also supported.
--
-- 'timePeriod', 'getSavingsPlansCoverage_timePeriod' - The time period that you want the usage and costs for. The @Start@ date
-- must be within 13 months. The @End@ date must be after the @Start@ date,
-- and before the current date. Future dates can\'t be used as an @End@
-- date.
newGetSavingsPlansCoverage ::
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansCoverage
newGetSavingsPlansCoverage pTimePeriod_ =
  GetSavingsPlansCoverage'
    { nextToken = Core.Nothing,
      granularity = Core.Nothing,
      maxResults = Core.Nothing,
      metrics = Core.Nothing,
      groupBy = Core.Nothing,
      sortBy = Core.Nothing,
      filter' = Core.Nothing,
      timePeriod = pTimePeriod_
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansCoverage_nextToken :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe Core.Text)
getSavingsPlansCoverage_nextToken = Lens.lens (\GetSavingsPlansCoverage' {nextToken} -> nextToken) (\s@GetSavingsPlansCoverage' {} a -> s {nextToken = a} :: GetSavingsPlansCoverage)

-- | The granularity of the Amazon Web Services cost data for your Savings
-- Plans. @Granularity@ can\'t be set if @GroupBy@ is set.
--
-- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and
-- @MONTHLY@ granularities.
getSavingsPlansCoverage_granularity :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe Granularity)
getSavingsPlansCoverage_granularity = Lens.lens (\GetSavingsPlansCoverage' {granularity} -> granularity) (\s@GetSavingsPlansCoverage' {} a -> s {granularity = a} :: GetSavingsPlansCoverage)

-- | The number of items to be returned in a response. The default is @20@,
-- with a minimum value of @1@.
getSavingsPlansCoverage_maxResults :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe Core.Natural)
getSavingsPlansCoverage_maxResults = Lens.lens (\GetSavingsPlansCoverage' {maxResults} -> maxResults) (\s@GetSavingsPlansCoverage' {} a -> s {maxResults = a} :: GetSavingsPlansCoverage)

-- | The measurement that you want your Savings Plans coverage reported in.
-- The only valid value is @SpendCoveredBySavingsPlans@.
getSavingsPlansCoverage_metrics :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe [Core.Text])
getSavingsPlansCoverage_metrics = Lens.lens (\GetSavingsPlansCoverage' {metrics} -> metrics) (\s@GetSavingsPlansCoverage' {} a -> s {metrics = a} :: GetSavingsPlansCoverage) Core.. Lens.mapping Lens._Coerce

-- | You can group the data using the attributes @INSTANCE_FAMILY@, @REGION@,
-- or @SERVICE@.
getSavingsPlansCoverage_groupBy :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe [GroupDefinition])
getSavingsPlansCoverage_groupBy = Lens.lens (\GetSavingsPlansCoverage' {groupBy} -> groupBy) (\s@GetSavingsPlansCoverage' {} a -> s {groupBy = a} :: GetSavingsPlansCoverage) Core.. Lens.mapping Lens._Coerce

-- | The value by which you want to sort the data.
--
-- The following values are supported for @Key@:
--
-- -   @SpendCoveredBySavingsPlan@
--
-- -   @OnDemandCost@
--
-- -   @CoveragePercentage@
--
-- -   @TotalCost@
--
-- -   @InstanceFamily@
--
-- -   @Region@
--
-- -   @Service@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
getSavingsPlansCoverage_sortBy :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe SortDefinition)
getSavingsPlansCoverage_sortBy = Lens.lens (\GetSavingsPlansCoverage' {sortBy} -> sortBy) (\s@GetSavingsPlansCoverage' {} a -> s {sortBy = a} :: GetSavingsPlansCoverage)

-- | Filters Savings Plans coverage data by dimensions. You can filter data
-- for Savings Plans usage with the following dimensions:
--
-- -   @LINKED_ACCOUNT@
--
-- -   @REGION@
--
-- -   @SERVICE@
--
-- -   @INSTANCE_FAMILY@
--
-- @GetSavingsPlansCoverage@ uses the same
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object as the other operations, but only @AND@ is supported among each
-- dimension. If there are multiple values for a dimension, they are OR\'d
-- together.
--
-- Cost category is also supported.
getSavingsPlansCoverage_filter :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe Expression)
getSavingsPlansCoverage_filter = Lens.lens (\GetSavingsPlansCoverage' {filter'} -> filter') (\s@GetSavingsPlansCoverage' {} a -> s {filter' = a} :: GetSavingsPlansCoverage)

-- | The time period that you want the usage and costs for. The @Start@ date
-- must be within 13 months. The @End@ date must be after the @Start@ date,
-- and before the current date. Future dates can\'t be used as an @End@
-- date.
getSavingsPlansCoverage_timePeriod :: Lens.Lens' GetSavingsPlansCoverage DateInterval
getSavingsPlansCoverage_timePeriod = Lens.lens (\GetSavingsPlansCoverage' {timePeriod} -> timePeriod) (\s@GetSavingsPlansCoverage' {} a -> s {timePeriod = a} :: GetSavingsPlansCoverage)

instance Core.AWSRequest GetSavingsPlansCoverage where
  type
    AWSResponse GetSavingsPlansCoverage =
      GetSavingsPlansCoverageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSavingsPlansCoverageResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "SavingsPlansCoverages"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable GetSavingsPlansCoverage

instance Core.NFData GetSavingsPlansCoverage

instance Core.ToHeaders GetSavingsPlansCoverage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetSavingsPlansCoverage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSavingsPlansCoverage where
  toJSON GetSavingsPlansCoverage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Granularity" Core..=) Core.<$> granularity,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Metrics" Core..=) Core.<$> metrics,
            ("GroupBy" Core..=) Core.<$> groupBy,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("Filter" Core..=) Core.<$> filter',
            Core.Just ("TimePeriod" Core..= timePeriod)
          ]
      )

instance Core.ToPath GetSavingsPlansCoverage where
  toPath = Core.const "/"

instance Core.ToQuery GetSavingsPlansCoverage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSavingsPlansCoverageResponse' smart constructor.
data GetSavingsPlansCoverageResponse = GetSavingsPlansCoverageResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The amount of spend that your Savings Plans covered.
    savingsPlansCoverages :: [SavingsPlansCoverage]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSavingsPlansCoverageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSavingsPlansCoverageResponse_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'httpStatus', 'getSavingsPlansCoverageResponse_httpStatus' - The response's http status code.
--
-- 'savingsPlansCoverages', 'getSavingsPlansCoverageResponse_savingsPlansCoverages' - The amount of spend that your Savings Plans covered.
newGetSavingsPlansCoverageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSavingsPlansCoverageResponse
newGetSavingsPlansCoverageResponse pHttpStatus_ =
  GetSavingsPlansCoverageResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      savingsPlansCoverages = Core.mempty
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansCoverageResponse_nextToken :: Lens.Lens' GetSavingsPlansCoverageResponse (Core.Maybe Core.Text)
getSavingsPlansCoverageResponse_nextToken = Lens.lens (\GetSavingsPlansCoverageResponse' {nextToken} -> nextToken) (\s@GetSavingsPlansCoverageResponse' {} a -> s {nextToken = a} :: GetSavingsPlansCoverageResponse)

-- | The response's http status code.
getSavingsPlansCoverageResponse_httpStatus :: Lens.Lens' GetSavingsPlansCoverageResponse Core.Int
getSavingsPlansCoverageResponse_httpStatus = Lens.lens (\GetSavingsPlansCoverageResponse' {httpStatus} -> httpStatus) (\s@GetSavingsPlansCoverageResponse' {} a -> s {httpStatus = a} :: GetSavingsPlansCoverageResponse)

-- | The amount of spend that your Savings Plans covered.
getSavingsPlansCoverageResponse_savingsPlansCoverages :: Lens.Lens' GetSavingsPlansCoverageResponse [SavingsPlansCoverage]
getSavingsPlansCoverageResponse_savingsPlansCoverages = Lens.lens (\GetSavingsPlansCoverageResponse' {savingsPlansCoverages} -> savingsPlansCoverages) (\s@GetSavingsPlansCoverageResponse' {} a -> s {savingsPlansCoverages = a} :: GetSavingsPlansCoverageResponse) Core.. Lens._Coerce

instance Core.NFData GetSavingsPlansCoverageResponse
