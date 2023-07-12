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
-- Module      : Amazonka.CostExplorer.GetSavingsPlansCoverage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.CostExplorer.GetSavingsPlansCoverage
  ( -- * Creating a Request
    GetSavingsPlansCoverage (..),
    newGetSavingsPlansCoverage,

    -- * Request Lenses
    getSavingsPlansCoverage_filter,
    getSavingsPlansCoverage_granularity,
    getSavingsPlansCoverage_groupBy,
    getSavingsPlansCoverage_maxResults,
    getSavingsPlansCoverage_metrics,
    getSavingsPlansCoverage_nextToken,
    getSavingsPlansCoverage_sortBy,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSavingsPlansCoverage' smart constructor.
data GetSavingsPlansCoverage = GetSavingsPlansCoverage'
  { -- | Filters Savings Plans coverage data by dimensions. You can filter data
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
    filter' :: Prelude.Maybe Expression,
    -- | The granularity of the Amazon Web Services cost data for your Savings
    -- Plans. @Granularity@ can\'t be set if @GroupBy@ is set.
    --
    -- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and
    -- @MONTHLY@ granularities.
    granularity :: Prelude.Maybe Granularity,
    -- | You can group the data using the attributes @INSTANCE_FAMILY@, @REGION@,
    -- or @SERVICE@.
    groupBy :: Prelude.Maybe [GroupDefinition],
    -- | The number of items to be returned in a response. The default is @20@,
    -- with a minimum value of @1@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The measurement that you want your Savings Plans coverage reported in.
    -- The only valid value is @SpendCoveredBySavingsPlans@.
    metrics :: Prelude.Maybe [Prelude.Text],
    -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The value that you want to sort the data by.
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
    -- The supported values for @SortOrder@ are @ASCENDING@ and @DESCENDING@.
    sortBy :: Prelude.Maybe SortDefinition,
    -- | The time period that you want the usage and costs for. The @Start@ date
    -- must be within 13 months. The @End@ date must be after the @Start@ date,
    -- and before the current date. Future dates can\'t be used as an @End@
    -- date.
    timePeriod :: DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSavingsPlansCoverage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'granularity', 'getSavingsPlansCoverage_granularity' - The granularity of the Amazon Web Services cost data for your Savings
-- Plans. @Granularity@ can\'t be set if @GroupBy@ is set.
--
-- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and
-- @MONTHLY@ granularities.
--
-- 'groupBy', 'getSavingsPlansCoverage_groupBy' - You can group the data using the attributes @INSTANCE_FAMILY@, @REGION@,
-- or @SERVICE@.
--
-- 'maxResults', 'getSavingsPlansCoverage_maxResults' - The number of items to be returned in a response. The default is @20@,
-- with a minimum value of @1@.
--
-- 'metrics', 'getSavingsPlansCoverage_metrics' - The measurement that you want your Savings Plans coverage reported in.
-- The only valid value is @SpendCoveredBySavingsPlans@.
--
-- 'nextToken', 'getSavingsPlansCoverage_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'sortBy', 'getSavingsPlansCoverage_sortBy' - The value that you want to sort the data by.
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
-- The supported values for @SortOrder@ are @ASCENDING@ and @DESCENDING@.
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
    { filter' = Prelude.Nothing,
      granularity = Prelude.Nothing,
      groupBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      metrics = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      timePeriod = pTimePeriod_
    }

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
getSavingsPlansCoverage_filter :: Lens.Lens' GetSavingsPlansCoverage (Prelude.Maybe Expression)
getSavingsPlansCoverage_filter = Lens.lens (\GetSavingsPlansCoverage' {filter'} -> filter') (\s@GetSavingsPlansCoverage' {} a -> s {filter' = a} :: GetSavingsPlansCoverage)

-- | The granularity of the Amazon Web Services cost data for your Savings
-- Plans. @Granularity@ can\'t be set if @GroupBy@ is set.
--
-- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and
-- @MONTHLY@ granularities.
getSavingsPlansCoverage_granularity :: Lens.Lens' GetSavingsPlansCoverage (Prelude.Maybe Granularity)
getSavingsPlansCoverage_granularity = Lens.lens (\GetSavingsPlansCoverage' {granularity} -> granularity) (\s@GetSavingsPlansCoverage' {} a -> s {granularity = a} :: GetSavingsPlansCoverage)

-- | You can group the data using the attributes @INSTANCE_FAMILY@, @REGION@,
-- or @SERVICE@.
getSavingsPlansCoverage_groupBy :: Lens.Lens' GetSavingsPlansCoverage (Prelude.Maybe [GroupDefinition])
getSavingsPlansCoverage_groupBy = Lens.lens (\GetSavingsPlansCoverage' {groupBy} -> groupBy) (\s@GetSavingsPlansCoverage' {} a -> s {groupBy = a} :: GetSavingsPlansCoverage) Prelude.. Lens.mapping Lens.coerced

-- | The number of items to be returned in a response. The default is @20@,
-- with a minimum value of @1@.
getSavingsPlansCoverage_maxResults :: Lens.Lens' GetSavingsPlansCoverage (Prelude.Maybe Prelude.Natural)
getSavingsPlansCoverage_maxResults = Lens.lens (\GetSavingsPlansCoverage' {maxResults} -> maxResults) (\s@GetSavingsPlansCoverage' {} a -> s {maxResults = a} :: GetSavingsPlansCoverage)

-- | The measurement that you want your Savings Plans coverage reported in.
-- The only valid value is @SpendCoveredBySavingsPlans@.
getSavingsPlansCoverage_metrics :: Lens.Lens' GetSavingsPlansCoverage (Prelude.Maybe [Prelude.Text])
getSavingsPlansCoverage_metrics = Lens.lens (\GetSavingsPlansCoverage' {metrics} -> metrics) (\s@GetSavingsPlansCoverage' {} a -> s {metrics = a} :: GetSavingsPlansCoverage) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansCoverage_nextToken :: Lens.Lens' GetSavingsPlansCoverage (Prelude.Maybe Prelude.Text)
getSavingsPlansCoverage_nextToken = Lens.lens (\GetSavingsPlansCoverage' {nextToken} -> nextToken) (\s@GetSavingsPlansCoverage' {} a -> s {nextToken = a} :: GetSavingsPlansCoverage)

-- | The value that you want to sort the data by.
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
-- The supported values for @SortOrder@ are @ASCENDING@ and @DESCENDING@.
getSavingsPlansCoverage_sortBy :: Lens.Lens' GetSavingsPlansCoverage (Prelude.Maybe SortDefinition)
getSavingsPlansCoverage_sortBy = Lens.lens (\GetSavingsPlansCoverage' {sortBy} -> sortBy) (\s@GetSavingsPlansCoverage' {} a -> s {sortBy = a} :: GetSavingsPlansCoverage)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSavingsPlansCoverageResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "SavingsPlansCoverages"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetSavingsPlansCoverage where
  hashWithSalt _salt GetSavingsPlansCoverage' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` granularity
      `Prelude.hashWithSalt` groupBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` timePeriod

instance Prelude.NFData GetSavingsPlansCoverage where
  rnf GetSavingsPlansCoverage' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf granularity
      `Prelude.seq` Prelude.rnf groupBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf timePeriod

instance Data.ToHeaders GetSavingsPlansCoverage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetSavingsPlansCoverage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSavingsPlansCoverage where
  toJSON GetSavingsPlansCoverage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("Granularity" Data..=) Prelude.<$> granularity,
            ("GroupBy" Data..=) Prelude.<$> groupBy,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("Metrics" Data..=) Prelude.<$> metrics,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            Prelude.Just ("TimePeriod" Data..= timePeriod)
          ]
      )

instance Data.ToPath GetSavingsPlansCoverage where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSavingsPlansCoverage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSavingsPlansCoverageResponse' smart constructor.
data GetSavingsPlansCoverageResponse = GetSavingsPlansCoverageResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The amount of spend that your Savings Plans covered.
    savingsPlansCoverages :: [SavingsPlansCoverage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetSavingsPlansCoverageResponse
newGetSavingsPlansCoverageResponse pHttpStatus_ =
  GetSavingsPlansCoverageResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      savingsPlansCoverages = Prelude.mempty
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansCoverageResponse_nextToken :: Lens.Lens' GetSavingsPlansCoverageResponse (Prelude.Maybe Prelude.Text)
getSavingsPlansCoverageResponse_nextToken = Lens.lens (\GetSavingsPlansCoverageResponse' {nextToken} -> nextToken) (\s@GetSavingsPlansCoverageResponse' {} a -> s {nextToken = a} :: GetSavingsPlansCoverageResponse)

-- | The response's http status code.
getSavingsPlansCoverageResponse_httpStatus :: Lens.Lens' GetSavingsPlansCoverageResponse Prelude.Int
getSavingsPlansCoverageResponse_httpStatus = Lens.lens (\GetSavingsPlansCoverageResponse' {httpStatus} -> httpStatus) (\s@GetSavingsPlansCoverageResponse' {} a -> s {httpStatus = a} :: GetSavingsPlansCoverageResponse)

-- | The amount of spend that your Savings Plans covered.
getSavingsPlansCoverageResponse_savingsPlansCoverages :: Lens.Lens' GetSavingsPlansCoverageResponse [SavingsPlansCoverage]
getSavingsPlansCoverageResponse_savingsPlansCoverages = Lens.lens (\GetSavingsPlansCoverageResponse' {savingsPlansCoverages} -> savingsPlansCoverages) (\s@GetSavingsPlansCoverageResponse' {} a -> s {savingsPlansCoverages = a} :: GetSavingsPlansCoverageResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetSavingsPlansCoverageResponse
  where
  rnf GetSavingsPlansCoverageResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf savingsPlansCoverages
