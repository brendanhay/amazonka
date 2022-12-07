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
-- Module      : Amazonka.CostExplorer.GetSavingsPlansUtilizationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attribute data along with aggregate utilization and savings
-- data for a given time period. This doesn\'t support granular or grouped
-- data (daily\/monthly) in response. You can\'t retrieve data by dates in
-- a single response similar to @GetSavingsPlanUtilization@, but you have
-- the option to make multiple calls to @GetSavingsPlanUtilizationDetails@
-- by providing individual dates. You can use @GetDimensionValues@ in
-- @SAVINGS_PLANS@ to determine the possible dimension values.
--
-- @GetSavingsPlanUtilizationDetails@ internally groups data by
-- @SavingsPlansArn@.
module Amazonka.CostExplorer.GetSavingsPlansUtilizationDetails
  ( -- * Creating a Request
    GetSavingsPlansUtilizationDetails (..),
    newGetSavingsPlansUtilizationDetails,

    -- * Request Lenses
    getSavingsPlansUtilizationDetails_nextToken,
    getSavingsPlansUtilizationDetails_sortBy,
    getSavingsPlansUtilizationDetails_filter,
    getSavingsPlansUtilizationDetails_maxResults,
    getSavingsPlansUtilizationDetails_dataType,
    getSavingsPlansUtilizationDetails_timePeriod,

    -- * Destructuring the Response
    GetSavingsPlansUtilizationDetailsResponse (..),
    newGetSavingsPlansUtilizationDetailsResponse,

    -- * Response Lenses
    getSavingsPlansUtilizationDetailsResponse_nextToken,
    getSavingsPlansUtilizationDetailsResponse_total,
    getSavingsPlansUtilizationDetailsResponse_httpStatus,
    getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails,
    getSavingsPlansUtilizationDetailsResponse_timePeriod,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSavingsPlansUtilizationDetails' smart constructor.
data GetSavingsPlansUtilizationDetails = GetSavingsPlansUtilizationDetails'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The value that you want to sort the data by.
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
    -- -   @AmortizedRecurringCommitment@
    --
    -- -   @AmortizedUpfrontCommitment@
    --
    -- The supported values for @SortOrder@ are @ASCENDING@ and @DESCENDING@.
    sortBy :: Prelude.Maybe SortDefinition,
    -- | Filters Savings Plans utilization coverage data for active Savings Plans
    -- dimensions. You can filter data with the following dimensions:
    --
    -- -   @LINKED_ACCOUNT@
    --
    -- -   @SAVINGS_PLAN_ARN@
    --
    -- -   @REGION@
    --
    -- -   @PAYMENT_OPTION@
    --
    -- -   @INSTANCE_TYPE_FAMILY@
    --
    -- @GetSavingsPlansUtilizationDetails@ uses the same
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
    -- object as the other operations, but only @AND@ is supported among each
    -- dimension.
    filter' :: Prelude.Maybe Expression,
    -- | The number of items to be returned in a response. The default is @20@,
    -- with a minimum value of @1@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The data type.
    dataType :: Prelude.Maybe [SavingsPlansDataType],
    -- | The time period that you want the usage and costs for. The @Start@ date
    -- must be within 13 months. The @End@ date must be after the @Start@ date,
    -- and before the current date. Future dates can\'t be used as an @End@
    -- date.
    timePeriod :: DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSavingsPlansUtilizationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSavingsPlansUtilizationDetails_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'sortBy', 'getSavingsPlansUtilizationDetails_sortBy' - The value that you want to sort the data by.
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
-- -   @AmortizedRecurringCommitment@
--
-- -   @AmortizedUpfrontCommitment@
--
-- The supported values for @SortOrder@ are @ASCENDING@ and @DESCENDING@.
--
-- 'filter'', 'getSavingsPlansUtilizationDetails_filter' - Filters Savings Plans utilization coverage data for active Savings Plans
-- dimensions. You can filter data with the following dimensions:
--
-- -   @LINKED_ACCOUNT@
--
-- -   @SAVINGS_PLAN_ARN@
--
-- -   @REGION@
--
-- -   @PAYMENT_OPTION@
--
-- -   @INSTANCE_TYPE_FAMILY@
--
-- @GetSavingsPlansUtilizationDetails@ uses the same
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object as the other operations, but only @AND@ is supported among each
-- dimension.
--
-- 'maxResults', 'getSavingsPlansUtilizationDetails_maxResults' - The number of items to be returned in a response. The default is @20@,
-- with a minimum value of @1@.
--
-- 'dataType', 'getSavingsPlansUtilizationDetails_dataType' - The data type.
--
-- 'timePeriod', 'getSavingsPlansUtilizationDetails_timePeriod' - The time period that you want the usage and costs for. The @Start@ date
-- must be within 13 months. The @End@ date must be after the @Start@ date,
-- and before the current date. Future dates can\'t be used as an @End@
-- date.
newGetSavingsPlansUtilizationDetails ::
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansUtilizationDetails
newGetSavingsPlansUtilizationDetails pTimePeriod_ =
  GetSavingsPlansUtilizationDetails'
    { nextToken =
        Prelude.Nothing,
      sortBy = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dataType = Prelude.Nothing,
      timePeriod = pTimePeriod_
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansUtilizationDetails_nextToken :: Lens.Lens' GetSavingsPlansUtilizationDetails (Prelude.Maybe Prelude.Text)
getSavingsPlansUtilizationDetails_nextToken = Lens.lens (\GetSavingsPlansUtilizationDetails' {nextToken} -> nextToken) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {nextToken = a} :: GetSavingsPlansUtilizationDetails)

-- | The value that you want to sort the data by.
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
-- -   @AmortizedRecurringCommitment@
--
-- -   @AmortizedUpfrontCommitment@
--
-- The supported values for @SortOrder@ are @ASCENDING@ and @DESCENDING@.
getSavingsPlansUtilizationDetails_sortBy :: Lens.Lens' GetSavingsPlansUtilizationDetails (Prelude.Maybe SortDefinition)
getSavingsPlansUtilizationDetails_sortBy = Lens.lens (\GetSavingsPlansUtilizationDetails' {sortBy} -> sortBy) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {sortBy = a} :: GetSavingsPlansUtilizationDetails)

-- | Filters Savings Plans utilization coverage data for active Savings Plans
-- dimensions. You can filter data with the following dimensions:
--
-- -   @LINKED_ACCOUNT@
--
-- -   @SAVINGS_PLAN_ARN@
--
-- -   @REGION@
--
-- -   @PAYMENT_OPTION@
--
-- -   @INSTANCE_TYPE_FAMILY@
--
-- @GetSavingsPlansUtilizationDetails@ uses the same
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object as the other operations, but only @AND@ is supported among each
-- dimension.
getSavingsPlansUtilizationDetails_filter :: Lens.Lens' GetSavingsPlansUtilizationDetails (Prelude.Maybe Expression)
getSavingsPlansUtilizationDetails_filter = Lens.lens (\GetSavingsPlansUtilizationDetails' {filter'} -> filter') (\s@GetSavingsPlansUtilizationDetails' {} a -> s {filter' = a} :: GetSavingsPlansUtilizationDetails)

-- | The number of items to be returned in a response. The default is @20@,
-- with a minimum value of @1@.
getSavingsPlansUtilizationDetails_maxResults :: Lens.Lens' GetSavingsPlansUtilizationDetails (Prelude.Maybe Prelude.Natural)
getSavingsPlansUtilizationDetails_maxResults = Lens.lens (\GetSavingsPlansUtilizationDetails' {maxResults} -> maxResults) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {maxResults = a} :: GetSavingsPlansUtilizationDetails)

-- | The data type.
getSavingsPlansUtilizationDetails_dataType :: Lens.Lens' GetSavingsPlansUtilizationDetails (Prelude.Maybe [SavingsPlansDataType])
getSavingsPlansUtilizationDetails_dataType = Lens.lens (\GetSavingsPlansUtilizationDetails' {dataType} -> dataType) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {dataType = a} :: GetSavingsPlansUtilizationDetails) Prelude.. Lens.mapping Lens.coerced

-- | The time period that you want the usage and costs for. The @Start@ date
-- must be within 13 months. The @End@ date must be after the @Start@ date,
-- and before the current date. Future dates can\'t be used as an @End@
-- date.
getSavingsPlansUtilizationDetails_timePeriod :: Lens.Lens' GetSavingsPlansUtilizationDetails DateInterval
getSavingsPlansUtilizationDetails_timePeriod = Lens.lens (\GetSavingsPlansUtilizationDetails' {timePeriod} -> timePeriod) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {timePeriod = a} :: GetSavingsPlansUtilizationDetails)

instance
  Core.AWSRequest
    GetSavingsPlansUtilizationDetails
  where
  type
    AWSResponse GetSavingsPlansUtilizationDetails =
      GetSavingsPlansUtilizationDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSavingsPlansUtilizationDetailsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "Total")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Data..?> "SavingsPlansUtilizationDetails"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (x Data..:> "TimePeriod")
      )

instance
  Prelude.Hashable
    GetSavingsPlansUtilizationDetails
  where
  hashWithSalt
    _salt
    GetSavingsPlansUtilizationDetails' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` sortBy
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` dataType
        `Prelude.hashWithSalt` timePeriod

instance
  Prelude.NFData
    GetSavingsPlansUtilizationDetails
  where
  rnf GetSavingsPlansUtilizationDetails' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf timePeriod

instance
  Data.ToHeaders
    GetSavingsPlansUtilizationDetails
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetSavingsPlansUtilizationDetails" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetSavingsPlansUtilizationDetails
  where
  toJSON GetSavingsPlansUtilizationDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("DataType" Data..=) Prelude.<$> dataType,
            Prelude.Just ("TimePeriod" Data..= timePeriod)
          ]
      )

instance
  Data.ToPath
    GetSavingsPlansUtilizationDetails
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetSavingsPlansUtilizationDetails
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSavingsPlansUtilizationDetailsResponse' smart constructor.
data GetSavingsPlansUtilizationDetailsResponse = GetSavingsPlansUtilizationDetailsResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total Savings Plans utilization, regardless of time period.
    total :: Prelude.Maybe SavingsPlansUtilizationAggregates,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Retrieves a single daily or monthly Savings Plans utilization rate and
    -- details for your account.
    savingsPlansUtilizationDetails :: [SavingsPlansUtilizationDetail],
    timePeriod :: DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSavingsPlansUtilizationDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSavingsPlansUtilizationDetailsResponse_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'total', 'getSavingsPlansUtilizationDetailsResponse_total' - The total Savings Plans utilization, regardless of time period.
--
-- 'httpStatus', 'getSavingsPlansUtilizationDetailsResponse_httpStatus' - The response's http status code.
--
-- 'savingsPlansUtilizationDetails', 'getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails' - Retrieves a single daily or monthly Savings Plans utilization rate and
-- details for your account.
--
-- 'timePeriod', 'getSavingsPlansUtilizationDetailsResponse_timePeriod' - Undocumented member.
newGetSavingsPlansUtilizationDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansUtilizationDetailsResponse
newGetSavingsPlansUtilizationDetailsResponse
  pHttpStatus_
  pTimePeriod_ =
    GetSavingsPlansUtilizationDetailsResponse'
      { nextToken =
          Prelude.Nothing,
        total = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        savingsPlansUtilizationDetails =
          Prelude.mempty,
        timePeriod = pTimePeriod_
      }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansUtilizationDetailsResponse_nextToken :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse (Prelude.Maybe Prelude.Text)
getSavingsPlansUtilizationDetailsResponse_nextToken = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {nextToken} -> nextToken) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {nextToken = a} :: GetSavingsPlansUtilizationDetailsResponse)

-- | The total Savings Plans utilization, regardless of time period.
getSavingsPlansUtilizationDetailsResponse_total :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse (Prelude.Maybe SavingsPlansUtilizationAggregates)
getSavingsPlansUtilizationDetailsResponse_total = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {total} -> total) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {total = a} :: GetSavingsPlansUtilizationDetailsResponse)

-- | The response's http status code.
getSavingsPlansUtilizationDetailsResponse_httpStatus :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse Prelude.Int
getSavingsPlansUtilizationDetailsResponse_httpStatus = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {httpStatus} -> httpStatus) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {httpStatus = a} :: GetSavingsPlansUtilizationDetailsResponse)

-- | Retrieves a single daily or monthly Savings Plans utilization rate and
-- details for your account.
getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse [SavingsPlansUtilizationDetail]
getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {savingsPlansUtilizationDetails} -> savingsPlansUtilizationDetails) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {savingsPlansUtilizationDetails = a} :: GetSavingsPlansUtilizationDetailsResponse) Prelude.. Lens.coerced

-- | Undocumented member.
getSavingsPlansUtilizationDetailsResponse_timePeriod :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse DateInterval
getSavingsPlansUtilizationDetailsResponse_timePeriod = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {timePeriod} -> timePeriod) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {timePeriod = a} :: GetSavingsPlansUtilizationDetailsResponse)

instance
  Prelude.NFData
    GetSavingsPlansUtilizationDetailsResponse
  where
  rnf GetSavingsPlansUtilizationDetailsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf savingsPlansUtilizationDetails
      `Prelude.seq` Prelude.rnf timePeriod
