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
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
  ( -- * Creating a Request
    GetSavingsPlansUtilizationDetails (..),
    newGetSavingsPlansUtilizationDetails,

    -- * Request Lenses
    getSavingsPlansUtilizationDetails_nextToken,
    getSavingsPlansUtilizationDetails_maxResults,
    getSavingsPlansUtilizationDetails_sortBy,
    getSavingsPlansUtilizationDetails_dataType,
    getSavingsPlansUtilizationDetails_filter,
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

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSavingsPlansUtilizationDetails' smart constructor.
data GetSavingsPlansUtilizationDetails = GetSavingsPlansUtilizationDetails'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of items to be returned in a response. The default is @20@,
    -- with a minimum value of @1@.
    maxResults :: Core.Maybe Core.Natural,
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
    -- -   @AmortizedRecurringCommitment@
    --
    -- -   @AmortizedUpfrontCommitment@
    --
    -- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
    sortBy :: Core.Maybe SortDefinition,
    -- | The data type.
    dataType :: Core.Maybe [SavingsPlansDataType],
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
    filter' :: Core.Maybe Expression,
    -- | The time period that you want the usage and costs for. The @Start@ date
    -- must be within 13 months. The @End@ date must be after the @Start@ date,
    -- and before the current date. Future dates can\'t be used as an @End@
    -- date.
    timePeriod :: DateInterval
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxResults', 'getSavingsPlansUtilizationDetails_maxResults' - The number of items to be returned in a response. The default is @20@,
-- with a minimum value of @1@.
--
-- 'sortBy', 'getSavingsPlansUtilizationDetails_sortBy' - The value by which you want to sort the data.
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
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
--
-- 'dataType', 'getSavingsPlansUtilizationDetails_dataType' - The data type.
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
        Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing,
      dataType = Core.Nothing,
      filter' = Core.Nothing,
      timePeriod = pTimePeriod_
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansUtilizationDetails_nextToken :: Lens.Lens' GetSavingsPlansUtilizationDetails (Core.Maybe Core.Text)
getSavingsPlansUtilizationDetails_nextToken = Lens.lens (\GetSavingsPlansUtilizationDetails' {nextToken} -> nextToken) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {nextToken = a} :: GetSavingsPlansUtilizationDetails)

-- | The number of items to be returned in a response. The default is @20@,
-- with a minimum value of @1@.
getSavingsPlansUtilizationDetails_maxResults :: Lens.Lens' GetSavingsPlansUtilizationDetails (Core.Maybe Core.Natural)
getSavingsPlansUtilizationDetails_maxResults = Lens.lens (\GetSavingsPlansUtilizationDetails' {maxResults} -> maxResults) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {maxResults = a} :: GetSavingsPlansUtilizationDetails)

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
-- -   @AmortizedRecurringCommitment@
--
-- -   @AmortizedUpfrontCommitment@
--
-- Supported values for @SortOrder@ are @ASCENDING@ or @DESCENDING@.
getSavingsPlansUtilizationDetails_sortBy :: Lens.Lens' GetSavingsPlansUtilizationDetails (Core.Maybe SortDefinition)
getSavingsPlansUtilizationDetails_sortBy = Lens.lens (\GetSavingsPlansUtilizationDetails' {sortBy} -> sortBy) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {sortBy = a} :: GetSavingsPlansUtilizationDetails)

-- | The data type.
getSavingsPlansUtilizationDetails_dataType :: Lens.Lens' GetSavingsPlansUtilizationDetails (Core.Maybe [SavingsPlansDataType])
getSavingsPlansUtilizationDetails_dataType = Lens.lens (\GetSavingsPlansUtilizationDetails' {dataType} -> dataType) (\s@GetSavingsPlansUtilizationDetails' {} a -> s {dataType = a} :: GetSavingsPlansUtilizationDetails) Core.. Lens.mapping Lens._Coerce

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
getSavingsPlansUtilizationDetails_filter :: Lens.Lens' GetSavingsPlansUtilizationDetails (Core.Maybe Expression)
getSavingsPlansUtilizationDetails_filter = Lens.lens (\GetSavingsPlansUtilizationDetails' {filter'} -> filter') (\s@GetSavingsPlansUtilizationDetails' {} a -> s {filter' = a} :: GetSavingsPlansUtilizationDetails)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSavingsPlansUtilizationDetailsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Total")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "SavingsPlansUtilizationDetails"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..:> "TimePeriod")
      )

instance
  Core.Hashable
    GetSavingsPlansUtilizationDetails

instance
  Core.NFData
    GetSavingsPlansUtilizationDetails

instance
  Core.ToHeaders
    GetSavingsPlansUtilizationDetails
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetSavingsPlansUtilizationDetails" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetSavingsPlansUtilizationDetails
  where
  toJSON GetSavingsPlansUtilizationDetails' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("DataType" Core..=) Core.<$> dataType,
            ("Filter" Core..=) Core.<$> filter',
            Core.Just ("TimePeriod" Core..= timePeriod)
          ]
      )

instance
  Core.ToPath
    GetSavingsPlansUtilizationDetails
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetSavingsPlansUtilizationDetails
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSavingsPlansUtilizationDetailsResponse' smart constructor.
data GetSavingsPlansUtilizationDetailsResponse = GetSavingsPlansUtilizationDetailsResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Core.Maybe Core.Text,
    -- | The total Savings Plans utilization, regardless of time period.
    total :: Core.Maybe SavingsPlansUtilizationAggregates,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Retrieves a single daily or monthly Savings Plans utilization rate and
    -- details for your account.
    savingsPlansUtilizationDetails :: [SavingsPlansUtilizationDetail],
    timePeriod :: DateInterval
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansUtilizationDetailsResponse
newGetSavingsPlansUtilizationDetailsResponse
  pHttpStatus_
  pTimePeriod_ =
    GetSavingsPlansUtilizationDetailsResponse'
      { nextToken =
          Core.Nothing,
        total = Core.Nothing,
        httpStatus = pHttpStatus_,
        savingsPlansUtilizationDetails =
          Core.mempty,
        timePeriod = pTimePeriod_
      }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getSavingsPlansUtilizationDetailsResponse_nextToken :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse (Core.Maybe Core.Text)
getSavingsPlansUtilizationDetailsResponse_nextToken = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {nextToken} -> nextToken) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {nextToken = a} :: GetSavingsPlansUtilizationDetailsResponse)

-- | The total Savings Plans utilization, regardless of time period.
getSavingsPlansUtilizationDetailsResponse_total :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse (Core.Maybe SavingsPlansUtilizationAggregates)
getSavingsPlansUtilizationDetailsResponse_total = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {total} -> total) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {total = a} :: GetSavingsPlansUtilizationDetailsResponse)

-- | The response's http status code.
getSavingsPlansUtilizationDetailsResponse_httpStatus :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse Core.Int
getSavingsPlansUtilizationDetailsResponse_httpStatus = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {httpStatus} -> httpStatus) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {httpStatus = a} :: GetSavingsPlansUtilizationDetailsResponse)

-- | Retrieves a single daily or monthly Savings Plans utilization rate and
-- details for your account.
getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse [SavingsPlansUtilizationDetail]
getSavingsPlansUtilizationDetailsResponse_savingsPlansUtilizationDetails = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {savingsPlansUtilizationDetails} -> savingsPlansUtilizationDetails) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {savingsPlansUtilizationDetails = a} :: GetSavingsPlansUtilizationDetailsResponse) Core.. Lens._Coerce

-- | Undocumented member.
getSavingsPlansUtilizationDetailsResponse_timePeriod :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse DateInterval
getSavingsPlansUtilizationDetailsResponse_timePeriod = Lens.lens (\GetSavingsPlansUtilizationDetailsResponse' {timePeriod} -> timePeriod) (\s@GetSavingsPlansUtilizationDetailsResponse' {} a -> s {timePeriod = a} :: GetSavingsPlansUtilizationDetailsResponse)

instance
  Core.NFData
    GetSavingsPlansUtilizationDetailsResponse
