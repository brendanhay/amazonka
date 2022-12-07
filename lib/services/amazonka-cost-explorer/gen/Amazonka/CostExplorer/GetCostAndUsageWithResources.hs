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
-- Module      : Amazonka.CostExplorer.GetCostAndUsageWithResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves cost and usage metrics with resources for your account. You
-- can specify which cost and usage-related metric, such as @BlendedCosts@
-- or @UsageQuantity@, that you want the request to return. You can also
-- filter and group your data by various dimensions, such as @SERVICE@ or
-- @AZ@, in a specific time range. For a complete list of valid dimensions,
-- see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html GetDimensionValues>
-- operation. Management account in an organization in Organizations have
-- access to all member accounts. This API is currently available for the
-- Amazon Elastic Compute Cloud – Compute service only.
--
-- This is an opt-in only feature. You can enable this feature from the
-- Cost Explorer Settings page. For information about how to access the
-- Settings page, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/ce-access.html Controlling Access for Cost Explorer>
-- in the /Billing and Cost Management User Guide/.
module Amazonka.CostExplorer.GetCostAndUsageWithResources
  ( -- * Creating a Request
    GetCostAndUsageWithResources (..),
    newGetCostAndUsageWithResources,

    -- * Request Lenses
    getCostAndUsageWithResources_nextPageToken,
    getCostAndUsageWithResources_groupBy,
    getCostAndUsageWithResources_metrics,
    getCostAndUsageWithResources_timePeriod,
    getCostAndUsageWithResources_granularity,
    getCostAndUsageWithResources_filter,

    -- * Destructuring the Response
    GetCostAndUsageWithResourcesResponse (..),
    newGetCostAndUsageWithResourcesResponse,

    -- * Response Lenses
    getCostAndUsageWithResourcesResponse_nextPageToken,
    getCostAndUsageWithResourcesResponse_dimensionValueAttributes,
    getCostAndUsageWithResourcesResponse_groupDefinitions,
    getCostAndUsageWithResourcesResponse_resultsByTime,
    getCostAndUsageWithResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCostAndUsageWithResources' smart constructor.
data GetCostAndUsageWithResources = GetCostAndUsageWithResources'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | You can group Amazon Web Services costs using up to two different
    -- groups: @DIMENSION@, @TAG@, @COST_CATEGORY@.
    groupBy :: Prelude.Maybe [GroupDefinition],
    -- | Which metrics are returned in the query. For more information about
    -- blended and unblended rates, see
    -- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
    --
    -- Valid values are @AmortizedCost@, @BlendedCost@, @NetAmortizedCost@,
    -- @NetUnblendedCost@, @NormalizedUsageAmount@, @UnblendedCost@, and
    -- @UsageQuantity@.
    --
    -- If you return the @UsageQuantity@ metric, the service aggregates all
    -- usage numbers without taking the units into account. For example, if you
    -- aggregate @usageQuantity@ across all of Amazon EC2, the results aren\'t
    -- meaningful because Amazon EC2 compute hours and data transfer are
    -- measured in different units (for example, hour or GB). To get more
    -- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
    -- @UsageTypeGroups@.
    --
    -- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
    metrics :: Prelude.Maybe [Prelude.Text],
    -- | Sets the start and end dates for retrieving Amazon Web Services costs.
    -- The range must be within the last 14 days (the start date cannot be
    -- earlier than 14 days ago). The start date is inclusive, but the end date
    -- is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
    -- @2017-05-01@, then the cost and usage data is retrieved from
    -- @2017-01-01@ up to and including @2017-04-30@ but not including
    -- @2017-05-01@.
    timePeriod :: DateInterval,
    -- | Sets the Amazon Web Services cost granularity to @MONTHLY@, @DAILY@, or
    -- @HOURLY@. If @Granularity@ isn\'t set, the response object doesn\'t
    -- include the @Granularity@, @MONTHLY@, @DAILY@, or @HOURLY@.
    granularity :: Granularity,
    -- | Filters Amazon Web Services costs by different dimensions. For example,
    -- you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that
    -- are associated with that account\'s usage of that service. You can nest
    -- @Expression@ objects to define any combination of dimension filters. For
    -- more information, see
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
    --
    -- The @GetCostAndUsageWithResources@ operation requires that you either
    -- group by or filter by a @ResourceId@. It requires the
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
    -- @\"SERVICE = Amazon Elastic Compute Cloud - Compute\"@ in the filter.
    --
    -- Valid values for @MatchOptions@ for @Dimensions@ are @EQUALS@ and
    -- @CASE_SENSITIVE@.
    --
    -- Valid values for @MatchOptions@ for @CostCategories@ and @Tags@ are
    -- @EQUALS@, @ABSENT@, and @CASE_SENSITIVE@. Default values are @EQUALS@
    -- and @CASE_SENSITIVE@.
    filter' :: Expression
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostAndUsageWithResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getCostAndUsageWithResources_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'groupBy', 'getCostAndUsageWithResources_groupBy' - You can group Amazon Web Services costs using up to two different
-- groups: @DIMENSION@, @TAG@, @COST_CATEGORY@.
--
-- 'metrics', 'getCostAndUsageWithResources_metrics' - Which metrics are returned in the query. For more information about
-- blended and unblended rates, see
-- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
--
-- Valid values are @AmortizedCost@, @BlendedCost@, @NetAmortizedCost@,
-- @NetUnblendedCost@, @NormalizedUsageAmount@, @UnblendedCost@, and
-- @UsageQuantity@.
--
-- If you return the @UsageQuantity@ metric, the service aggregates all
-- usage numbers without taking the units into account. For example, if you
-- aggregate @usageQuantity@ across all of Amazon EC2, the results aren\'t
-- meaningful because Amazon EC2 compute hours and data transfer are
-- measured in different units (for example, hour or GB). To get more
-- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
-- @UsageTypeGroups@.
--
-- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
--
-- 'timePeriod', 'getCostAndUsageWithResources_timePeriod' - Sets the start and end dates for retrieving Amazon Web Services costs.
-- The range must be within the last 14 days (the start date cannot be
-- earlier than 14 days ago). The start date is inclusive, but the end date
-- is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
-- @2017-05-01@, then the cost and usage data is retrieved from
-- @2017-01-01@ up to and including @2017-04-30@ but not including
-- @2017-05-01@.
--
-- 'granularity', 'getCostAndUsageWithResources_granularity' - Sets the Amazon Web Services cost granularity to @MONTHLY@, @DAILY@, or
-- @HOURLY@. If @Granularity@ isn\'t set, the response object doesn\'t
-- include the @Granularity@, @MONTHLY@, @DAILY@, or @HOURLY@.
--
-- 'filter'', 'getCostAndUsageWithResources_filter' - Filters Amazon Web Services costs by different dimensions. For example,
-- you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that
-- are associated with that account\'s usage of that service. You can nest
-- @Expression@ objects to define any combination of dimension filters. For
-- more information, see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
--
-- The @GetCostAndUsageWithResources@ operation requires that you either
-- group by or filter by a @ResourceId@. It requires the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- @\"SERVICE = Amazon Elastic Compute Cloud - Compute\"@ in the filter.
--
-- Valid values for @MatchOptions@ for @Dimensions@ are @EQUALS@ and
-- @CASE_SENSITIVE@.
--
-- Valid values for @MatchOptions@ for @CostCategories@ and @Tags@ are
-- @EQUALS@, @ABSENT@, and @CASE_SENSITIVE@. Default values are @EQUALS@
-- and @CASE_SENSITIVE@.
newGetCostAndUsageWithResources ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'granularity'
  Granularity ->
  -- | 'filter''
  Expression ->
  GetCostAndUsageWithResources
newGetCostAndUsageWithResources
  pTimePeriod_
  pGranularity_
  pFilter_ =
    GetCostAndUsageWithResources'
      { nextPageToken =
          Prelude.Nothing,
        groupBy = Prelude.Nothing,
        metrics = Prelude.Nothing,
        timePeriod = pTimePeriod_,
        granularity = pGranularity_,
        filter' = pFilter_
      }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getCostAndUsageWithResources_nextPageToken :: Lens.Lens' GetCostAndUsageWithResources (Prelude.Maybe Prelude.Text)
getCostAndUsageWithResources_nextPageToken = Lens.lens (\GetCostAndUsageWithResources' {nextPageToken} -> nextPageToken) (\s@GetCostAndUsageWithResources' {} a -> s {nextPageToken = a} :: GetCostAndUsageWithResources)

-- | You can group Amazon Web Services costs using up to two different
-- groups: @DIMENSION@, @TAG@, @COST_CATEGORY@.
getCostAndUsageWithResources_groupBy :: Lens.Lens' GetCostAndUsageWithResources (Prelude.Maybe [GroupDefinition])
getCostAndUsageWithResources_groupBy = Lens.lens (\GetCostAndUsageWithResources' {groupBy} -> groupBy) (\s@GetCostAndUsageWithResources' {} a -> s {groupBy = a} :: GetCostAndUsageWithResources) Prelude.. Lens.mapping Lens.coerced

-- | Which metrics are returned in the query. For more information about
-- blended and unblended rates, see
-- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
--
-- Valid values are @AmortizedCost@, @BlendedCost@, @NetAmortizedCost@,
-- @NetUnblendedCost@, @NormalizedUsageAmount@, @UnblendedCost@, and
-- @UsageQuantity@.
--
-- If you return the @UsageQuantity@ metric, the service aggregates all
-- usage numbers without taking the units into account. For example, if you
-- aggregate @usageQuantity@ across all of Amazon EC2, the results aren\'t
-- meaningful because Amazon EC2 compute hours and data transfer are
-- measured in different units (for example, hour or GB). To get more
-- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
-- @UsageTypeGroups@.
--
-- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
getCostAndUsageWithResources_metrics :: Lens.Lens' GetCostAndUsageWithResources (Prelude.Maybe [Prelude.Text])
getCostAndUsageWithResources_metrics = Lens.lens (\GetCostAndUsageWithResources' {metrics} -> metrics) (\s@GetCostAndUsageWithResources' {} a -> s {metrics = a} :: GetCostAndUsageWithResources) Prelude.. Lens.mapping Lens.coerced

-- | Sets the start and end dates for retrieving Amazon Web Services costs.
-- The range must be within the last 14 days (the start date cannot be
-- earlier than 14 days ago). The start date is inclusive, but the end date
-- is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
-- @2017-05-01@, then the cost and usage data is retrieved from
-- @2017-01-01@ up to and including @2017-04-30@ but not including
-- @2017-05-01@.
getCostAndUsageWithResources_timePeriod :: Lens.Lens' GetCostAndUsageWithResources DateInterval
getCostAndUsageWithResources_timePeriod = Lens.lens (\GetCostAndUsageWithResources' {timePeriod} -> timePeriod) (\s@GetCostAndUsageWithResources' {} a -> s {timePeriod = a} :: GetCostAndUsageWithResources)

-- | Sets the Amazon Web Services cost granularity to @MONTHLY@, @DAILY@, or
-- @HOURLY@. If @Granularity@ isn\'t set, the response object doesn\'t
-- include the @Granularity@, @MONTHLY@, @DAILY@, or @HOURLY@.
getCostAndUsageWithResources_granularity :: Lens.Lens' GetCostAndUsageWithResources Granularity
getCostAndUsageWithResources_granularity = Lens.lens (\GetCostAndUsageWithResources' {granularity} -> granularity) (\s@GetCostAndUsageWithResources' {} a -> s {granularity = a} :: GetCostAndUsageWithResources)

-- | Filters Amazon Web Services costs by different dimensions. For example,
-- you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that
-- are associated with that account\'s usage of that service. You can nest
-- @Expression@ objects to define any combination of dimension filters. For
-- more information, see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
--
-- The @GetCostAndUsageWithResources@ operation requires that you either
-- group by or filter by a @ResourceId@. It requires the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- @\"SERVICE = Amazon Elastic Compute Cloud - Compute\"@ in the filter.
--
-- Valid values for @MatchOptions@ for @Dimensions@ are @EQUALS@ and
-- @CASE_SENSITIVE@.
--
-- Valid values for @MatchOptions@ for @CostCategories@ and @Tags@ are
-- @EQUALS@, @ABSENT@, and @CASE_SENSITIVE@. Default values are @EQUALS@
-- and @CASE_SENSITIVE@.
getCostAndUsageWithResources_filter :: Lens.Lens' GetCostAndUsageWithResources Expression
getCostAndUsageWithResources_filter = Lens.lens (\GetCostAndUsageWithResources' {filter'} -> filter') (\s@GetCostAndUsageWithResources' {} a -> s {filter' = a} :: GetCostAndUsageWithResources)

instance Core.AWSRequest GetCostAndUsageWithResources where
  type
    AWSResponse GetCostAndUsageWithResources =
      GetCostAndUsageWithResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostAndUsageWithResourcesResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> ( x Data..?> "DimensionValueAttributes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "GroupDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ResultsByTime" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCostAndUsageWithResources
  where
  hashWithSalt _salt GetCostAndUsageWithResources' {..} =
    _salt `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` groupBy
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` granularity
      `Prelude.hashWithSalt` filter'

instance Prelude.NFData GetCostAndUsageWithResources where
  rnf GetCostAndUsageWithResources' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf groupBy
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf granularity
      `Prelude.seq` Prelude.rnf filter'

instance Data.ToHeaders GetCostAndUsageWithResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetCostAndUsageWithResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCostAndUsageWithResources where
  toJSON GetCostAndUsageWithResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextPageToken" Data..=) Prelude.<$> nextPageToken,
            ("GroupBy" Data..=) Prelude.<$> groupBy,
            ("Metrics" Data..=) Prelude.<$> metrics,
            Prelude.Just ("TimePeriod" Data..= timePeriod),
            Prelude.Just ("Granularity" Data..= granularity),
            Prelude.Just ("Filter" Data..= filter')
          ]
      )

instance Data.ToPath GetCostAndUsageWithResources where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCostAndUsageWithResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCostAndUsageWithResourcesResponse' smart constructor.
data GetCostAndUsageWithResourcesResponse = GetCostAndUsageWithResourcesResponse'
  { -- | The token for the next set of retrievable results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The attributes that apply to a specific dimension value. For example, if
    -- the value is a linked account, the attribute is that account name.
    dimensionValueAttributes :: Prelude.Maybe [DimensionValuesWithAttributes],
    -- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in
    -- the request.
    groupDefinitions :: Prelude.Maybe [GroupDefinition],
    -- | The time period that\'s covered by the results in the response.
    resultsByTime :: Prelude.Maybe [ResultByTime],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostAndUsageWithResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getCostAndUsageWithResourcesResponse_nextPageToken' - The token for the next set of retrievable results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'dimensionValueAttributes', 'getCostAndUsageWithResourcesResponse_dimensionValueAttributes' - The attributes that apply to a specific dimension value. For example, if
-- the value is a linked account, the attribute is that account name.
--
-- 'groupDefinitions', 'getCostAndUsageWithResourcesResponse_groupDefinitions' - The groups that are specified by the @Filter@ or @GroupBy@ parameters in
-- the request.
--
-- 'resultsByTime', 'getCostAndUsageWithResourcesResponse_resultsByTime' - The time period that\'s covered by the results in the response.
--
-- 'httpStatus', 'getCostAndUsageWithResourcesResponse_httpStatus' - The response's http status code.
newGetCostAndUsageWithResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCostAndUsageWithResourcesResponse
newGetCostAndUsageWithResourcesResponse pHttpStatus_ =
  GetCostAndUsageWithResourcesResponse'
    { nextPageToken =
        Prelude.Nothing,
      dimensionValueAttributes =
        Prelude.Nothing,
      groupDefinitions = Prelude.Nothing,
      resultsByTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of retrievable results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getCostAndUsageWithResourcesResponse_nextPageToken :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Prelude.Maybe Prelude.Text)
getCostAndUsageWithResourcesResponse_nextPageToken = Lens.lens (\GetCostAndUsageWithResourcesResponse' {nextPageToken} -> nextPageToken) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {nextPageToken = a} :: GetCostAndUsageWithResourcesResponse)

-- | The attributes that apply to a specific dimension value. For example, if
-- the value is a linked account, the attribute is that account name.
getCostAndUsageWithResourcesResponse_dimensionValueAttributes :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Prelude.Maybe [DimensionValuesWithAttributes])
getCostAndUsageWithResourcesResponse_dimensionValueAttributes = Lens.lens (\GetCostAndUsageWithResourcesResponse' {dimensionValueAttributes} -> dimensionValueAttributes) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {dimensionValueAttributes = a} :: GetCostAndUsageWithResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in
-- the request.
getCostAndUsageWithResourcesResponse_groupDefinitions :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Prelude.Maybe [GroupDefinition])
getCostAndUsageWithResourcesResponse_groupDefinitions = Lens.lens (\GetCostAndUsageWithResourcesResponse' {groupDefinitions} -> groupDefinitions) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {groupDefinitions = a} :: GetCostAndUsageWithResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time period that\'s covered by the results in the response.
getCostAndUsageWithResourcesResponse_resultsByTime :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Prelude.Maybe [ResultByTime])
getCostAndUsageWithResourcesResponse_resultsByTime = Lens.lens (\GetCostAndUsageWithResourcesResponse' {resultsByTime} -> resultsByTime) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {resultsByTime = a} :: GetCostAndUsageWithResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCostAndUsageWithResourcesResponse_httpStatus :: Lens.Lens' GetCostAndUsageWithResourcesResponse Prelude.Int
getCostAndUsageWithResourcesResponse_httpStatus = Lens.lens (\GetCostAndUsageWithResourcesResponse' {httpStatus} -> httpStatus) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {httpStatus = a} :: GetCostAndUsageWithResourcesResponse)

instance
  Prelude.NFData
    GetCostAndUsageWithResourcesResponse
  where
  rnf GetCostAndUsageWithResourcesResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf dimensionValueAttributes
      `Prelude.seq` Prelude.rnf groupDefinitions
      `Prelude.seq` Prelude.rnf resultsByTime
      `Prelude.seq` Prelude.rnf httpStatus
