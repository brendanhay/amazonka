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
-- Module      : Network.AWS.CostExplorer.GetCostAndUsageWithResources
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- operation. Management account in an organization in AWS Organizations
-- have access to all member accounts. This API is currently available for
-- the Amazon Elastic Compute Cloud – Compute service only.
--
-- This is an opt-in only feature. You can enable this feature from the
-- Cost Explorer Settings page. For information on how to access the
-- Settings page, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/ce-access.html Controlling Access for Cost Explorer>
-- in the /AWS Billing and Cost Management User Guide/.
module Network.AWS.CostExplorer.GetCostAndUsageWithResources
  ( -- * Creating a Request
    GetCostAndUsageWithResources (..),
    newGetCostAndUsageWithResources,

    -- * Request Lenses
    getCostAndUsageWithResources_granularity,
    getCostAndUsageWithResources_nextPageToken,
    getCostAndUsageWithResources_metrics,
    getCostAndUsageWithResources_groupBy,
    getCostAndUsageWithResources_timePeriod,
    getCostAndUsageWithResources_filter,

    -- * Destructuring the Response
    GetCostAndUsageWithResourcesResponse (..),
    newGetCostAndUsageWithResourcesResponse,

    -- * Response Lenses
    getCostAndUsageWithResourcesResponse_nextPageToken,
    getCostAndUsageWithResourcesResponse_resultsByTime,
    getCostAndUsageWithResourcesResponse_dimensionValueAttributes,
    getCostAndUsageWithResourcesResponse_groupDefinitions,
    getCostAndUsageWithResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCostAndUsageWithResources' smart constructor.
data GetCostAndUsageWithResources = GetCostAndUsageWithResources'
  { -- | Sets the AWS cost granularity to @MONTHLY@, @DAILY@, or @HOURLY@. If
    -- @Granularity@ isn\'t set, the response object doesn\'t include the
    -- @Granularity@, @MONTHLY@, @DAILY@, or @HOURLY@.
    granularity :: Core.Maybe Granularity,
    -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Core.Maybe Core.Text,
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
    -- measured in different units (for example, hours vs. GB). To get more
    -- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
    -- @UsageTypeGroups@.
    --
    -- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
    metrics :: Core.Maybe [Core.Text],
    -- | You can group Amazon Web Services costs using up to two different
    -- groups: @DIMENSION@, @TAG@, @COST_CATEGORY@.
    groupBy :: Core.Maybe [GroupDefinition],
    -- | Sets the start and end dates for retrieving Amazon Web Services costs.
    -- The range must be within the last 14 days (the start date cannot be
    -- earlier than 14 days ago). The start date is inclusive, but the end date
    -- is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
    -- @2017-05-01@, then the cost and usage data is retrieved from
    -- @2017-01-01@ up to and including @2017-04-30@ but not including
    -- @2017-05-01@.
    timePeriod :: DateInterval,
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
    filter' :: Expression
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCostAndUsageWithResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'granularity', 'getCostAndUsageWithResources_granularity' - Sets the AWS cost granularity to @MONTHLY@, @DAILY@, or @HOURLY@. If
-- @Granularity@ isn\'t set, the response object doesn\'t include the
-- @Granularity@, @MONTHLY@, @DAILY@, or @HOURLY@.
--
-- 'nextPageToken', 'getCostAndUsageWithResources_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
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
-- measured in different units (for example, hours vs. GB). To get more
-- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
-- @UsageTypeGroups@.
--
-- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
--
-- 'groupBy', 'getCostAndUsageWithResources_groupBy' - You can group Amazon Web Services costs using up to two different
-- groups: @DIMENSION@, @TAG@, @COST_CATEGORY@.
--
-- 'timePeriod', 'getCostAndUsageWithResources_timePeriod' - Sets the start and end dates for retrieving Amazon Web Services costs.
-- The range must be within the last 14 days (the start date cannot be
-- earlier than 14 days ago). The start date is inclusive, but the end date
-- is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
-- @2017-05-01@, then the cost and usage data is retrieved from
-- @2017-01-01@ up to and including @2017-04-30@ but not including
-- @2017-05-01@.
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
newGetCostAndUsageWithResources ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'filter''
  Expression ->
  GetCostAndUsageWithResources
newGetCostAndUsageWithResources pTimePeriod_ pFilter_ =
  GetCostAndUsageWithResources'
    { granularity =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      metrics = Core.Nothing,
      groupBy = Core.Nothing,
      timePeriod = pTimePeriod_,
      filter' = pFilter_
    }

-- | Sets the AWS cost granularity to @MONTHLY@, @DAILY@, or @HOURLY@. If
-- @Granularity@ isn\'t set, the response object doesn\'t include the
-- @Granularity@, @MONTHLY@, @DAILY@, or @HOURLY@.
getCostAndUsageWithResources_granularity :: Lens.Lens' GetCostAndUsageWithResources (Core.Maybe Granularity)
getCostAndUsageWithResources_granularity = Lens.lens (\GetCostAndUsageWithResources' {granularity} -> granularity) (\s@GetCostAndUsageWithResources' {} a -> s {granularity = a} :: GetCostAndUsageWithResources)

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getCostAndUsageWithResources_nextPageToken :: Lens.Lens' GetCostAndUsageWithResources (Core.Maybe Core.Text)
getCostAndUsageWithResources_nextPageToken = Lens.lens (\GetCostAndUsageWithResources' {nextPageToken} -> nextPageToken) (\s@GetCostAndUsageWithResources' {} a -> s {nextPageToken = a} :: GetCostAndUsageWithResources)

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
-- measured in different units (for example, hours vs. GB). To get more
-- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
-- @UsageTypeGroups@.
--
-- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
getCostAndUsageWithResources_metrics :: Lens.Lens' GetCostAndUsageWithResources (Core.Maybe [Core.Text])
getCostAndUsageWithResources_metrics = Lens.lens (\GetCostAndUsageWithResources' {metrics} -> metrics) (\s@GetCostAndUsageWithResources' {} a -> s {metrics = a} :: GetCostAndUsageWithResources) Core.. Lens.mapping Lens._Coerce

-- | You can group Amazon Web Services costs using up to two different
-- groups: @DIMENSION@, @TAG@, @COST_CATEGORY@.
getCostAndUsageWithResources_groupBy :: Lens.Lens' GetCostAndUsageWithResources (Core.Maybe [GroupDefinition])
getCostAndUsageWithResources_groupBy = Lens.lens (\GetCostAndUsageWithResources' {groupBy} -> groupBy) (\s@GetCostAndUsageWithResources' {} a -> s {groupBy = a} :: GetCostAndUsageWithResources) Core.. Lens.mapping Lens._Coerce

-- | Sets the start and end dates for retrieving Amazon Web Services costs.
-- The range must be within the last 14 days (the start date cannot be
-- earlier than 14 days ago). The start date is inclusive, but the end date
-- is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is
-- @2017-05-01@, then the cost and usage data is retrieved from
-- @2017-01-01@ up to and including @2017-04-30@ but not including
-- @2017-05-01@.
getCostAndUsageWithResources_timePeriod :: Lens.Lens' GetCostAndUsageWithResources DateInterval
getCostAndUsageWithResources_timePeriod = Lens.lens (\GetCostAndUsageWithResources' {timePeriod} -> timePeriod) (\s@GetCostAndUsageWithResources' {} a -> s {timePeriod = a} :: GetCostAndUsageWithResources)

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
getCostAndUsageWithResources_filter :: Lens.Lens' GetCostAndUsageWithResources Expression
getCostAndUsageWithResources_filter = Lens.lens (\GetCostAndUsageWithResources' {filter'} -> filter') (\s@GetCostAndUsageWithResources' {} a -> s {filter' = a} :: GetCostAndUsageWithResources)

instance Core.AWSRequest GetCostAndUsageWithResources where
  type
    AWSResponse GetCostAndUsageWithResources =
      GetCostAndUsageWithResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostAndUsageWithResourcesResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> (x Core..?> "ResultsByTime" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "DimensionValueAttributes"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "GroupDefinitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCostAndUsageWithResources

instance Core.NFData GetCostAndUsageWithResources

instance Core.ToHeaders GetCostAndUsageWithResources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.GetCostAndUsageWithResources" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCostAndUsageWithResources where
  toJSON GetCostAndUsageWithResources' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Granularity" Core..=) Core.<$> granularity,
            ("NextPageToken" Core..=) Core.<$> nextPageToken,
            ("Metrics" Core..=) Core.<$> metrics,
            ("GroupBy" Core..=) Core.<$> groupBy,
            Core.Just ("TimePeriod" Core..= timePeriod),
            Core.Just ("Filter" Core..= filter')
          ]
      )

instance Core.ToPath GetCostAndUsageWithResources where
  toPath = Core.const "/"

instance Core.ToQuery GetCostAndUsageWithResources where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCostAndUsageWithResourcesResponse' smart constructor.
data GetCostAndUsageWithResourcesResponse = GetCostAndUsageWithResourcesResponse'
  { -- | The token for the next set of retrievable results. AWS provides the
    -- token when the response from a previous call has more results than the
    -- maximum page size.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The time period that is covered by the results in the response.
    resultsByTime :: Core.Maybe [ResultByTime],
    -- | The attributes that apply to a specific dimension value. For example, if
    -- the value is a linked account, the attribute is that account name.
    dimensionValueAttributes :: Core.Maybe [DimensionValuesWithAttributes],
    -- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in
    -- the request.
    groupDefinitions :: Core.Maybe [GroupDefinition],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCostAndUsageWithResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getCostAndUsageWithResourcesResponse_nextPageToken' - The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
--
-- 'resultsByTime', 'getCostAndUsageWithResourcesResponse_resultsByTime' - The time period that is covered by the results in the response.
--
-- 'dimensionValueAttributes', 'getCostAndUsageWithResourcesResponse_dimensionValueAttributes' - The attributes that apply to a specific dimension value. For example, if
-- the value is a linked account, the attribute is that account name.
--
-- 'groupDefinitions', 'getCostAndUsageWithResourcesResponse_groupDefinitions' - The groups that are specified by the @Filter@ or @GroupBy@ parameters in
-- the request.
--
-- 'httpStatus', 'getCostAndUsageWithResourcesResponse_httpStatus' - The response's http status code.
newGetCostAndUsageWithResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCostAndUsageWithResourcesResponse
newGetCostAndUsageWithResourcesResponse pHttpStatus_ =
  GetCostAndUsageWithResourcesResponse'
    { nextPageToken =
        Core.Nothing,
      resultsByTime = Core.Nothing,
      dimensionValueAttributes =
        Core.Nothing,
      groupDefinitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
getCostAndUsageWithResourcesResponse_nextPageToken :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Core.Maybe Core.Text)
getCostAndUsageWithResourcesResponse_nextPageToken = Lens.lens (\GetCostAndUsageWithResourcesResponse' {nextPageToken} -> nextPageToken) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {nextPageToken = a} :: GetCostAndUsageWithResourcesResponse)

-- | The time period that is covered by the results in the response.
getCostAndUsageWithResourcesResponse_resultsByTime :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Core.Maybe [ResultByTime])
getCostAndUsageWithResourcesResponse_resultsByTime = Lens.lens (\GetCostAndUsageWithResourcesResponse' {resultsByTime} -> resultsByTime) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {resultsByTime = a} :: GetCostAndUsageWithResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The attributes that apply to a specific dimension value. For example, if
-- the value is a linked account, the attribute is that account name.
getCostAndUsageWithResourcesResponse_dimensionValueAttributes :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Core.Maybe [DimensionValuesWithAttributes])
getCostAndUsageWithResourcesResponse_dimensionValueAttributes = Lens.lens (\GetCostAndUsageWithResourcesResponse' {dimensionValueAttributes} -> dimensionValueAttributes) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {dimensionValueAttributes = a} :: GetCostAndUsageWithResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in
-- the request.
getCostAndUsageWithResourcesResponse_groupDefinitions :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Core.Maybe [GroupDefinition])
getCostAndUsageWithResourcesResponse_groupDefinitions = Lens.lens (\GetCostAndUsageWithResourcesResponse' {groupDefinitions} -> groupDefinitions) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {groupDefinitions = a} :: GetCostAndUsageWithResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCostAndUsageWithResourcesResponse_httpStatus :: Lens.Lens' GetCostAndUsageWithResourcesResponse Core.Int
getCostAndUsageWithResourcesResponse_httpStatus = Lens.lens (\GetCostAndUsageWithResourcesResponse' {httpStatus} -> httpStatus) (\s@GetCostAndUsageWithResourcesResponse' {} a -> s {httpStatus = a} :: GetCostAndUsageWithResourcesResponse)

instance
  Core.NFData
    GetCostAndUsageWithResourcesResponse
