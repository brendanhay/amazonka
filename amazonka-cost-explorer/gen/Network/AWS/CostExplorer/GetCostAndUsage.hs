{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CostExplorer.GetCostAndUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves cost and usage metrics for your account. You can specify which
-- cost and usage-related metric, such as @BlendedCosts@ or
-- @UsageQuantity@, that you want the request to return. You can also
-- filter and group your data by various dimensions, such as @SERVICE@ or
-- @AZ@, in a specific time range. For a complete list of valid dimensions,
-- see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html GetDimensionValues>
-- operation. Management account in an organization in AWS Organizations
-- have access to all member accounts.
--
-- For information about filter limitations, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-limits.html Quotas and restrictions>
-- in the /Billing and Cost Management User Guide/.
module Network.AWS.CostExplorer.GetCostAndUsage
  ( -- * Creating a Request
    GetCostAndUsage (..),
    newGetCostAndUsage,

    -- * Request Lenses
    getCostAndUsage_granularity,
    getCostAndUsage_nextPageToken,
    getCostAndUsage_groupBy,
    getCostAndUsage_filter,
    getCostAndUsage_timePeriod,
    getCostAndUsage_metrics,

    -- * Destructuring the Response
    GetCostAndUsageResponse (..),
    newGetCostAndUsageResponse,

    -- * Response Lenses
    getCostAndUsageResponse_nextPageToken,
    getCostAndUsageResponse_resultsByTime,
    getCostAndUsageResponse_dimensionValueAttributes,
    getCostAndUsageResponse_groupDefinitions,
    getCostAndUsageResponse_httpStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCostAndUsage' smart constructor.
data GetCostAndUsage = GetCostAndUsage'
  { -- | Sets the AWS cost granularity to @MONTHLY@ or @DAILY@, or @HOURLY@. If
    -- @Granularity@ isn\'t set, the response object doesn\'t include the
    -- @Granularity@, either @MONTHLY@ or @DAILY@, or @HOURLY@.
    granularity :: Prelude.Maybe Granularity,
    -- | The token to retrieve the next set of results. AWS provides the token
    -- when the response from a previous call has more results than the maximum
    -- page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | You can group AWS costs using up to two different groups, either
    -- dimensions, tag keys, cost categories, or any two group by types.
    --
    -- When you group by tag key, you get all tag values, including empty
    -- strings.
    --
    -- Valid values are @AZ@, @INSTANCE_TYPE@, @LEGAL_ENTITY_NAME@,
    -- @LINKED_ACCOUNT@, @OPERATION@, @PLATFORM@, @PURCHASE_TYPE@, @SERVICE@,
    -- @TAGS@, @TENANCY@, @RECORD_TYPE@, and @USAGE_TYPE@.
    groupBy :: Prelude.Maybe [GroupDefinition],
    -- | Filters AWS costs by different dimensions. For example, you can specify
    -- @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated
    -- with that account\'s usage of that service. You can nest @Expression@
    -- objects to define any combination of dimension filters. For more
    -- information, see
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
    filter' :: Prelude.Maybe Expression,
    -- | Sets the start and end dates for retrieving AWS costs. The start date is
    -- inclusive, but the end date is exclusive. For example, if @start@ is
    -- @2017-01-01@ and @end@ is @2017-05-01@, then the cost and usage data is
    -- retrieved from @2017-01-01@ up to and including @2017-04-30@ but not
    -- including @2017-05-01@.
    timePeriod :: DateInterval,
    -- | Which metrics are returned in the query. For more information about
    -- blended and unblended rates, see
    -- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
    --
    -- Valid values are @AmortizedCost@, @BlendedCost@, @NetAmortizedCost@,
    -- @NetUnblendedCost@, @NormalizedUsageAmount@, @UnblendedCost@, and
    -- @UsageQuantity@.
    --
    -- If you return the @UsageQuantity@ metric, the service aggregates all
    -- usage numbers without taking into account the units. For example, if you
    -- aggregate @usageQuantity@ across all of Amazon EC2, the results aren\'t
    -- meaningful because Amazon EC2 compute hours and data transfer are
    -- measured in different units (for example, hours vs. GB). To get more
    -- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
    -- @UsageTypeGroups@.
    --
    -- @Metrics@ is required for @GetCostAndUsage@ requests.
    metrics :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCostAndUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'granularity', 'getCostAndUsage_granularity' - Sets the AWS cost granularity to @MONTHLY@ or @DAILY@, or @HOURLY@. If
-- @Granularity@ isn\'t set, the response object doesn\'t include the
-- @Granularity@, either @MONTHLY@ or @DAILY@, or @HOURLY@.
--
-- 'nextPageToken', 'getCostAndUsage_nextPageToken' - The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
--
-- 'groupBy', 'getCostAndUsage_groupBy' - You can group AWS costs using up to two different groups, either
-- dimensions, tag keys, cost categories, or any two group by types.
--
-- When you group by tag key, you get all tag values, including empty
-- strings.
--
-- Valid values are @AZ@, @INSTANCE_TYPE@, @LEGAL_ENTITY_NAME@,
-- @LINKED_ACCOUNT@, @OPERATION@, @PLATFORM@, @PURCHASE_TYPE@, @SERVICE@,
-- @TAGS@, @TENANCY@, @RECORD_TYPE@, and @USAGE_TYPE@.
--
-- 'filter'', 'getCostAndUsage_filter' - Filters AWS costs by different dimensions. For example, you can specify
-- @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated
-- with that account\'s usage of that service. You can nest @Expression@
-- objects to define any combination of dimension filters. For more
-- information, see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
--
-- 'timePeriod', 'getCostAndUsage_timePeriod' - Sets the start and end dates for retrieving AWS costs. The start date is
-- inclusive, but the end date is exclusive. For example, if @start@ is
-- @2017-01-01@ and @end@ is @2017-05-01@, then the cost and usage data is
-- retrieved from @2017-01-01@ up to and including @2017-04-30@ but not
-- including @2017-05-01@.
--
-- 'metrics', 'getCostAndUsage_metrics' - Which metrics are returned in the query. For more information about
-- blended and unblended rates, see
-- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
--
-- Valid values are @AmortizedCost@, @BlendedCost@, @NetAmortizedCost@,
-- @NetUnblendedCost@, @NormalizedUsageAmount@, @UnblendedCost@, and
-- @UsageQuantity@.
--
-- If you return the @UsageQuantity@ metric, the service aggregates all
-- usage numbers without taking into account the units. For example, if you
-- aggregate @usageQuantity@ across all of Amazon EC2, the results aren\'t
-- meaningful because Amazon EC2 compute hours and data transfer are
-- measured in different units (for example, hours vs. GB). To get more
-- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
-- @UsageTypeGroups@.
--
-- @Metrics@ is required for @GetCostAndUsage@ requests.
newGetCostAndUsage ::
  -- | 'timePeriod'
  DateInterval ->
  GetCostAndUsage
newGetCostAndUsage pTimePeriod_ =
  GetCostAndUsage'
    { granularity = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      groupBy = Prelude.Nothing,
      filter' = Prelude.Nothing,
      timePeriod = pTimePeriod_,
      metrics = Prelude.mempty
    }

-- | Sets the AWS cost granularity to @MONTHLY@ or @DAILY@, or @HOURLY@. If
-- @Granularity@ isn\'t set, the response object doesn\'t include the
-- @Granularity@, either @MONTHLY@ or @DAILY@, or @HOURLY@.
getCostAndUsage_granularity :: Lens.Lens' GetCostAndUsage (Prelude.Maybe Granularity)
getCostAndUsage_granularity = Lens.lens (\GetCostAndUsage' {granularity} -> granularity) (\s@GetCostAndUsage' {} a -> s {granularity = a} :: GetCostAndUsage)

-- | The token to retrieve the next set of results. AWS provides the token
-- when the response from a previous call has more results than the maximum
-- page size.
getCostAndUsage_nextPageToken :: Lens.Lens' GetCostAndUsage (Prelude.Maybe Prelude.Text)
getCostAndUsage_nextPageToken = Lens.lens (\GetCostAndUsage' {nextPageToken} -> nextPageToken) (\s@GetCostAndUsage' {} a -> s {nextPageToken = a} :: GetCostAndUsage)

-- | You can group AWS costs using up to two different groups, either
-- dimensions, tag keys, cost categories, or any two group by types.
--
-- When you group by tag key, you get all tag values, including empty
-- strings.
--
-- Valid values are @AZ@, @INSTANCE_TYPE@, @LEGAL_ENTITY_NAME@,
-- @LINKED_ACCOUNT@, @OPERATION@, @PLATFORM@, @PURCHASE_TYPE@, @SERVICE@,
-- @TAGS@, @TENANCY@, @RECORD_TYPE@, and @USAGE_TYPE@.
getCostAndUsage_groupBy :: Lens.Lens' GetCostAndUsage (Prelude.Maybe [GroupDefinition])
getCostAndUsage_groupBy = Lens.lens (\GetCostAndUsage' {groupBy} -> groupBy) (\s@GetCostAndUsage' {} a -> s {groupBy = a} :: GetCostAndUsage) Prelude.. Lens.mapping Prelude._Coerce

-- | Filters AWS costs by different dimensions. For example, you can specify
-- @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated
-- with that account\'s usage of that service. You can nest @Expression@
-- objects to define any combination of dimension filters. For more
-- information, see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
getCostAndUsage_filter :: Lens.Lens' GetCostAndUsage (Prelude.Maybe Expression)
getCostAndUsage_filter = Lens.lens (\GetCostAndUsage' {filter'} -> filter') (\s@GetCostAndUsage' {} a -> s {filter' = a} :: GetCostAndUsage)

-- | Sets the start and end dates for retrieving AWS costs. The start date is
-- inclusive, but the end date is exclusive. For example, if @start@ is
-- @2017-01-01@ and @end@ is @2017-05-01@, then the cost and usage data is
-- retrieved from @2017-01-01@ up to and including @2017-04-30@ but not
-- including @2017-05-01@.
getCostAndUsage_timePeriod :: Lens.Lens' GetCostAndUsage DateInterval
getCostAndUsage_timePeriod = Lens.lens (\GetCostAndUsage' {timePeriod} -> timePeriod) (\s@GetCostAndUsage' {} a -> s {timePeriod = a} :: GetCostAndUsage)

-- | Which metrics are returned in the query. For more information about
-- blended and unblended rates, see
-- <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the \"blended\" annotation appear on some line items in my bill?>.
--
-- Valid values are @AmortizedCost@, @BlendedCost@, @NetAmortizedCost@,
-- @NetUnblendedCost@, @NormalizedUsageAmount@, @UnblendedCost@, and
-- @UsageQuantity@.
--
-- If you return the @UsageQuantity@ metric, the service aggregates all
-- usage numbers without taking into account the units. For example, if you
-- aggregate @usageQuantity@ across all of Amazon EC2, the results aren\'t
-- meaningful because Amazon EC2 compute hours and data transfer are
-- measured in different units (for example, hours vs. GB). To get more
-- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
-- @UsageTypeGroups@.
--
-- @Metrics@ is required for @GetCostAndUsage@ requests.
getCostAndUsage_metrics :: Lens.Lens' GetCostAndUsage [Prelude.Text]
getCostAndUsage_metrics = Lens.lens (\GetCostAndUsage' {metrics} -> metrics) (\s@GetCostAndUsage' {} a -> s {metrics = a} :: GetCostAndUsage) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest GetCostAndUsage where
  type Rs GetCostAndUsage = GetCostAndUsageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostAndUsageResponse'
            Prelude.<$> (x Prelude..?> "NextPageToken")
            Prelude.<*> ( x Prelude..?> "ResultsByTime"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "DimensionValueAttributes"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "GroupDefinitions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCostAndUsage

instance Prelude.NFData GetCostAndUsage

instance Prelude.ToHeaders GetCostAndUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.GetCostAndUsage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetCostAndUsage where
  toJSON GetCostAndUsage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Granularity" Prelude..=) Prelude.<$> granularity,
            ("NextPageToken" Prelude..=)
              Prelude.<$> nextPageToken,
            ("GroupBy" Prelude..=) Prelude.<$> groupBy,
            ("Filter" Prelude..=) Prelude.<$> filter',
            Prelude.Just ("TimePeriod" Prelude..= timePeriod),
            Prelude.Just ("Metrics" Prelude..= metrics)
          ]
      )

instance Prelude.ToPath GetCostAndUsage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCostAndUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCostAndUsageResponse' smart constructor.
data GetCostAndUsageResponse = GetCostAndUsageResponse'
  { -- | The token for the next set of retrievable results. AWS provides the
    -- token when the response from a previous call has more results than the
    -- maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The time period that is covered by the results in the response.
    resultsByTime :: Prelude.Maybe [ResultByTime],
    -- | The attributes that apply to a specific dimension value. For example, if
    -- the value is a linked account, the attribute is that account name.
    dimensionValueAttributes :: Prelude.Maybe [DimensionValuesWithAttributes],
    -- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in
    -- the request.
    groupDefinitions :: Prelude.Maybe [GroupDefinition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCostAndUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getCostAndUsageResponse_nextPageToken' - The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
--
-- 'resultsByTime', 'getCostAndUsageResponse_resultsByTime' - The time period that is covered by the results in the response.
--
-- 'dimensionValueAttributes', 'getCostAndUsageResponse_dimensionValueAttributes' - The attributes that apply to a specific dimension value. For example, if
-- the value is a linked account, the attribute is that account name.
--
-- 'groupDefinitions', 'getCostAndUsageResponse_groupDefinitions' - The groups that are specified by the @Filter@ or @GroupBy@ parameters in
-- the request.
--
-- 'httpStatus', 'getCostAndUsageResponse_httpStatus' - The response's http status code.
newGetCostAndUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCostAndUsageResponse
newGetCostAndUsageResponse pHttpStatus_ =
  GetCostAndUsageResponse'
    { nextPageToken =
        Prelude.Nothing,
      resultsByTime = Prelude.Nothing,
      dimensionValueAttributes = Prelude.Nothing,
      groupDefinitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of retrievable results. AWS provides the
-- token when the response from a previous call has more results than the
-- maximum page size.
getCostAndUsageResponse_nextPageToken :: Lens.Lens' GetCostAndUsageResponse (Prelude.Maybe Prelude.Text)
getCostAndUsageResponse_nextPageToken = Lens.lens (\GetCostAndUsageResponse' {nextPageToken} -> nextPageToken) (\s@GetCostAndUsageResponse' {} a -> s {nextPageToken = a} :: GetCostAndUsageResponse)

-- | The time period that is covered by the results in the response.
getCostAndUsageResponse_resultsByTime :: Lens.Lens' GetCostAndUsageResponse (Prelude.Maybe [ResultByTime])
getCostAndUsageResponse_resultsByTime = Lens.lens (\GetCostAndUsageResponse' {resultsByTime} -> resultsByTime) (\s@GetCostAndUsageResponse' {} a -> s {resultsByTime = a} :: GetCostAndUsageResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The attributes that apply to a specific dimension value. For example, if
-- the value is a linked account, the attribute is that account name.
getCostAndUsageResponse_dimensionValueAttributes :: Lens.Lens' GetCostAndUsageResponse (Prelude.Maybe [DimensionValuesWithAttributes])
getCostAndUsageResponse_dimensionValueAttributes = Lens.lens (\GetCostAndUsageResponse' {dimensionValueAttributes} -> dimensionValueAttributes) (\s@GetCostAndUsageResponse' {} a -> s {dimensionValueAttributes = a} :: GetCostAndUsageResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in
-- the request.
getCostAndUsageResponse_groupDefinitions :: Lens.Lens' GetCostAndUsageResponse (Prelude.Maybe [GroupDefinition])
getCostAndUsageResponse_groupDefinitions = Lens.lens (\GetCostAndUsageResponse' {groupDefinitions} -> groupDefinitions) (\s@GetCostAndUsageResponse' {} a -> s {groupDefinitions = a} :: GetCostAndUsageResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getCostAndUsageResponse_httpStatus :: Lens.Lens' GetCostAndUsageResponse Prelude.Int
getCostAndUsageResponse_httpStatus = Lens.lens (\GetCostAndUsageResponse' {httpStatus} -> httpStatus) (\s@GetCostAndUsageResponse' {} a -> s {httpStatus = a} :: GetCostAndUsageResponse)

instance Prelude.NFData GetCostAndUsageResponse
