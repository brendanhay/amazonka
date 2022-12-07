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
-- Module      : Amazonka.CostExplorer.GetCostAndUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves cost and usage metrics for your account. You can specify which
-- cost and usage-related metric that you want the request to return. For
-- example, you can specify @BlendedCosts@ or @UsageQuantity@. You can also
-- filter and group your data by various dimensions, such as @SERVICE@ or
-- @AZ@, in a specific time range. For a complete list of valid dimensions,
-- see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html GetDimensionValues>
-- operation. Management account in an organization in Organizations have
-- access to all member accounts.
--
-- For information about filter limitations, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-limits.html Quotas and restrictions>
-- in the /Billing and Cost Management User Guide/.
module Amazonka.CostExplorer.GetCostAndUsage
  ( -- * Creating a Request
    GetCostAndUsage (..),
    newGetCostAndUsage,

    -- * Request Lenses
    getCostAndUsage_nextPageToken,
    getCostAndUsage_groupBy,
    getCostAndUsage_filter,
    getCostAndUsage_timePeriod,
    getCostAndUsage_granularity,
    getCostAndUsage_metrics,

    -- * Destructuring the Response
    GetCostAndUsageResponse (..),
    newGetCostAndUsageResponse,

    -- * Response Lenses
    getCostAndUsageResponse_nextPageToken,
    getCostAndUsageResponse_dimensionValueAttributes,
    getCostAndUsageResponse_groupDefinitions,
    getCostAndUsageResponse_resultsByTime,
    getCostAndUsageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCostAndUsage' smart constructor.
data GetCostAndUsage = GetCostAndUsage'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | You can group Amazon Web Services costs using up to two different
    -- groups, either dimensions, tag keys, cost categories, or any two group
    -- by types.
    --
    -- Valid values for the @DIMENSION@ type are @AZ@, @INSTANCE_TYPE@,
    -- @LEGAL_ENTITY_NAME@, @INVOICING_ENTITY@, @LINKED_ACCOUNT@, @OPERATION@,
    -- @PLATFORM@, @PURCHASE_TYPE@, @SERVICE@, @TENANCY@, @RECORD_TYPE@, and
    -- @USAGE_TYPE@.
    --
    -- When you group by the @TAG@ type and include a valid tag key, you get
    -- all tag values, including empty strings.
    groupBy :: Prelude.Maybe [GroupDefinition],
    -- | Filters Amazon Web Services costs by different dimensions. For example,
    -- you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that
    -- are associated with that account\'s usage of that service. You can nest
    -- @Expression@ objects to define any combination of dimension filters. For
    -- more information, see
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
    --
    -- Valid values for @MatchOptions@ for @Dimensions@ are @EQUALS@ and
    -- @CASE_SENSITIVE@.
    --
    -- Valid values for @MatchOptions@ for @CostCategories@ and @Tags@ are
    -- @EQUALS@, @ABSENT@, and @CASE_SENSITIVE@. Default values are @EQUALS@
    -- and @CASE_SENSITIVE@.
    filter' :: Prelude.Maybe Expression,
    -- | Sets the start date and end date for retrieving Amazon Web Services
    -- costs. The start date is inclusive, but the end date is exclusive. For
    -- example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the
    -- cost and usage data is retrieved from @2017-01-01@ up to and including
    -- @2017-04-30@ but not including @2017-05-01@.
    timePeriod :: DateInterval,
    -- | Sets the Amazon Web Services cost granularity to @MONTHLY@ or @DAILY@,
    -- or @HOURLY@. If @Granularity@ isn\'t set, the response object doesn\'t
    -- include the @Granularity@, either @MONTHLY@ or @DAILY@, or @HOURLY@.
    granularity :: Granularity,
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
    -- measured in different units (for example, hours and GB). To get more
    -- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
    -- @UsageTypeGroups@.
    --
    -- @Metrics@ is required for @GetCostAndUsage@ requests.
    metrics :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostAndUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getCostAndUsage_nextPageToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'groupBy', 'getCostAndUsage_groupBy' - You can group Amazon Web Services costs using up to two different
-- groups, either dimensions, tag keys, cost categories, or any two group
-- by types.
--
-- Valid values for the @DIMENSION@ type are @AZ@, @INSTANCE_TYPE@,
-- @LEGAL_ENTITY_NAME@, @INVOICING_ENTITY@, @LINKED_ACCOUNT@, @OPERATION@,
-- @PLATFORM@, @PURCHASE_TYPE@, @SERVICE@, @TENANCY@, @RECORD_TYPE@, and
-- @USAGE_TYPE@.
--
-- When you group by the @TAG@ type and include a valid tag key, you get
-- all tag values, including empty strings.
--
-- 'filter'', 'getCostAndUsage_filter' - Filters Amazon Web Services costs by different dimensions. For example,
-- you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that
-- are associated with that account\'s usage of that service. You can nest
-- @Expression@ objects to define any combination of dimension filters. For
-- more information, see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
--
-- Valid values for @MatchOptions@ for @Dimensions@ are @EQUALS@ and
-- @CASE_SENSITIVE@.
--
-- Valid values for @MatchOptions@ for @CostCategories@ and @Tags@ are
-- @EQUALS@, @ABSENT@, and @CASE_SENSITIVE@. Default values are @EQUALS@
-- and @CASE_SENSITIVE@.
--
-- 'timePeriod', 'getCostAndUsage_timePeriod' - Sets the start date and end date for retrieving Amazon Web Services
-- costs. The start date is inclusive, but the end date is exclusive. For
-- example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the
-- cost and usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
--
-- 'granularity', 'getCostAndUsage_granularity' - Sets the Amazon Web Services cost granularity to @MONTHLY@ or @DAILY@,
-- or @HOURLY@. If @Granularity@ isn\'t set, the response object doesn\'t
-- include the @Granularity@, either @MONTHLY@ or @DAILY@, or @HOURLY@.
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
-- measured in different units (for example, hours and GB). To get more
-- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
-- @UsageTypeGroups@.
--
-- @Metrics@ is required for @GetCostAndUsage@ requests.
newGetCostAndUsage ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'granularity'
  Granularity ->
  GetCostAndUsage
newGetCostAndUsage pTimePeriod_ pGranularity_ =
  GetCostAndUsage'
    { nextPageToken = Prelude.Nothing,
      groupBy = Prelude.Nothing,
      filter' = Prelude.Nothing,
      timePeriod = pTimePeriod_,
      granularity = pGranularity_,
      metrics = Prelude.mempty
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getCostAndUsage_nextPageToken :: Lens.Lens' GetCostAndUsage (Prelude.Maybe Prelude.Text)
getCostAndUsage_nextPageToken = Lens.lens (\GetCostAndUsage' {nextPageToken} -> nextPageToken) (\s@GetCostAndUsage' {} a -> s {nextPageToken = a} :: GetCostAndUsage)

-- | You can group Amazon Web Services costs using up to two different
-- groups, either dimensions, tag keys, cost categories, or any two group
-- by types.
--
-- Valid values for the @DIMENSION@ type are @AZ@, @INSTANCE_TYPE@,
-- @LEGAL_ENTITY_NAME@, @INVOICING_ENTITY@, @LINKED_ACCOUNT@, @OPERATION@,
-- @PLATFORM@, @PURCHASE_TYPE@, @SERVICE@, @TENANCY@, @RECORD_TYPE@, and
-- @USAGE_TYPE@.
--
-- When you group by the @TAG@ type and include a valid tag key, you get
-- all tag values, including empty strings.
getCostAndUsage_groupBy :: Lens.Lens' GetCostAndUsage (Prelude.Maybe [GroupDefinition])
getCostAndUsage_groupBy = Lens.lens (\GetCostAndUsage' {groupBy} -> groupBy) (\s@GetCostAndUsage' {} a -> s {groupBy = a} :: GetCostAndUsage) Prelude.. Lens.mapping Lens.coerced

-- | Filters Amazon Web Services costs by different dimensions. For example,
-- you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that
-- are associated with that account\'s usage of that service. You can nest
-- @Expression@ objects to define any combination of dimension filters. For
-- more information, see
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>.
--
-- Valid values for @MatchOptions@ for @Dimensions@ are @EQUALS@ and
-- @CASE_SENSITIVE@.
--
-- Valid values for @MatchOptions@ for @CostCategories@ and @Tags@ are
-- @EQUALS@, @ABSENT@, and @CASE_SENSITIVE@. Default values are @EQUALS@
-- and @CASE_SENSITIVE@.
getCostAndUsage_filter :: Lens.Lens' GetCostAndUsage (Prelude.Maybe Expression)
getCostAndUsage_filter = Lens.lens (\GetCostAndUsage' {filter'} -> filter') (\s@GetCostAndUsage' {} a -> s {filter' = a} :: GetCostAndUsage)

-- | Sets the start date and end date for retrieving Amazon Web Services
-- costs. The start date is inclusive, but the end date is exclusive. For
-- example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@, then the
-- cost and usage data is retrieved from @2017-01-01@ up to and including
-- @2017-04-30@ but not including @2017-05-01@.
getCostAndUsage_timePeriod :: Lens.Lens' GetCostAndUsage DateInterval
getCostAndUsage_timePeriod = Lens.lens (\GetCostAndUsage' {timePeriod} -> timePeriod) (\s@GetCostAndUsage' {} a -> s {timePeriod = a} :: GetCostAndUsage)

-- | Sets the Amazon Web Services cost granularity to @MONTHLY@ or @DAILY@,
-- or @HOURLY@. If @Granularity@ isn\'t set, the response object doesn\'t
-- include the @Granularity@, either @MONTHLY@ or @DAILY@, or @HOURLY@.
getCostAndUsage_granularity :: Lens.Lens' GetCostAndUsage Granularity
getCostAndUsage_granularity = Lens.lens (\GetCostAndUsage' {granularity} -> granularity) (\s@GetCostAndUsage' {} a -> s {granularity = a} :: GetCostAndUsage)

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
-- measured in different units (for example, hours and GB). To get more
-- meaningful @UsageQuantity@ metrics, filter by @UsageType@ or
-- @UsageTypeGroups@.
--
-- @Metrics@ is required for @GetCostAndUsage@ requests.
getCostAndUsage_metrics :: Lens.Lens' GetCostAndUsage [Prelude.Text]
getCostAndUsage_metrics = Lens.lens (\GetCostAndUsage' {metrics} -> metrics) (\s@GetCostAndUsage' {} a -> s {metrics = a} :: GetCostAndUsage) Prelude.. Lens.coerced

instance Core.AWSRequest GetCostAndUsage where
  type
    AWSResponse GetCostAndUsage =
      GetCostAndUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostAndUsageResponse'
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

instance Prelude.Hashable GetCostAndUsage where
  hashWithSalt _salt GetCostAndUsage' {..} =
    _salt `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` groupBy
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` granularity
      `Prelude.hashWithSalt` metrics

instance Prelude.NFData GetCostAndUsage where
  rnf GetCostAndUsage' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf groupBy
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf granularity
      `Prelude.seq` Prelude.rnf metrics

instance Data.ToHeaders GetCostAndUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.GetCostAndUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCostAndUsage where
  toJSON GetCostAndUsage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextPageToken" Data..=) Prelude.<$> nextPageToken,
            ("GroupBy" Data..=) Prelude.<$> groupBy,
            ("Filter" Data..=) Prelude.<$> filter',
            Prelude.Just ("TimePeriod" Data..= timePeriod),
            Prelude.Just ("Granularity" Data..= granularity),
            Prelude.Just ("Metrics" Data..= metrics)
          ]
      )

instance Data.ToPath GetCostAndUsage where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCostAndUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCostAndUsageResponse' smart constructor.
data GetCostAndUsageResponse = GetCostAndUsageResponse'
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
-- Create a value of 'GetCostAndUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getCostAndUsageResponse_nextPageToken' - The token for the next set of retrievable results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'dimensionValueAttributes', 'getCostAndUsageResponse_dimensionValueAttributes' - The attributes that apply to a specific dimension value. For example, if
-- the value is a linked account, the attribute is that account name.
--
-- 'groupDefinitions', 'getCostAndUsageResponse_groupDefinitions' - The groups that are specified by the @Filter@ or @GroupBy@ parameters in
-- the request.
--
-- 'resultsByTime', 'getCostAndUsageResponse_resultsByTime' - The time period that\'s covered by the results in the response.
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
      dimensionValueAttributes = Prelude.Nothing,
      groupDefinitions = Prelude.Nothing,
      resultsByTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of retrievable results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
getCostAndUsageResponse_nextPageToken :: Lens.Lens' GetCostAndUsageResponse (Prelude.Maybe Prelude.Text)
getCostAndUsageResponse_nextPageToken = Lens.lens (\GetCostAndUsageResponse' {nextPageToken} -> nextPageToken) (\s@GetCostAndUsageResponse' {} a -> s {nextPageToken = a} :: GetCostAndUsageResponse)

-- | The attributes that apply to a specific dimension value. For example, if
-- the value is a linked account, the attribute is that account name.
getCostAndUsageResponse_dimensionValueAttributes :: Lens.Lens' GetCostAndUsageResponse (Prelude.Maybe [DimensionValuesWithAttributes])
getCostAndUsageResponse_dimensionValueAttributes = Lens.lens (\GetCostAndUsageResponse' {dimensionValueAttributes} -> dimensionValueAttributes) (\s@GetCostAndUsageResponse' {} a -> s {dimensionValueAttributes = a} :: GetCostAndUsageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in
-- the request.
getCostAndUsageResponse_groupDefinitions :: Lens.Lens' GetCostAndUsageResponse (Prelude.Maybe [GroupDefinition])
getCostAndUsageResponse_groupDefinitions = Lens.lens (\GetCostAndUsageResponse' {groupDefinitions} -> groupDefinitions) (\s@GetCostAndUsageResponse' {} a -> s {groupDefinitions = a} :: GetCostAndUsageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time period that\'s covered by the results in the response.
getCostAndUsageResponse_resultsByTime :: Lens.Lens' GetCostAndUsageResponse (Prelude.Maybe [ResultByTime])
getCostAndUsageResponse_resultsByTime = Lens.lens (\GetCostAndUsageResponse' {resultsByTime} -> resultsByTime) (\s@GetCostAndUsageResponse' {} a -> s {resultsByTime = a} :: GetCostAndUsageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCostAndUsageResponse_httpStatus :: Lens.Lens' GetCostAndUsageResponse Prelude.Int
getCostAndUsageResponse_httpStatus = Lens.lens (\GetCostAndUsageResponse' {httpStatus} -> httpStatus) (\s@GetCostAndUsageResponse' {} a -> s {httpStatus = a} :: GetCostAndUsageResponse)

instance Prelude.NFData GetCostAndUsageResponse where
  rnf GetCostAndUsageResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf dimensionValueAttributes
      `Prelude.seq` Prelude.rnf groupDefinitions
      `Prelude.seq` Prelude.rnf resultsByTime
      `Prelude.seq` Prelude.rnf httpStatus
