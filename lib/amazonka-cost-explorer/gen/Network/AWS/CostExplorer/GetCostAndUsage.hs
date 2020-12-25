{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetCostAndUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves cost and usage metrics for your account. You can specify which cost and usage-related metric, such as @BlendedCosts@ or @UsageQuantity@ , that you want the request to return. You can also filter and group your data by various dimensions, such as @SERVICE@ or @AZ@ , in a specific time range. For a complete list of valid dimensions, see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html GetDimensionValues> operation. Management account in an organization in AWS Organizations have access to all member accounts.
--
-- For information about filter limitations, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-limits.html Quotas and restrictions> in the /Billing and Cost Management User Guide/ .
module Network.AWS.CostExplorer.GetCostAndUsage
  ( -- * Creating a request
    GetCostAndUsage (..),
    mkGetCostAndUsage,

    -- ** Request lenses
    gcauTimePeriod,
    gcauMetrics,
    gcauFilter,
    gcauGranularity,
    gcauGroupBy,
    gcauNextPageToken,

    -- * Destructuring the response
    GetCostAndUsageResponse (..),
    mkGetCostAndUsageResponse,

    -- ** Response lenses
    gcaurrsGroupDefinitions,
    gcaurrsNextPageToken,
    gcaurrsResultsByTime,
    gcaurrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCostAndUsage' smart constructor.
data GetCostAndUsage = GetCostAndUsage'
  { -- | Sets the start and end dates for retrieving AWS costs. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
    timePeriod :: Types.DateInterval,
    -- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
    --
    -- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
    -- @Metrics@ is required for @GetCostAndUsage@ requests.
    metrics :: [Types.MetricName],
    -- | Filters AWS costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
    filter :: Core.Maybe Types.Expression,
    -- | Sets the AWS cost granularity to @MONTHLY@ or @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , either @MONTHLY@ or @DAILY@ , or @HOURLY@ .
    granularity :: Core.Maybe Types.Granularity,
    -- | You can group AWS costs using up to two different groups, either dimensions, tag keys, cost categories, or any two group by types.
    --
    -- When you group by tag key, you get all tag values, including empty strings.
    -- Valid values are @AZ@ , @INSTANCE_TYPE@ , @LEGAL_ENTITY_NAME@ , @LINKED_ACCOUNT@ , @OPERATION@ , @PLATFORM@ , @PURCHASE_TYPE@ , @SERVICE@ , @TAGS@ , @TENANCY@ , @RECORD_TYPE@ , and @USAGE_TYPE@ .
    groupBy :: Core.Maybe [Types.GroupDefinition],
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCostAndUsage' value with any optional fields omitted.
mkGetCostAndUsage ::
  -- | 'timePeriod'
  Types.DateInterval ->
  GetCostAndUsage
mkGetCostAndUsage timePeriod =
  GetCostAndUsage'
    { timePeriod,
      metrics = Core.mempty,
      filter = Core.Nothing,
      granularity = Core.Nothing,
      groupBy = Core.Nothing,
      nextPageToken = Core.Nothing
    }

-- | Sets the start and end dates for retrieving AWS costs. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauTimePeriod :: Lens.Lens' GetCostAndUsage Types.DateInterval
gcauTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED gcauTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
--
-- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
-- @Metrics@ is required for @GetCostAndUsage@ requests.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauMetrics :: Lens.Lens' GetCostAndUsage [Types.MetricName]
gcauMetrics = Lens.field @"metrics"
{-# DEPRECATED gcauMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | Filters AWS costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauFilter :: Lens.Lens' GetCostAndUsage (Core.Maybe Types.Expression)
gcauFilter = Lens.field @"filter"
{-# DEPRECATED gcauFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Sets the AWS cost granularity to @MONTHLY@ or @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , either @MONTHLY@ or @DAILY@ , or @HOURLY@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauGranularity :: Lens.Lens' GetCostAndUsage (Core.Maybe Types.Granularity)
gcauGranularity = Lens.field @"granularity"
{-# DEPRECATED gcauGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | You can group AWS costs using up to two different groups, either dimensions, tag keys, cost categories, or any two group by types.
--
-- When you group by tag key, you get all tag values, including empty strings.
-- Valid values are @AZ@ , @INSTANCE_TYPE@ , @LEGAL_ENTITY_NAME@ , @LINKED_ACCOUNT@ , @OPERATION@ , @PLATFORM@ , @PURCHASE_TYPE@ , @SERVICE@ , @TAGS@ , @TENANCY@ , @RECORD_TYPE@ , and @USAGE_TYPE@ .
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauGroupBy :: Lens.Lens' GetCostAndUsage (Core.Maybe [Types.GroupDefinition])
gcauGroupBy = Lens.field @"groupBy"
{-# DEPRECATED gcauGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauNextPageToken :: Lens.Lens' GetCostAndUsage (Core.Maybe Types.NextPageToken)
gcauNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gcauNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

instance Core.FromJSON GetCostAndUsage where
  toJSON GetCostAndUsage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TimePeriod" Core..= timePeriod),
            Core.Just ("Metrics" Core..= metrics),
            ("Filter" Core..=) Core.<$> filter,
            ("Granularity" Core..=) Core.<$> granularity,
            ("GroupBy" Core..=) Core.<$> groupBy,
            ("NextPageToken" Core..=) Core.<$> nextPageToken
          ]
      )

instance Core.AWSRequest GetCostAndUsage where
  type Rs GetCostAndUsage = GetCostAndUsageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSInsightsIndexService.GetCostAndUsage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostAndUsageResponse'
            Core.<$> (x Core..:? "GroupDefinitions")
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "ResultsByTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCostAndUsageResponse' smart constructor.
data GetCostAndUsageResponse = GetCostAndUsageResponse'
  { -- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
    groupDefinitions :: Core.Maybe [Types.GroupDefinition],
    -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The time period that is covered by the results in the response.
    resultsByTime :: Core.Maybe [Types.ResultByTime],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCostAndUsageResponse' value with any optional fields omitted.
mkGetCostAndUsageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCostAndUsageResponse
mkGetCostAndUsageResponse responseStatus =
  GetCostAndUsageResponse'
    { groupDefinitions = Core.Nothing,
      nextPageToken = Core.Nothing,
      resultsByTime = Core.Nothing,
      responseStatus
    }

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
--
-- /Note:/ Consider using 'groupDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaurrsGroupDefinitions :: Lens.Lens' GetCostAndUsageResponse (Core.Maybe [Types.GroupDefinition])
gcaurrsGroupDefinitions = Lens.field @"groupDefinitions"
{-# DEPRECATED gcaurrsGroupDefinitions "Use generic-lens or generic-optics with 'groupDefinitions' instead." #-}

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaurrsNextPageToken :: Lens.Lens' GetCostAndUsageResponse (Core.Maybe Types.NextPageToken)
gcaurrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED gcaurrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The time period that is covered by the results in the response.
--
-- /Note:/ Consider using 'resultsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaurrsResultsByTime :: Lens.Lens' GetCostAndUsageResponse (Core.Maybe [Types.ResultByTime])
gcaurrsResultsByTime = Lens.field @"resultsByTime"
{-# DEPRECATED gcaurrsResultsByTime "Use generic-lens or generic-optics with 'resultsByTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaurrsResponseStatus :: Lens.Lens' GetCostAndUsageResponse Core.Int
gcaurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcaurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
