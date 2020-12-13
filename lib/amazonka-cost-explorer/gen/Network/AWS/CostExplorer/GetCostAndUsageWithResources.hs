{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetCostAndUsageWithResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves cost and usage metrics with resources for your account. You can specify which cost and usage-related metric, such as @BlendedCosts@ or @UsageQuantity@ , that you want the request to return. You can also filter and group your data by various dimensions, such as @SERVICE@ or @AZ@ , in a specific time range. For a complete list of valid dimensions, see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_GetDimensionValues.html GetDimensionValues> operation. Management account in an organization in AWS Organizations have access to all member accounts. This API is currently available for the Amazon Elastic Compute Cloud – Compute service only.
module Network.AWS.CostExplorer.GetCostAndUsageWithResources
  ( -- * Creating a request
    GetCostAndUsageWithResources (..),
    mkGetCostAndUsageWithResources,

    -- ** Request lenses
    gcauwrGroupBy,
    gcauwrNextPageToken,
    gcauwrMetrics,
    gcauwrTimePeriod,
    gcauwrGranularity,
    gcauwrFilter,

    -- * Destructuring the response
    GetCostAndUsageWithResourcesResponse (..),
    mkGetCostAndUsageWithResourcesResponse,

    -- ** Response lenses
    gcauwrrsResultsByTime,
    gcauwrrsNextPageToken,
    gcauwrrsGroupDefinitions,
    gcauwrrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCostAndUsageWithResources' smart constructor.
data GetCostAndUsageWithResources = GetCostAndUsageWithResources'
  { -- | You can group Amazon Web Services costs using up to two different groups: @DIMENSION@ , @TAG@ , @COST_CATEGORY@ .
    groupBy :: Lude.Maybe [GroupDefinition],
    -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
    --
    -- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
    -- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
    metrics :: Lude.Maybe [Lude.Text],
    -- | Sets the start and end dates for retrieving Amazon Web Services costs. The range must be within the last 14 days (the start date cannot be earlier than 14 days ago). The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
    timePeriod :: DateInterval,
    -- | Sets the AWS cost granularity to @MONTHLY@ , @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , @MONTHLY@ , @DAILY@ , or @HOURLY@ .
    granularity :: Lude.Maybe Granularity,
    -- | Filters Amazon Web Services costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
    --
    -- The @GetCostAndUsageWithResources@ operation requires that you either group by or filter by a @ResourceId@ . It requires the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @"SERVICE = Amazon Elastic Compute Cloud - Compute"@ in the filter.
    filter :: Expression
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCostAndUsageWithResources' with the minimum fields required to make a request.
--
-- * 'groupBy' - You can group Amazon Web Services costs using up to two different groups: @DIMENSION@ , @TAG@ , @COST_CATEGORY@ .
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'metrics' - Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
--
-- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
-- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
-- * 'timePeriod' - Sets the start and end dates for retrieving Amazon Web Services costs. The range must be within the last 14 days (the start date cannot be earlier than 14 days ago). The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
-- * 'granularity' - Sets the AWS cost granularity to @MONTHLY@ , @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , @MONTHLY@ , @DAILY@ , or @HOURLY@ .
-- * 'filter' - Filters Amazon Web Services costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
--
-- The @GetCostAndUsageWithResources@ operation requires that you either group by or filter by a @ResourceId@ . It requires the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @"SERVICE = Amazon Elastic Compute Cloud - Compute"@ in the filter.
mkGetCostAndUsageWithResources ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'filter'
  Expression ->
  GetCostAndUsageWithResources
mkGetCostAndUsageWithResources pTimePeriod_ pFilter_ =
  GetCostAndUsageWithResources'
    { groupBy = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      metrics = Lude.Nothing,
      timePeriod = pTimePeriod_,
      granularity = Lude.Nothing,
      filter = pFilter_
    }

-- | You can group Amazon Web Services costs using up to two different groups: @DIMENSION@ , @TAG@ , @COST_CATEGORY@ .
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrGroupBy :: Lens.Lens' GetCostAndUsageWithResources (Lude.Maybe [GroupDefinition])
gcauwrGroupBy = Lens.lens (groupBy :: GetCostAndUsageWithResources -> Lude.Maybe [GroupDefinition]) (\s a -> s {groupBy = a} :: GetCostAndUsageWithResources)
{-# DEPRECATED gcauwrGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrNextPageToken :: Lens.Lens' GetCostAndUsageWithResources (Lude.Maybe Lude.Text)
gcauwrNextPageToken = Lens.lens (nextPageToken :: GetCostAndUsageWithResources -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetCostAndUsageWithResources)
{-# DEPRECATED gcauwrNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
--
-- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
-- @Metrics@ is required for @GetCostAndUsageWithResources@ requests.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrMetrics :: Lens.Lens' GetCostAndUsageWithResources (Lude.Maybe [Lude.Text])
gcauwrMetrics = Lens.lens (metrics :: GetCostAndUsageWithResources -> Lude.Maybe [Lude.Text]) (\s a -> s {metrics = a} :: GetCostAndUsageWithResources)
{-# DEPRECATED gcauwrMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | Sets the start and end dates for retrieving Amazon Web Services costs. The range must be within the last 14 days (the start date cannot be earlier than 14 days ago). The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrTimePeriod :: Lens.Lens' GetCostAndUsageWithResources DateInterval
gcauwrTimePeriod = Lens.lens (timePeriod :: GetCostAndUsageWithResources -> DateInterval) (\s a -> s {timePeriod = a} :: GetCostAndUsageWithResources)
{-# DEPRECATED gcauwrTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Sets the AWS cost granularity to @MONTHLY@ , @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , @MONTHLY@ , @DAILY@ , or @HOURLY@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrGranularity :: Lens.Lens' GetCostAndUsageWithResources (Lude.Maybe Granularity)
gcauwrGranularity = Lens.lens (granularity :: GetCostAndUsageWithResources -> Lude.Maybe Granularity) (\s a -> s {granularity = a} :: GetCostAndUsageWithResources)
{-# DEPRECATED gcauwrGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | Filters Amazon Web Services costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
--
-- The @GetCostAndUsageWithResources@ operation requires that you either group by or filter by a @ResourceId@ . It requires the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> @"SERVICE = Amazon Elastic Compute Cloud - Compute"@ in the filter.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrFilter :: Lens.Lens' GetCostAndUsageWithResources Expression
gcauwrFilter = Lens.lens (filter :: GetCostAndUsageWithResources -> Expression) (\s a -> s {filter = a} :: GetCostAndUsageWithResources)
{-# DEPRECATED gcauwrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.AWSRequest GetCostAndUsageWithResources where
  type
    Rs GetCostAndUsageWithResources =
      GetCostAndUsageWithResourcesResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCostAndUsageWithResourcesResponse'
            Lude.<$> (x Lude..?> "ResultsByTime" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "GroupDefinitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCostAndUsageWithResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetCostAndUsageWithResources" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCostAndUsageWithResources where
  toJSON GetCostAndUsageWithResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupBy" Lude..=) Lude.<$> groupBy,
            ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("Metrics" Lude..=) Lude.<$> metrics,
            Lude.Just ("TimePeriod" Lude..= timePeriod),
            ("Granularity" Lude..=) Lude.<$> granularity,
            Lude.Just ("Filter" Lude..= filter)
          ]
      )

instance Lude.ToPath GetCostAndUsageWithResources where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCostAndUsageWithResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCostAndUsageWithResourcesResponse' smart constructor.
data GetCostAndUsageWithResourcesResponse = GetCostAndUsageWithResourcesResponse'
  { -- | The time period that is covered by the results in the response.
    resultsByTime :: Lude.Maybe [ResultByTime],
    -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
    groupDefinitions :: Lude.Maybe [GroupDefinition],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCostAndUsageWithResourcesResponse' with the minimum fields required to make a request.
--
-- * 'resultsByTime' - The time period that is covered by the results in the response.
-- * 'nextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'groupDefinitions' - The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
-- * 'responseStatus' - The response status code.
mkGetCostAndUsageWithResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCostAndUsageWithResourcesResponse
mkGetCostAndUsageWithResourcesResponse pResponseStatus_ =
  GetCostAndUsageWithResourcesResponse'
    { resultsByTime =
        Lude.Nothing,
      nextPageToken = Lude.Nothing,
      groupDefinitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time period that is covered by the results in the response.
--
-- /Note:/ Consider using 'resultsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrrsResultsByTime :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Lude.Maybe [ResultByTime])
gcauwrrsResultsByTime = Lens.lens (resultsByTime :: GetCostAndUsageWithResourcesResponse -> Lude.Maybe [ResultByTime]) (\s a -> s {resultsByTime = a} :: GetCostAndUsageWithResourcesResponse)
{-# DEPRECATED gcauwrrsResultsByTime "Use generic-lens or generic-optics with 'resultsByTime' instead." #-}

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrrsNextPageToken :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Lude.Maybe Lude.Text)
gcauwrrsNextPageToken = Lens.lens (nextPageToken :: GetCostAndUsageWithResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetCostAndUsageWithResourcesResponse)
{-# DEPRECATED gcauwrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
--
-- /Note:/ Consider using 'groupDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrrsGroupDefinitions :: Lens.Lens' GetCostAndUsageWithResourcesResponse (Lude.Maybe [GroupDefinition])
gcauwrrsGroupDefinitions = Lens.lens (groupDefinitions :: GetCostAndUsageWithResourcesResponse -> Lude.Maybe [GroupDefinition]) (\s a -> s {groupDefinitions = a} :: GetCostAndUsageWithResourcesResponse)
{-# DEPRECATED gcauwrrsGroupDefinitions "Use generic-lens or generic-optics with 'groupDefinitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauwrrsResponseStatus :: Lens.Lens' GetCostAndUsageWithResourcesResponse Lude.Int
gcauwrrsResponseStatus = Lens.lens (responseStatus :: GetCostAndUsageWithResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCostAndUsageWithResourcesResponse)
{-# DEPRECATED gcauwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
