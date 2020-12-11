{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gcauGroupBy,
    gcauNextPageToken,
    gcauGranularity,
    gcauFilter,
    gcauTimePeriod,
    gcauMetrics,

    -- * Destructuring the response
    GetCostAndUsageResponse (..),
    mkGetCostAndUsageResponse,

    -- ** Response lenses
    gcaursResultsByTime,
    gcaursNextPageToken,
    gcaursGroupDefinitions,
    gcaursResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCostAndUsage' smart constructor.
data GetCostAndUsage = GetCostAndUsage'
  { groupBy ::
      Lude.Maybe [GroupDefinition],
    nextPageToken :: Lude.Maybe Lude.Text,
    granularity :: Lude.Maybe Granularity,
    filter :: Lude.Maybe Expression,
    timePeriod :: DateInterval,
    metrics :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCostAndUsage' with the minimum fields required to make a request.
--
-- * 'filter' - Filters AWS costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
-- * 'granularity' - Sets the AWS cost granularity to @MONTHLY@ or @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , either @MONTHLY@ or @DAILY@ , or @HOURLY@ .
-- * 'groupBy' - You can group AWS costs using up to two different groups, either dimensions, tag keys, cost categories, or any two group by types.
--
-- When you group by tag key, you get all tag values, including empty strings.
-- Valid values are @AZ@ , @INSTANCE_TYPE@ , @LEGAL_ENTITY_NAME@ , @LINKED_ACCOUNT@ , @OPERATION@ , @PLATFORM@ , @PURCHASE_TYPE@ , @SERVICE@ , @TAGS@ , @TENANCY@ , @RECORD_TYPE@ , and @USAGE_TYPE@ .
-- * 'metrics' - Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
--
-- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
-- @Metrics@ is required for @GetCostAndUsage@ requests.
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'timePeriod' - Sets the start and end dates for retrieving AWS costs. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
mkGetCostAndUsage ::
  -- | 'timePeriod'
  DateInterval ->
  GetCostAndUsage
mkGetCostAndUsage pTimePeriod_ =
  GetCostAndUsage'
    { groupBy = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      granularity = Lude.Nothing,
      filter = Lude.Nothing,
      timePeriod = pTimePeriod_,
      metrics = Lude.mempty
    }

-- | You can group AWS costs using up to two different groups, either dimensions, tag keys, cost categories, or any two group by types.
--
-- When you group by tag key, you get all tag values, including empty strings.
-- Valid values are @AZ@ , @INSTANCE_TYPE@ , @LEGAL_ENTITY_NAME@ , @LINKED_ACCOUNT@ , @OPERATION@ , @PLATFORM@ , @PURCHASE_TYPE@ , @SERVICE@ , @TAGS@ , @TENANCY@ , @RECORD_TYPE@ , and @USAGE_TYPE@ .
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauGroupBy :: Lens.Lens' GetCostAndUsage (Lude.Maybe [GroupDefinition])
gcauGroupBy = Lens.lens (groupBy :: GetCostAndUsage -> Lude.Maybe [GroupDefinition]) (\s a -> s {groupBy = a} :: GetCostAndUsage)
{-# DEPRECATED gcauGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauNextPageToken :: Lens.Lens' GetCostAndUsage (Lude.Maybe Lude.Text)
gcauNextPageToken = Lens.lens (nextPageToken :: GetCostAndUsage -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetCostAndUsage)
{-# DEPRECATED gcauNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | Sets the AWS cost granularity to @MONTHLY@ or @DAILY@ , or @HOURLY@ . If @Granularity@ isn't set, the response object doesn't include the @Granularity@ , either @MONTHLY@ or @DAILY@ , or @HOURLY@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauGranularity :: Lens.Lens' GetCostAndUsage (Lude.Maybe Granularity)
gcauGranularity = Lens.lens (granularity :: GetCostAndUsage -> Lude.Maybe Granularity) (\s a -> s {granularity = a} :: GetCostAndUsage)
{-# DEPRECATED gcauGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | Filters AWS costs by different dimensions. For example, you can specify @SERVICE@ and @LINKED_ACCOUNT@ and get the costs that are associated with that account's usage of that service. You can nest @Expression@ objects to define any combination of dimension filters. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> .
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauFilter :: Lens.Lens' GetCostAndUsage (Lude.Maybe Expression)
gcauFilter = Lens.lens (filter :: GetCostAndUsage -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetCostAndUsage)
{-# DEPRECATED gcauFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | Sets the start and end dates for retrieving AWS costs. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauTimePeriod :: Lens.Lens' GetCostAndUsage DateInterval
gcauTimePeriod = Lens.lens (timePeriod :: GetCostAndUsage -> DateInterval) (\s a -> s {timePeriod = a} :: GetCostAndUsage)
{-# DEPRECATED gcauTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | Which metrics are returned in the query. For more information about blended and unblended rates, see <http://aws.amazon.com/premiumsupport/knowledge-center/blended-rates-intro/ Why does the "blended" annotation appear on some line items in my bill?> .
--
-- Valid values are @AmortizedCost@ , @BlendedCost@ , @NetAmortizedCost@ , @NetUnblendedCost@ , @NormalizedUsageAmount@ , @UnblendedCost@ , and @UsageQuantity@ .
-- @Metrics@ is required for @GetCostAndUsage@ requests.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcauMetrics :: Lens.Lens' GetCostAndUsage [Lude.Text]
gcauMetrics = Lens.lens (metrics :: GetCostAndUsage -> [Lude.Text]) (\s a -> s {metrics = a} :: GetCostAndUsage)
{-# DEPRECATED gcauMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

instance Lude.AWSRequest GetCostAndUsage where
  type Rs GetCostAndUsage = GetCostAndUsageResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCostAndUsageResponse'
            Lude.<$> (x Lude..?> "ResultsByTime" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "GroupDefinitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCostAndUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSInsightsIndexService.GetCostAndUsage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCostAndUsage where
  toJSON GetCostAndUsage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupBy" Lude..=) Lude.<$> groupBy,
            ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("Granularity" Lude..=) Lude.<$> granularity,
            ("Filter" Lude..=) Lude.<$> filter,
            Lude.Just ("TimePeriod" Lude..= timePeriod),
            Lude.Just ("Metrics" Lude..= metrics)
          ]
      )

instance Lude.ToPath GetCostAndUsage where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCostAndUsage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCostAndUsageResponse' smart constructor.
data GetCostAndUsageResponse = GetCostAndUsageResponse'
  { resultsByTime ::
      Lude.Maybe [ResultByTime],
    nextPageToken :: Lude.Maybe Lude.Text,
    groupDefinitions ::
      Lude.Maybe [GroupDefinition],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCostAndUsageResponse' with the minimum fields required to make a request.
--
-- * 'groupDefinitions' - The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
-- * 'nextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'responseStatus' - The response status code.
-- * 'resultsByTime' - The time period that is covered by the results in the response.
mkGetCostAndUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCostAndUsageResponse
mkGetCostAndUsageResponse pResponseStatus_ =
  GetCostAndUsageResponse'
    { resultsByTime = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      groupDefinitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time period that is covered by the results in the response.
--
-- /Note:/ Consider using 'resultsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaursResultsByTime :: Lens.Lens' GetCostAndUsageResponse (Lude.Maybe [ResultByTime])
gcaursResultsByTime = Lens.lens (resultsByTime :: GetCostAndUsageResponse -> Lude.Maybe [ResultByTime]) (\s a -> s {resultsByTime = a} :: GetCostAndUsageResponse)
{-# DEPRECATED gcaursResultsByTime "Use generic-lens or generic-optics with 'resultsByTime' instead." #-}

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaursNextPageToken :: Lens.Lens' GetCostAndUsageResponse (Lude.Maybe Lude.Text)
gcaursNextPageToken = Lens.lens (nextPageToken :: GetCostAndUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetCostAndUsageResponse)
{-# DEPRECATED gcaursNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The groups that are specified by the @Filter@ or @GroupBy@ parameters in the request.
--
-- /Note:/ Consider using 'groupDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaursGroupDefinitions :: Lens.Lens' GetCostAndUsageResponse (Lude.Maybe [GroupDefinition])
gcaursGroupDefinitions = Lens.lens (groupDefinitions :: GetCostAndUsageResponse -> Lude.Maybe [GroupDefinition]) (\s a -> s {groupDefinitions = a} :: GetCostAndUsageResponse)
{-# DEPRECATED gcaursGroupDefinitions "Use generic-lens or generic-optics with 'groupDefinitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcaursResponseStatus :: Lens.Lens' GetCostAndUsageResponse Lude.Int
gcaursResponseStatus = Lens.lens (responseStatus :: GetCostAndUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCostAndUsageResponse)
{-# DEPRECATED gcaursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
