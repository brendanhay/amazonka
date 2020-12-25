{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.ListMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the specified metrics. You can use the returned metrics with <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData> or <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics> to obtain statistical data.
--
-- Up to 500 results are returned for any one call. To retrieve additional results, use the returned token with subsequent calls.
-- After you create a metric, allow up to 15 minutes before the metric appears. You can see statistics about the metric sooner by using <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData> or <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics> .
-- @ListMetrics@ doesn't return information about metrics if those metrics haven't reported data in the past two weeks. To retrieve those metrics, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData> or <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics> .
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.ListMetrics
  ( -- * Creating a request
    ListMetrics (..),
    mkListMetrics,

    -- ** Request lenses
    lmDimensions,
    lmMetricName,
    lmNamespace,
    lmNextToken,
    lmRecentlyActive,

    -- * Destructuring the response
    ListMetricsResponse (..),
    mkListMetricsResponse,

    -- ** Response lenses
    lmrrsMetrics,
    lmrrsNextToken,
    lmrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListMetrics' smart constructor.
data ListMetrics = ListMetrics'
  { -- | The dimensions to filter against. Only the dimensions that match exactly will be returned.
    dimensions :: Core.Maybe [Types.DimensionFilter],
    -- | The name of the metric to filter against. Only the metrics with names that match exactly will be returned.
    metricName :: Core.Maybe Types.MetricName,
    -- | The metric namespace to filter against. Only the namespace that matches exactly will be returned.
    namespace :: Core.Maybe Types.Namespace,
    -- | The token returned by a previous call to indicate that there is more data available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | To filter the results to show only metrics that have had data points published in the past three hours, specify this parameter with a value of @PT3H@ . This is the only valid value for this parameter.
    --
    -- The results that are returned are an approximation of the value you specify. There is a low probability that the returned results include metrics with last published data as much as 40 minutes more than the specified time interval.
    recentlyActive :: Core.Maybe Types.RecentlyActive
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMetrics' value with any optional fields omitted.
mkListMetrics ::
  ListMetrics
mkListMetrics =
  ListMetrics'
    { dimensions = Core.Nothing,
      metricName = Core.Nothing,
      namespace = Core.Nothing,
      nextToken = Core.Nothing,
      recentlyActive = Core.Nothing
    }

-- | The dimensions to filter against. Only the dimensions that match exactly will be returned.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmDimensions :: Lens.Lens' ListMetrics (Core.Maybe [Types.DimensionFilter])
lmDimensions = Lens.field @"dimensions"
{-# DEPRECATED lmDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The name of the metric to filter against. Only the metrics with names that match exactly will be returned.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmMetricName :: Lens.Lens' ListMetrics (Core.Maybe Types.MetricName)
lmMetricName = Lens.field @"metricName"
{-# DEPRECATED lmMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The metric namespace to filter against. Only the namespace that matches exactly will be returned.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNamespace :: Lens.Lens' ListMetrics (Core.Maybe Types.Namespace)
lmNamespace = Lens.field @"namespace"
{-# DEPRECATED lmNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNextToken :: Lens.Lens' ListMetrics (Core.Maybe Types.NextToken)
lmNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | To filter the results to show only metrics that have had data points published in the past three hours, specify this parameter with a value of @PT3H@ . This is the only valid value for this parameter.
--
-- The results that are returned are an approximation of the value you specify. There is a low probability that the returned results include metrics with last published data as much as 40 minutes more than the specified time interval.
--
-- /Note:/ Consider using 'recentlyActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmRecentlyActive :: Lens.Lens' ListMetrics (Core.Maybe Types.RecentlyActive)
lmRecentlyActive = Lens.field @"recentlyActive"
{-# DEPRECATED lmRecentlyActive "Use generic-lens or generic-optics with 'recentlyActive' instead." #-}

instance Core.AWSRequest ListMetrics where
  type Rs ListMetrics = ListMetricsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListMetrics")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> ( Core.toQueryValue
                            "Dimensions"
                            (Core.toQueryList "member" Core.<$> dimensions)
                        )
                Core.<> (Core.toQueryValue "MetricName" Core.<$> metricName)
                Core.<> (Core.toQueryValue "Namespace" Core.<$> namespace)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryValue "RecentlyActive" Core.<$> recentlyActive)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListMetricsResult"
      ( \s h x ->
          ListMetricsResponse'
            Core.<$> (x Core..@? "Metrics" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListMetrics where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"metrics" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListMetricsResponse' smart constructor.
data ListMetricsResponse = ListMetricsResponse'
  { -- | The metrics that match your request.
    metrics :: Core.Maybe [Types.Metric],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMetricsResponse' value with any optional fields omitted.
mkListMetricsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListMetricsResponse
mkListMetricsResponse responseStatus =
  ListMetricsResponse'
    { metrics = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The metrics that match your request.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsMetrics :: Lens.Lens' ListMetricsResponse (Core.Maybe [Types.Metric])
lmrrsMetrics = Lens.field @"metrics"
{-# DEPRECATED lmrrsMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsNextToken :: Lens.Lens' ListMetricsResponse (Core.Maybe Types.NextToken)
lmrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsResponseStatus :: Lens.Lens' ListMetricsResponse Core.Int
lmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
