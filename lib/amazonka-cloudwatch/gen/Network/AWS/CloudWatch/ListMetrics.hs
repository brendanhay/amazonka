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
    lmMetricName,
    lmNamespace,
    lmNextToken,
    lmRecentlyActive,
    lmDimensions,

    -- * Destructuring the response
    ListMetricsResponse (..),
    mkListMetricsResponse,

    -- ** Response lenses
    lmrsMetrics,
    lmrsNextToken,
    lmrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListMetrics' smart constructor.
data ListMetrics = ListMetrics'
  { -- | The name of the metric to filter against. Only the metrics with names that match exactly will be returned.
    metricName :: Lude.Maybe Lude.Text,
    -- | The metric namespace to filter against. Only the namespace that matches exactly will be returned.
    namespace :: Lude.Maybe Lude.Text,
    -- | The token returned by a previous call to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | To filter the results to show only metrics that have had data points published in the past three hours, specify this parameter with a value of @PT3H@ . This is the only valid value for this parameter.
    --
    -- The results that are returned are an approximation of the value you specify. There is a low probability that the returned results include metrics with last published data as much as 40 minutes more than the specified time interval.
    recentlyActive :: Lude.Maybe RecentlyActive,
    -- | The dimensions to filter against. Only the dimensions that match exactly will be returned.
    dimensions :: Lude.Maybe [DimensionFilter]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMetrics' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the metric to filter against. Only the metrics with names that match exactly will be returned.
-- * 'namespace' - The metric namespace to filter against. Only the namespace that matches exactly will be returned.
-- * 'nextToken' - The token returned by a previous call to indicate that there is more data available.
-- * 'recentlyActive' - To filter the results to show only metrics that have had data points published in the past three hours, specify this parameter with a value of @PT3H@ . This is the only valid value for this parameter.
--
-- The results that are returned are an approximation of the value you specify. There is a low probability that the returned results include metrics with last published data as much as 40 minutes more than the specified time interval.
-- * 'dimensions' - The dimensions to filter against. Only the dimensions that match exactly will be returned.
mkListMetrics ::
  ListMetrics
mkListMetrics =
  ListMetrics'
    { metricName = Lude.Nothing,
      namespace = Lude.Nothing,
      nextToken = Lude.Nothing,
      recentlyActive = Lude.Nothing,
      dimensions = Lude.Nothing
    }

-- | The name of the metric to filter against. Only the metrics with names that match exactly will be returned.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmMetricName :: Lens.Lens' ListMetrics (Lude.Maybe Lude.Text)
lmMetricName = Lens.lens (metricName :: ListMetrics -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: ListMetrics)
{-# DEPRECATED lmMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The metric namespace to filter against. Only the namespace that matches exactly will be returned.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNamespace :: Lens.Lens' ListMetrics (Lude.Maybe Lude.Text)
lmNamespace = Lens.lens (namespace :: ListMetrics -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: ListMetrics)
{-# DEPRECATED lmNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNextToken :: Lens.Lens' ListMetrics (Lude.Maybe Lude.Text)
lmNextToken = Lens.lens (nextToken :: ListMetrics -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMetrics)
{-# DEPRECATED lmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | To filter the results to show only metrics that have had data points published in the past three hours, specify this parameter with a value of @PT3H@ . This is the only valid value for this parameter.
--
-- The results that are returned are an approximation of the value you specify. There is a low probability that the returned results include metrics with last published data as much as 40 minutes more than the specified time interval.
--
-- /Note:/ Consider using 'recentlyActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmRecentlyActive :: Lens.Lens' ListMetrics (Lude.Maybe RecentlyActive)
lmRecentlyActive = Lens.lens (recentlyActive :: ListMetrics -> Lude.Maybe RecentlyActive) (\s a -> s {recentlyActive = a} :: ListMetrics)
{-# DEPRECATED lmRecentlyActive "Use generic-lens or generic-optics with 'recentlyActive' instead." #-}

-- | The dimensions to filter against. Only the dimensions that match exactly will be returned.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmDimensions :: Lens.Lens' ListMetrics (Lude.Maybe [DimensionFilter])
lmDimensions = Lens.lens (dimensions :: ListMetrics -> Lude.Maybe [DimensionFilter]) (\s a -> s {dimensions = a} :: ListMetrics)
{-# DEPRECATED lmDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Page.AWSPager ListMetrics where
  page rq rs
    | Page.stop (rs Lens.^. lmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmrsMetrics) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmNextToken Lens..~ rs Lens.^. lmrsNextToken

instance Lude.AWSRequest ListMetrics where
  type Rs ListMetrics = ListMetricsResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "ListMetricsResult"
      ( \s h x ->
          ListMetricsResponse'
            Lude.<$> ( x Lude..@? "Metrics" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMetrics where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListMetrics where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMetrics where
  toQuery ListMetrics' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListMetrics" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "MetricName" Lude.=: metricName,
        "Namespace" Lude.=: namespace,
        "NextToken" Lude.=: nextToken,
        "RecentlyActive" Lude.=: recentlyActive,
        "Dimensions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> dimensions)
      ]

-- | /See:/ 'mkListMetricsResponse' smart constructor.
data ListMetricsResponse = ListMetricsResponse'
  { -- | The metrics that match your request.
    metrics :: Lude.Maybe [Metric],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMetricsResponse' with the minimum fields required to make a request.
--
-- * 'metrics' - The metrics that match your request.
-- * 'nextToken' - The token that marks the start of the next batch of returned results.
-- * 'responseStatus' - The response status code.
mkListMetricsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMetricsResponse
mkListMetricsResponse pResponseStatus_ =
  ListMetricsResponse'
    { metrics = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The metrics that match your request.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsMetrics :: Lens.Lens' ListMetricsResponse (Lude.Maybe [Metric])
lmrsMetrics = Lens.lens (metrics :: ListMetricsResponse -> Lude.Maybe [Metric]) (\s a -> s {metrics = a} :: ListMetricsResponse)
{-# DEPRECATED lmrsMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsNextToken :: Lens.Lens' ListMetricsResponse (Lude.Maybe Lude.Text)
lmrsNextToken = Lens.lens (nextToken :: ListMetricsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMetricsResponse)
{-# DEPRECATED lmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsResponseStatus :: Lens.Lens' ListMetricsResponse Lude.Int
lmrsResponseStatus = Lens.lens (responseStatus :: ListMetricsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMetricsResponse)
{-# DEPRECATED lmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
