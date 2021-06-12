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
-- Module      : Network.AWS.CloudWatch.ListMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the specified metrics. You can use the returned metrics with
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics>
-- to obtain statistical data.
--
-- Up to 500 results are returned for any one call. To retrieve additional
-- results, use the returned token with subsequent calls.
--
-- After you create a metric, allow up to 15 minutes before the metric
-- appears. You can see statistics about the metric sooner by using
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics>.
--
-- @ListMetrics@ doesn\'t return information about metrics if those metrics
-- haven\'t reported data in the past two weeks. To retrieve those metrics,
-- use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics>.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.ListMetrics
  ( -- * Creating a Request
    ListMetrics (..),
    newListMetrics,

    -- * Request Lenses
    listMetrics_nextToken,
    listMetrics_metricName,
    listMetrics_dimensions,
    listMetrics_namespace,
    listMetrics_recentlyActive,

    -- * Destructuring the Response
    ListMetricsResponse (..),
    newListMetricsResponse,

    -- * Response Lenses
    listMetricsResponse_nextToken,
    listMetricsResponse_metrics,
    listMetricsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMetrics' smart constructor.
data ListMetrics = ListMetrics'
  { -- | The token returned by a previous call to indicate that there is more
    -- data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the metric to filter against. Only the metrics with names
    -- that match exactly will be returned.
    metricName :: Core.Maybe Core.Text,
    -- | The dimensions to filter against. Only the dimensions that match exactly
    -- will be returned.
    dimensions :: Core.Maybe [DimensionFilter],
    -- | The metric namespace to filter against. Only the namespace that matches
    -- exactly will be returned.
    namespace :: Core.Maybe Core.Text,
    -- | To filter the results to show only metrics that have had data points
    -- published in the past three hours, specify this parameter with a value
    -- of @PT3H@. This is the only valid value for this parameter.
    --
    -- The results that are returned are an approximation of the value you
    -- specify. There is a low probability that the returned results include
    -- metrics with last published data as much as 40 minutes more than the
    -- specified time interval.
    recentlyActive :: Core.Maybe RecentlyActive
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMetrics_nextToken' - The token returned by a previous call to indicate that there is more
-- data available.
--
-- 'metricName', 'listMetrics_metricName' - The name of the metric to filter against. Only the metrics with names
-- that match exactly will be returned.
--
-- 'dimensions', 'listMetrics_dimensions' - The dimensions to filter against. Only the dimensions that match exactly
-- will be returned.
--
-- 'namespace', 'listMetrics_namespace' - The metric namespace to filter against. Only the namespace that matches
-- exactly will be returned.
--
-- 'recentlyActive', 'listMetrics_recentlyActive' - To filter the results to show only metrics that have had data points
-- published in the past three hours, specify this parameter with a value
-- of @PT3H@. This is the only valid value for this parameter.
--
-- The results that are returned are an approximation of the value you
-- specify. There is a low probability that the returned results include
-- metrics with last published data as much as 40 minutes more than the
-- specified time interval.
newListMetrics ::
  ListMetrics
newListMetrics =
  ListMetrics'
    { nextToken = Core.Nothing,
      metricName = Core.Nothing,
      dimensions = Core.Nothing,
      namespace = Core.Nothing,
      recentlyActive = Core.Nothing
    }

-- | The token returned by a previous call to indicate that there is more
-- data available.
listMetrics_nextToken :: Lens.Lens' ListMetrics (Core.Maybe Core.Text)
listMetrics_nextToken = Lens.lens (\ListMetrics' {nextToken} -> nextToken) (\s@ListMetrics' {} a -> s {nextToken = a} :: ListMetrics)

-- | The name of the metric to filter against. Only the metrics with names
-- that match exactly will be returned.
listMetrics_metricName :: Lens.Lens' ListMetrics (Core.Maybe Core.Text)
listMetrics_metricName = Lens.lens (\ListMetrics' {metricName} -> metricName) (\s@ListMetrics' {} a -> s {metricName = a} :: ListMetrics)

-- | The dimensions to filter against. Only the dimensions that match exactly
-- will be returned.
listMetrics_dimensions :: Lens.Lens' ListMetrics (Core.Maybe [DimensionFilter])
listMetrics_dimensions = Lens.lens (\ListMetrics' {dimensions} -> dimensions) (\s@ListMetrics' {} a -> s {dimensions = a} :: ListMetrics) Core.. Lens.mapping Lens._Coerce

-- | The metric namespace to filter against. Only the namespace that matches
-- exactly will be returned.
listMetrics_namespace :: Lens.Lens' ListMetrics (Core.Maybe Core.Text)
listMetrics_namespace = Lens.lens (\ListMetrics' {namespace} -> namespace) (\s@ListMetrics' {} a -> s {namespace = a} :: ListMetrics)

-- | To filter the results to show only metrics that have had data points
-- published in the past three hours, specify this parameter with a value
-- of @PT3H@. This is the only valid value for this parameter.
--
-- The results that are returned are an approximation of the value you
-- specify. There is a low probability that the returned results include
-- metrics with last published data as much as 40 minutes more than the
-- specified time interval.
listMetrics_recentlyActive :: Lens.Lens' ListMetrics (Core.Maybe RecentlyActive)
listMetrics_recentlyActive = Lens.lens (\ListMetrics' {recentlyActive} -> recentlyActive) (\s@ListMetrics' {} a -> s {recentlyActive = a} :: ListMetrics)

instance Core.AWSPager ListMetrics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMetricsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listMetricsResponse_metrics Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listMetrics_nextToken
          Lens..~ rs
          Lens.^? listMetricsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListMetrics where
  type AWSResponse ListMetrics = ListMetricsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListMetricsResult"
      ( \s h x ->
          ListMetricsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Metrics" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMetrics

instance Core.NFData ListMetrics

instance Core.ToHeaders ListMetrics where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListMetrics where
  toPath = Core.const "/"

instance Core.ToQuery ListMetrics where
  toQuery ListMetrics' {..} =
    Core.mconcat
      [ "Action" Core.=: ("ListMetrics" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MetricName" Core.=: metricName,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> dimensions),
        "Namespace" Core.=: namespace,
        "RecentlyActive" Core.=: recentlyActive
      ]

-- | /See:/ 'newListMetricsResponse' smart constructor.
data ListMetricsResponse = ListMetricsResponse'
  { -- | The token that marks the start of the next batch of returned results.
    nextToken :: Core.Maybe Core.Text,
    -- | The metrics that match your request.
    metrics :: Core.Maybe [Metric],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMetricsResponse_nextToken' - The token that marks the start of the next batch of returned results.
--
-- 'metrics', 'listMetricsResponse_metrics' - The metrics that match your request.
--
-- 'httpStatus', 'listMetricsResponse_httpStatus' - The response's http status code.
newListMetricsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMetricsResponse
newListMetricsResponse pHttpStatus_ =
  ListMetricsResponse'
    { nextToken = Core.Nothing,
      metrics = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that marks the start of the next batch of returned results.
listMetricsResponse_nextToken :: Lens.Lens' ListMetricsResponse (Core.Maybe Core.Text)
listMetricsResponse_nextToken = Lens.lens (\ListMetricsResponse' {nextToken} -> nextToken) (\s@ListMetricsResponse' {} a -> s {nextToken = a} :: ListMetricsResponse)

-- | The metrics that match your request.
listMetricsResponse_metrics :: Lens.Lens' ListMetricsResponse (Core.Maybe [Metric])
listMetricsResponse_metrics = Lens.lens (\ListMetricsResponse' {metrics} -> metrics) (\s@ListMetricsResponse' {} a -> s {metrics = a} :: ListMetricsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMetricsResponse_httpStatus :: Lens.Lens' ListMetricsResponse Core.Int
listMetricsResponse_httpStatus = Lens.lens (\ListMetricsResponse' {httpStatus} -> httpStatus) (\s@ListMetricsResponse' {} a -> s {httpStatus = a} :: ListMetricsResponse)

instance Core.NFData ListMetricsResponse
