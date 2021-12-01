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
-- Module      : Amazonka.CloudWatch.ListMetrics
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
module Amazonka.CloudWatch.ListMetrics
  ( -- * Creating a Request
    ListMetrics (..),
    newListMetrics,

    -- * Request Lenses
    listMetrics_metricName,
    listMetrics_namespace,
    listMetrics_nextToken,
    listMetrics_recentlyActive,
    listMetrics_dimensions,

    -- * Destructuring the Response
    ListMetricsResponse (..),
    newListMetricsResponse,

    -- * Response Lenses
    listMetricsResponse_metrics,
    listMetricsResponse_nextToken,
    listMetricsResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMetrics' smart constructor.
data ListMetrics = ListMetrics'
  { -- | The name of the metric to filter against. Only the metrics with names
    -- that match exactly will be returned.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The metric namespace to filter against. Only the namespace that matches
    -- exactly will be returned.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The token returned by a previous call to indicate that there is more
    -- data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | To filter the results to show only metrics that have had data points
    -- published in the past three hours, specify this parameter with a value
    -- of @PT3H@. This is the only valid value for this parameter.
    --
    -- The results that are returned are an approximation of the value you
    -- specify. There is a low probability that the returned results include
    -- metrics with last published data as much as 40 minutes more than the
    -- specified time interval.
    recentlyActive :: Prelude.Maybe RecentlyActive,
    -- | The dimensions to filter against. Only the dimensions that match exactly
    -- will be returned.
    dimensions :: Prelude.Maybe [DimensionFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'listMetrics_metricName' - The name of the metric to filter against. Only the metrics with names
-- that match exactly will be returned.
--
-- 'namespace', 'listMetrics_namespace' - The metric namespace to filter against. Only the namespace that matches
-- exactly will be returned.
--
-- 'nextToken', 'listMetrics_nextToken' - The token returned by a previous call to indicate that there is more
-- data available.
--
-- 'recentlyActive', 'listMetrics_recentlyActive' - To filter the results to show only metrics that have had data points
-- published in the past three hours, specify this parameter with a value
-- of @PT3H@. This is the only valid value for this parameter.
--
-- The results that are returned are an approximation of the value you
-- specify. There is a low probability that the returned results include
-- metrics with last published data as much as 40 minutes more than the
-- specified time interval.
--
-- 'dimensions', 'listMetrics_dimensions' - The dimensions to filter against. Only the dimensions that match exactly
-- will be returned.
newListMetrics ::
  ListMetrics
newListMetrics =
  ListMetrics'
    { metricName = Prelude.Nothing,
      namespace = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      recentlyActive = Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | The name of the metric to filter against. Only the metrics with names
-- that match exactly will be returned.
listMetrics_metricName :: Lens.Lens' ListMetrics (Prelude.Maybe Prelude.Text)
listMetrics_metricName = Lens.lens (\ListMetrics' {metricName} -> metricName) (\s@ListMetrics' {} a -> s {metricName = a} :: ListMetrics)

-- | The metric namespace to filter against. Only the namespace that matches
-- exactly will be returned.
listMetrics_namespace :: Lens.Lens' ListMetrics (Prelude.Maybe Prelude.Text)
listMetrics_namespace = Lens.lens (\ListMetrics' {namespace} -> namespace) (\s@ListMetrics' {} a -> s {namespace = a} :: ListMetrics)

-- | The token returned by a previous call to indicate that there is more
-- data available.
listMetrics_nextToken :: Lens.Lens' ListMetrics (Prelude.Maybe Prelude.Text)
listMetrics_nextToken = Lens.lens (\ListMetrics' {nextToken} -> nextToken) (\s@ListMetrics' {} a -> s {nextToken = a} :: ListMetrics)

-- | To filter the results to show only metrics that have had data points
-- published in the past three hours, specify this parameter with a value
-- of @PT3H@. This is the only valid value for this parameter.
--
-- The results that are returned are an approximation of the value you
-- specify. There is a low probability that the returned results include
-- metrics with last published data as much as 40 minutes more than the
-- specified time interval.
listMetrics_recentlyActive :: Lens.Lens' ListMetrics (Prelude.Maybe RecentlyActive)
listMetrics_recentlyActive = Lens.lens (\ListMetrics' {recentlyActive} -> recentlyActive) (\s@ListMetrics' {} a -> s {recentlyActive = a} :: ListMetrics)

-- | The dimensions to filter against. Only the dimensions that match exactly
-- will be returned.
listMetrics_dimensions :: Lens.Lens' ListMetrics (Prelude.Maybe [DimensionFilter])
listMetrics_dimensions = Lens.lens (\ListMetrics' {dimensions} -> dimensions) (\s@ListMetrics' {} a -> s {dimensions = a} :: ListMetrics) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListMetrics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMetricsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMetricsResponse_metrics Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMetrics_nextToken
          Lens..~ rs
          Lens.^? listMetricsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListMetrics where
  type AWSResponse ListMetrics = ListMetricsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListMetricsResult"
      ( \s h x ->
          ListMetricsResponse'
            Prelude.<$> ( x Core..@? "Metrics" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMetrics where
  hashWithSalt salt' ListMetrics' {..} =
    salt' `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` recentlyActive
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData ListMetrics where
  rnf ListMetrics' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf recentlyActive
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToHeaders ListMetrics where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListMetrics where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMetrics where
  toQuery ListMetrics' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListMetrics" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "MetricName" Core.=: metricName,
        "Namespace" Core.=: namespace,
        "NextToken" Core.=: nextToken,
        "RecentlyActive" Core.=: recentlyActive,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> dimensions)
      ]

-- | /See:/ 'newListMetricsResponse' smart constructor.
data ListMetricsResponse = ListMetricsResponse'
  { -- | The metrics that match your request.
    metrics :: Prelude.Maybe [Metric],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'listMetricsResponse_metrics' - The metrics that match your request.
--
-- 'nextToken', 'listMetricsResponse_nextToken' - The token that marks the start of the next batch of returned results.
--
-- 'httpStatus', 'listMetricsResponse_httpStatus' - The response's http status code.
newListMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMetricsResponse
newListMetricsResponse pHttpStatus_ =
  ListMetricsResponse'
    { metrics = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metrics that match your request.
listMetricsResponse_metrics :: Lens.Lens' ListMetricsResponse (Prelude.Maybe [Metric])
listMetricsResponse_metrics = Lens.lens (\ListMetricsResponse' {metrics} -> metrics) (\s@ListMetricsResponse' {} a -> s {metrics = a} :: ListMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that marks the start of the next batch of returned results.
listMetricsResponse_nextToken :: Lens.Lens' ListMetricsResponse (Prelude.Maybe Prelude.Text)
listMetricsResponse_nextToken = Lens.lens (\ListMetricsResponse' {nextToken} -> nextToken) (\s@ListMetricsResponse' {} a -> s {nextToken = a} :: ListMetricsResponse)

-- | The response's http status code.
listMetricsResponse_httpStatus :: Lens.Lens' ListMetricsResponse Prelude.Int
listMetricsResponse_httpStatus = Lens.lens (\ListMetricsResponse' {httpStatus} -> httpStatus) (\s@ListMetricsResponse' {} a -> s {httpStatus = a} :: ListMetricsResponse)

instance Prelude.NFData ListMetricsResponse where
  rnf ListMetricsResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
