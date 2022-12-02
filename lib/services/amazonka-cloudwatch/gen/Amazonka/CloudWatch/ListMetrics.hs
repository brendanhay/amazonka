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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listMetrics_nextToken,
    listMetrics_dimensions,
    listMetrics_metricName,
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

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMetrics' smart constructor.
data ListMetrics = ListMetrics'
  { -- | The token returned by a previous call to indicate that there is more
    -- data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The dimensions to filter against. Only the dimensions that match exactly
    -- will be returned.
    dimensions :: Prelude.Maybe [DimensionFilter],
    -- | The name of the metric to filter against. Only the metrics with names
    -- that match exactly will be returned.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The metric namespace to filter against. Only the namespace that matches
    -- exactly will be returned.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | To filter the results to show only metrics that have had data points
    -- published in the past three hours, specify this parameter with a value
    -- of @PT3H@. This is the only valid value for this parameter.
    --
    -- The results that are returned are an approximation of the value you
    -- specify. There is a low probability that the returned results include
    -- metrics with last published data as much as 40 minutes more than the
    -- specified time interval.
    recentlyActive :: Prelude.Maybe RecentlyActive
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
-- 'nextToken', 'listMetrics_nextToken' - The token returned by a previous call to indicate that there is more
-- data available.
--
-- 'dimensions', 'listMetrics_dimensions' - The dimensions to filter against. Only the dimensions that match exactly
-- will be returned.
--
-- 'metricName', 'listMetrics_metricName' - The name of the metric to filter against. Only the metrics with names
-- that match exactly will be returned.
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
    { nextToken = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing,
      recentlyActive = Prelude.Nothing
    }

-- | The token returned by a previous call to indicate that there is more
-- data available.
listMetrics_nextToken :: Lens.Lens' ListMetrics (Prelude.Maybe Prelude.Text)
listMetrics_nextToken = Lens.lens (\ListMetrics' {nextToken} -> nextToken) (\s@ListMetrics' {} a -> s {nextToken = a} :: ListMetrics)

-- | The dimensions to filter against. Only the dimensions that match exactly
-- will be returned.
listMetrics_dimensions :: Lens.Lens' ListMetrics (Prelude.Maybe [DimensionFilter])
listMetrics_dimensions = Lens.lens (\ListMetrics' {dimensions} -> dimensions) (\s@ListMetrics' {} a -> s {dimensions = a} :: ListMetrics) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric to filter against. Only the metrics with names
-- that match exactly will be returned.
listMetrics_metricName :: Lens.Lens' ListMetrics (Prelude.Maybe Prelude.Text)
listMetrics_metricName = Lens.lens (\ListMetrics' {metricName} -> metricName) (\s@ListMetrics' {} a -> s {metricName = a} :: ListMetrics)

-- | The metric namespace to filter against. Only the namespace that matches
-- exactly will be returned.
listMetrics_namespace :: Lens.Lens' ListMetrics (Prelude.Maybe Prelude.Text)
listMetrics_namespace = Lens.lens (\ListMetrics' {namespace} -> namespace) (\s@ListMetrics' {} a -> s {namespace = a} :: ListMetrics)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListMetricsResult"
      ( \s h x ->
          ListMetricsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "Metrics" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMetrics where
  hashWithSalt _salt ListMetrics' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` recentlyActive

instance Prelude.NFData ListMetrics where
  rnf ListMetrics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf recentlyActive

instance Data.ToHeaders ListMetrics where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMetrics where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMetrics where
  toQuery ListMetrics' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListMetrics" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "MetricName" Data.=: metricName,
        "Namespace" Data.=: namespace,
        "RecentlyActive" Data.=: recentlyActive
      ]

-- | /See:/ 'newListMetricsResponse' smart constructor.
data ListMetricsResponse = ListMetricsResponse'
  { -- | The token that marks the start of the next batch of returned results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The metrics that match your request.
    metrics :: Prelude.Maybe [Metric],
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
-- 'nextToken', 'listMetricsResponse_nextToken' - The token that marks the start of the next batch of returned results.
--
-- 'metrics', 'listMetricsResponse_metrics' - The metrics that match your request.
--
-- 'httpStatus', 'listMetricsResponse_httpStatus' - The response's http status code.
newListMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMetricsResponse
newListMetricsResponse pHttpStatus_ =
  ListMetricsResponse'
    { nextToken = Prelude.Nothing,
      metrics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that marks the start of the next batch of returned results.
listMetricsResponse_nextToken :: Lens.Lens' ListMetricsResponse (Prelude.Maybe Prelude.Text)
listMetricsResponse_nextToken = Lens.lens (\ListMetricsResponse' {nextToken} -> nextToken) (\s@ListMetricsResponse' {} a -> s {nextToken = a} :: ListMetricsResponse)

-- | The metrics that match your request.
listMetricsResponse_metrics :: Lens.Lens' ListMetricsResponse (Prelude.Maybe [Metric])
listMetricsResponse_metrics = Lens.lens (\ListMetricsResponse' {metrics} -> metrics) (\s@ListMetricsResponse' {} a -> s {metrics = a} :: ListMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMetricsResponse_httpStatus :: Lens.Lens' ListMetricsResponse Prelude.Int
listMetricsResponse_httpStatus = Lens.lens (\ListMetricsResponse' {httpStatus} -> httpStatus) (\s@ListMetricsResponse' {} a -> s {httpStatus = a} :: ListMetricsResponse)

instance Prelude.NFData ListMetricsResponse where
  rnf ListMetricsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf httpStatus
