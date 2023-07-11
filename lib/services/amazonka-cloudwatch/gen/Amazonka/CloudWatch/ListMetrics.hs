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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the specified metrics. You can use the returned metrics with
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics>
-- to get statistical data.
--
-- Up to 500 results are returned for any one call. To retrieve additional
-- results, use the returned token with subsequent calls.
--
-- After you create a metric, allow up to 15 minutes for the metric to
-- appear. To see metric statistics sooner, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics>.
--
-- If you are using CloudWatch cross-account observability, you can use
-- this operation in a monitoring account and view metrics from the linked
-- source accounts. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Unified-Cross-Account.html CloudWatch cross-account observability>.
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
    listMetrics_dimensions,
    listMetrics_includeLinkedAccounts,
    listMetrics_metricName,
    listMetrics_namespace,
    listMetrics_nextToken,
    listMetrics_owningAccount,
    listMetrics_recentlyActive,

    -- * Destructuring the Response
    ListMetricsResponse (..),
    newListMetricsResponse,

    -- * Response Lenses
    listMetricsResponse_metrics,
    listMetricsResponse_nextToken,
    listMetricsResponse_owningAccounts,
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
  { -- | The dimensions to filter against. Only the dimensions that match exactly
    -- will be returned.
    dimensions :: Prelude.Maybe [DimensionFilter],
    -- | If you are using this operation in a monitoring account, specify @true@
    -- to include metrics from source accounts in the returned data.
    --
    -- The default is @false@.
    includeLinkedAccounts :: Prelude.Maybe Prelude.Bool,
    -- | The name of the metric to filter against. Only the metrics with names
    -- that match exactly will be returned.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The metric namespace to filter against. Only the namespace that matches
    -- exactly will be returned.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The token returned by a previous call to indicate that there is more
    -- data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When you use this operation in a monitoring account, use this field to
    -- return metrics only from one source account. To do so, specify that
    -- source account ID in this field, and also specify @true@ for
    -- @IncludeLinkedAccounts@.
    owningAccount :: Prelude.Maybe Prelude.Text,
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
-- 'dimensions', 'listMetrics_dimensions' - The dimensions to filter against. Only the dimensions that match exactly
-- will be returned.
--
-- 'includeLinkedAccounts', 'listMetrics_includeLinkedAccounts' - If you are using this operation in a monitoring account, specify @true@
-- to include metrics from source accounts in the returned data.
--
-- The default is @false@.
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
-- 'owningAccount', 'listMetrics_owningAccount' - When you use this operation in a monitoring account, use this field to
-- return metrics only from one source account. To do so, specify that
-- source account ID in this field, and also specify @true@ for
-- @IncludeLinkedAccounts@.
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
    { dimensions = Prelude.Nothing,
      includeLinkedAccounts = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      owningAccount = Prelude.Nothing,
      recentlyActive = Prelude.Nothing
    }

-- | The dimensions to filter against. Only the dimensions that match exactly
-- will be returned.
listMetrics_dimensions :: Lens.Lens' ListMetrics (Prelude.Maybe [DimensionFilter])
listMetrics_dimensions = Lens.lens (\ListMetrics' {dimensions} -> dimensions) (\s@ListMetrics' {} a -> s {dimensions = a} :: ListMetrics) Prelude.. Lens.mapping Lens.coerced

-- | If you are using this operation in a monitoring account, specify @true@
-- to include metrics from source accounts in the returned data.
--
-- The default is @false@.
listMetrics_includeLinkedAccounts :: Lens.Lens' ListMetrics (Prelude.Maybe Prelude.Bool)
listMetrics_includeLinkedAccounts = Lens.lens (\ListMetrics' {includeLinkedAccounts} -> includeLinkedAccounts) (\s@ListMetrics' {} a -> s {includeLinkedAccounts = a} :: ListMetrics)

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

-- | When you use this operation in a monitoring account, use this field to
-- return metrics only from one source account. To do so, specify that
-- source account ID in this field, and also specify @true@ for
-- @IncludeLinkedAccounts@.
listMetrics_owningAccount :: Lens.Lens' ListMetrics (Prelude.Maybe Prelude.Text)
listMetrics_owningAccount = Lens.lens (\ListMetrics' {owningAccount} -> owningAccount) (\s@ListMetrics' {} a -> s {owningAccount = a} :: ListMetrics)

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
            Lens.^? listMetricsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMetricsResponse_metrics
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMetricsResponse_owningAccounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listMetrics_nextToken
          Lens..~ rs
          Lens.^? listMetricsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListMetrics where
  type AWSResponse ListMetrics = ListMetricsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListMetricsResult"
      ( \s h x ->
          ListMetricsResponse'
            Prelude.<$> ( x
                            Data..@? "Metrics"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> ( x
                            Data..@? "OwningAccounts"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMetrics where
  hashWithSalt _salt ListMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` includeLinkedAccounts
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` owningAccount
      `Prelude.hashWithSalt` recentlyActive

instance Prelude.NFData ListMetrics where
  rnf ListMetrics' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf includeLinkedAccounts
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf owningAccount
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
        "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "IncludeLinkedAccounts"
          Data.=: includeLinkedAccounts,
        "MetricName" Data.=: metricName,
        "Namespace" Data.=: namespace,
        "NextToken" Data.=: nextToken,
        "OwningAccount" Data.=: owningAccount,
        "RecentlyActive" Data.=: recentlyActive
      ]

-- | /See:/ 'newListMetricsResponse' smart constructor.
data ListMetricsResponse = ListMetricsResponse'
  { -- | The metrics that match your request.
    metrics :: Prelude.Maybe [Metric],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If you are using this operation in a monitoring account, this array
    -- contains the account IDs of the source accounts where the metrics in the
    -- returned data are from.
    --
    -- This field is a 1:1 mapping between each metric that is returned and the
    -- ID of the owning account.
    owningAccounts :: Prelude.Maybe [Prelude.Text],
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
-- 'owningAccounts', 'listMetricsResponse_owningAccounts' - If you are using this operation in a monitoring account, this array
-- contains the account IDs of the source accounts where the metrics in the
-- returned data are from.
--
-- This field is a 1:1 mapping between each metric that is returned and the
-- ID of the owning account.
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
      owningAccounts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metrics that match your request.
listMetricsResponse_metrics :: Lens.Lens' ListMetricsResponse (Prelude.Maybe [Metric])
listMetricsResponse_metrics = Lens.lens (\ListMetricsResponse' {metrics} -> metrics) (\s@ListMetricsResponse' {} a -> s {metrics = a} :: ListMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that marks the start of the next batch of returned results.
listMetricsResponse_nextToken :: Lens.Lens' ListMetricsResponse (Prelude.Maybe Prelude.Text)
listMetricsResponse_nextToken = Lens.lens (\ListMetricsResponse' {nextToken} -> nextToken) (\s@ListMetricsResponse' {} a -> s {nextToken = a} :: ListMetricsResponse)

-- | If you are using this operation in a monitoring account, this array
-- contains the account IDs of the source accounts where the metrics in the
-- returned data are from.
--
-- This field is a 1:1 mapping between each metric that is returned and the
-- ID of the owning account.
listMetricsResponse_owningAccounts :: Lens.Lens' ListMetricsResponse (Prelude.Maybe [Prelude.Text])
listMetricsResponse_owningAccounts = Lens.lens (\ListMetricsResponse' {owningAccounts} -> owningAccounts) (\s@ListMetricsResponse' {} a -> s {owningAccounts = a} :: ListMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMetricsResponse_httpStatus :: Lens.Lens' ListMetricsResponse Prelude.Int
listMetricsResponse_httpStatus = Lens.lens (\ListMetricsResponse' {httpStatus} -> httpStatus) (\s@ListMetricsResponse' {} a -> s {httpStatus = a} :: ListMetricsResponse)

instance Prelude.NFData ListMetricsResponse where
  rnf ListMetricsResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf owningAccounts
      `Prelude.seq` Prelude.rnf httpStatus
