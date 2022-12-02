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
-- Module      : Amazonka.Personalize.ListMetricAttributionMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the metrics for the metric attribution.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListMetricAttributionMetrics
  ( -- * Creating a Request
    ListMetricAttributionMetrics (..),
    newListMetricAttributionMetrics,

    -- * Request Lenses
    listMetricAttributionMetrics_nextToken,
    listMetricAttributionMetrics_maxResults,
    listMetricAttributionMetrics_metricAttributionArn,

    -- * Destructuring the Response
    ListMetricAttributionMetricsResponse (..),
    newListMetricAttributionMetricsResponse,

    -- * Response Lenses
    listMetricAttributionMetricsResponse_nextToken,
    listMetricAttributionMetricsResponse_metrics,
    listMetricAttributionMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMetricAttributionMetrics' smart constructor.
data ListMetricAttributionMetrics = ListMetricAttributionMetrics'
  { -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of metrics to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the metric attribution to retrieve
    -- attributes for.
    metricAttributionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricAttributionMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMetricAttributionMetrics_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'maxResults', 'listMetricAttributionMetrics_maxResults' - The maximum number of metrics to return in one page of results.
--
-- 'metricAttributionArn', 'listMetricAttributionMetrics_metricAttributionArn' - The Amazon Resource Name (ARN) of the metric attribution to retrieve
-- attributes for.
newListMetricAttributionMetrics ::
  ListMetricAttributionMetrics
newListMetricAttributionMetrics =
  ListMetricAttributionMetrics'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      metricAttributionArn = Prelude.Nothing
    }

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listMetricAttributionMetrics_nextToken :: Lens.Lens' ListMetricAttributionMetrics (Prelude.Maybe Prelude.Text)
listMetricAttributionMetrics_nextToken = Lens.lens (\ListMetricAttributionMetrics' {nextToken} -> nextToken) (\s@ListMetricAttributionMetrics' {} a -> s {nextToken = a} :: ListMetricAttributionMetrics)

-- | The maximum number of metrics to return in one page of results.
listMetricAttributionMetrics_maxResults :: Lens.Lens' ListMetricAttributionMetrics (Prelude.Maybe Prelude.Natural)
listMetricAttributionMetrics_maxResults = Lens.lens (\ListMetricAttributionMetrics' {maxResults} -> maxResults) (\s@ListMetricAttributionMetrics' {} a -> s {maxResults = a} :: ListMetricAttributionMetrics)

-- | The Amazon Resource Name (ARN) of the metric attribution to retrieve
-- attributes for.
listMetricAttributionMetrics_metricAttributionArn :: Lens.Lens' ListMetricAttributionMetrics (Prelude.Maybe Prelude.Text)
listMetricAttributionMetrics_metricAttributionArn = Lens.lens (\ListMetricAttributionMetrics' {metricAttributionArn} -> metricAttributionArn) (\s@ListMetricAttributionMetrics' {} a -> s {metricAttributionArn = a} :: ListMetricAttributionMetrics)

instance Core.AWSPager ListMetricAttributionMetrics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMetricAttributionMetricsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMetricAttributionMetricsResponse_metrics
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMetricAttributionMetrics_nextToken
          Lens..~ rs
          Lens.^? listMetricAttributionMetricsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMetricAttributionMetrics where
  type
    AWSResponse ListMetricAttributionMetrics =
      ListMetricAttributionMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMetricAttributionMetricsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "metrics" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListMetricAttributionMetrics
  where
  hashWithSalt _salt ListMetricAttributionMetrics' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` metricAttributionArn

instance Prelude.NFData ListMetricAttributionMetrics where
  rnf ListMetricAttributionMetrics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf metricAttributionArn

instance Data.ToHeaders ListMetricAttributionMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListMetricAttributionMetrics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMetricAttributionMetrics where
  toJSON ListMetricAttributionMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("metricAttributionArn" Data..=)
              Prelude.<$> metricAttributionArn
          ]
      )

instance Data.ToPath ListMetricAttributionMetrics where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMetricAttributionMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMetricAttributionMetricsResponse' smart constructor.
data ListMetricAttributionMetricsResponse = ListMetricAttributionMetricsResponse'
  { -- | Specify the pagination token from a previous
    -- @ListMetricAttributionMetricsResponse@ request to retrieve the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The metrics for the specified metric attribution.
    metrics :: Prelude.Maybe [MetricAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricAttributionMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMetricAttributionMetricsResponse_nextToken' - Specify the pagination token from a previous
-- @ListMetricAttributionMetricsResponse@ request to retrieve the next page
-- of results.
--
-- 'metrics', 'listMetricAttributionMetricsResponse_metrics' - The metrics for the specified metric attribution.
--
-- 'httpStatus', 'listMetricAttributionMetricsResponse_httpStatus' - The response's http status code.
newListMetricAttributionMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMetricAttributionMetricsResponse
newListMetricAttributionMetricsResponse pHttpStatus_ =
  ListMetricAttributionMetricsResponse'
    { nextToken =
        Prelude.Nothing,
      metrics = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specify the pagination token from a previous
-- @ListMetricAttributionMetricsResponse@ request to retrieve the next page
-- of results.
listMetricAttributionMetricsResponse_nextToken :: Lens.Lens' ListMetricAttributionMetricsResponse (Prelude.Maybe Prelude.Text)
listMetricAttributionMetricsResponse_nextToken = Lens.lens (\ListMetricAttributionMetricsResponse' {nextToken} -> nextToken) (\s@ListMetricAttributionMetricsResponse' {} a -> s {nextToken = a} :: ListMetricAttributionMetricsResponse)

-- | The metrics for the specified metric attribution.
listMetricAttributionMetricsResponse_metrics :: Lens.Lens' ListMetricAttributionMetricsResponse (Prelude.Maybe [MetricAttribute])
listMetricAttributionMetricsResponse_metrics = Lens.lens (\ListMetricAttributionMetricsResponse' {metrics} -> metrics) (\s@ListMetricAttributionMetricsResponse' {} a -> s {metrics = a} :: ListMetricAttributionMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMetricAttributionMetricsResponse_httpStatus :: Lens.Lens' ListMetricAttributionMetricsResponse Prelude.Int
listMetricAttributionMetricsResponse_httpStatus = Lens.lens (\ListMetricAttributionMetricsResponse' {httpStatus} -> httpStatus) (\s@ListMetricAttributionMetricsResponse' {} a -> s {httpStatus = a} :: ListMetricAttributionMetricsResponse)

instance
  Prelude.NFData
    ListMetricAttributionMetricsResponse
  where
  rnf ListMetricAttributionMetricsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf httpStatus
