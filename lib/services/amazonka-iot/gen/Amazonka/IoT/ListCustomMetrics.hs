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
-- Module      : Amazonka.IoT.ListCustomMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Device Defender detect custom metrics.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListCustomMetrics>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListCustomMetrics
  ( -- * Creating a Request
    ListCustomMetrics (..),
    newListCustomMetrics,

    -- * Request Lenses
    listCustomMetrics_nextToken,
    listCustomMetrics_maxResults,

    -- * Destructuring the Response
    ListCustomMetricsResponse (..),
    newListCustomMetricsResponse,

    -- * Response Lenses
    listCustomMetricsResponse_nextToken,
    listCustomMetricsResponse_metricNames,
    listCustomMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomMetrics' smart constructor.
data ListCustomMetrics = ListCustomMetrics'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomMetrics_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listCustomMetrics_maxResults' - The maximum number of results to return at one time. The default is 25.
newListCustomMetrics ::
  ListCustomMetrics
newListCustomMetrics =
  ListCustomMetrics'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results.
listCustomMetrics_nextToken :: Lens.Lens' ListCustomMetrics (Prelude.Maybe Prelude.Text)
listCustomMetrics_nextToken = Lens.lens (\ListCustomMetrics' {nextToken} -> nextToken) (\s@ListCustomMetrics' {} a -> s {nextToken = a} :: ListCustomMetrics)

-- | The maximum number of results to return at one time. The default is 25.
listCustomMetrics_maxResults :: Lens.Lens' ListCustomMetrics (Prelude.Maybe Prelude.Natural)
listCustomMetrics_maxResults = Lens.lens (\ListCustomMetrics' {maxResults} -> maxResults) (\s@ListCustomMetrics' {} a -> s {maxResults = a} :: ListCustomMetrics)

instance Core.AWSPager ListCustomMetrics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomMetricsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomMetricsResponse_metricNames
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCustomMetrics_nextToken
          Lens..~ rs
          Lens.^? listCustomMetricsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCustomMetrics where
  type
    AWSResponse ListCustomMetrics =
      ListCustomMetricsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomMetricsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "metricNames" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCustomMetrics where
  hashWithSalt _salt ListCustomMetrics' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCustomMetrics where
  rnf ListCustomMetrics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListCustomMetrics where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListCustomMetrics where
  toPath = Prelude.const "/custom-metrics"

instance Core.ToQuery ListCustomMetrics where
  toQuery ListCustomMetrics' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCustomMetricsResponse' smart constructor.
data ListCustomMetricsResponse = ListCustomMetricsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom metric.
    metricNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomMetricsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'metricNames', 'listCustomMetricsResponse_metricNames' - The name of the custom metric.
--
-- 'httpStatus', 'listCustomMetricsResponse_httpStatus' - The response's http status code.
newListCustomMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomMetricsResponse
newListCustomMetricsResponse pHttpStatus_ =
  ListCustomMetricsResponse'
    { nextToken =
        Prelude.Nothing,
      metricNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listCustomMetricsResponse_nextToken :: Lens.Lens' ListCustomMetricsResponse (Prelude.Maybe Prelude.Text)
listCustomMetricsResponse_nextToken = Lens.lens (\ListCustomMetricsResponse' {nextToken} -> nextToken) (\s@ListCustomMetricsResponse' {} a -> s {nextToken = a} :: ListCustomMetricsResponse)

-- | The name of the custom metric.
listCustomMetricsResponse_metricNames :: Lens.Lens' ListCustomMetricsResponse (Prelude.Maybe [Prelude.Text])
listCustomMetricsResponse_metricNames = Lens.lens (\ListCustomMetricsResponse' {metricNames} -> metricNames) (\s@ListCustomMetricsResponse' {} a -> s {metricNames = a} :: ListCustomMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCustomMetricsResponse_httpStatus :: Lens.Lens' ListCustomMetricsResponse Prelude.Int
listCustomMetricsResponse_httpStatus = Lens.lens (\ListCustomMetricsResponse' {httpStatus} -> httpStatus) (\s@ListCustomMetricsResponse' {} a -> s {httpStatus = a} :: ListCustomMetricsResponse)

instance Prelude.NFData ListCustomMetricsResponse where
  rnf ListCustomMetricsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf metricNames
      `Prelude.seq` Prelude.rnf httpStatus
