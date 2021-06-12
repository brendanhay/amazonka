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
-- Module      : Network.AWS.IoT.ListCustomMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Device Defender detect custom metrics.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListCustomMetrics
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCustomMetrics' smart constructor.
data ListCustomMetrics = ListCustomMetrics'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results.
listCustomMetrics_nextToken :: Lens.Lens' ListCustomMetrics (Core.Maybe Core.Text)
listCustomMetrics_nextToken = Lens.lens (\ListCustomMetrics' {nextToken} -> nextToken) (\s@ListCustomMetrics' {} a -> s {nextToken = a} :: ListCustomMetrics)

-- | The maximum number of results to return at one time. The default is 25.
listCustomMetrics_maxResults :: Lens.Lens' ListCustomMetrics (Core.Maybe Core.Natural)
listCustomMetrics_maxResults = Lens.lens (\ListCustomMetrics' {maxResults} -> maxResults) (\s@ListCustomMetrics' {} a -> s {maxResults = a} :: ListCustomMetrics)

instance Core.AWSPager ListCustomMetrics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomMetricsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomMetricsResponse_metricNames
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCustomMetrics_nextToken
          Lens..~ rs
          Lens.^? listCustomMetricsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListCustomMetrics where
  type
    AWSResponse ListCustomMetrics =
      ListCustomMetricsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomMetricsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "metricNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCustomMetrics

instance Core.NFData ListCustomMetrics

instance Core.ToHeaders ListCustomMetrics where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListCustomMetrics where
  toPath = Core.const "/custom-metrics"

instance Core.ToQuery ListCustomMetrics where
  toQuery ListCustomMetrics' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCustomMetricsResponse' smart constructor.
data ListCustomMetricsResponse = ListCustomMetricsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the custom metric.
    metricNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListCustomMetricsResponse
newListCustomMetricsResponse pHttpStatus_ =
  ListCustomMetricsResponse'
    { nextToken =
        Core.Nothing,
      metricNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listCustomMetricsResponse_nextToken :: Lens.Lens' ListCustomMetricsResponse (Core.Maybe Core.Text)
listCustomMetricsResponse_nextToken = Lens.lens (\ListCustomMetricsResponse' {nextToken} -> nextToken) (\s@ListCustomMetricsResponse' {} a -> s {nextToken = a} :: ListCustomMetricsResponse)

-- | The name of the custom metric.
listCustomMetricsResponse_metricNames :: Lens.Lens' ListCustomMetricsResponse (Core.Maybe [Core.Text])
listCustomMetricsResponse_metricNames = Lens.lens (\ListCustomMetricsResponse' {metricNames} -> metricNames) (\s@ListCustomMetricsResponse' {} a -> s {metricNames = a} :: ListCustomMetricsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCustomMetricsResponse_httpStatus :: Lens.Lens' ListCustomMetricsResponse Core.Int
listCustomMetricsResponse_httpStatus = Lens.lens (\ListCustomMetricsResponse' {httpStatus} -> httpStatus) (\s@ListCustomMetricsResponse' {} a -> s {httpStatus = a} :: ListCustomMetricsResponse)

instance Core.NFData ListCustomMetricsResponse
