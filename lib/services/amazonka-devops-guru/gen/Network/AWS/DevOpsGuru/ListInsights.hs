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
-- Module      : Network.AWS.DevOpsGuru.ListInsights
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of insights in your AWS account. You can specify which
-- insights are returned by their start time and status (@ONGOING@,
-- @CLOSED@, or @ANY@).
--
-- This operation returns paginated results.
module Network.AWS.DevOpsGuru.ListInsights
  ( -- * Creating a Request
    ListInsights (..),
    newListInsights,

    -- * Request Lenses
    listInsights_nextToken,
    listInsights_maxResults,
    listInsights_statusFilter,

    -- * Destructuring the Response
    ListInsightsResponse (..),
    newListInsightsResponse,

    -- * Response Lenses
    listInsightsResponse_reactiveInsights,
    listInsightsResponse_nextToken,
    listInsightsResponse_proactiveInsights,
    listInsightsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListInsights' smart constructor.
data ListInsights = ListInsights'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter used to filter the returned insights by their status. You can
    -- specify one status filter.
    statusFilter :: ListInsightsStatusFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInsights_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'listInsights_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'statusFilter', 'listInsights_statusFilter' - A filter used to filter the returned insights by their status. You can
-- specify one status filter.
newListInsights ::
  -- | 'statusFilter'
  ListInsightsStatusFilter ->
  ListInsights
newListInsights pStatusFilter_ =
  ListInsights'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      statusFilter = pStatusFilter_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listInsights_nextToken :: Lens.Lens' ListInsights (Prelude.Maybe Prelude.Text)
listInsights_nextToken = Lens.lens (\ListInsights' {nextToken} -> nextToken) (\s@ListInsights' {} a -> s {nextToken = a} :: ListInsights)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listInsights_maxResults :: Lens.Lens' ListInsights (Prelude.Maybe Prelude.Natural)
listInsights_maxResults = Lens.lens (\ListInsights' {maxResults} -> maxResults) (\s@ListInsights' {} a -> s {maxResults = a} :: ListInsights)

-- | A filter used to filter the returned insights by their status. You can
-- specify one status filter.
listInsights_statusFilter :: Lens.Lens' ListInsights ListInsightsStatusFilter
listInsights_statusFilter = Lens.lens (\ListInsights' {statusFilter} -> statusFilter) (\s@ListInsights' {} a -> s {statusFilter = a} :: ListInsights)

instance Core.AWSPager ListInsights where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInsightsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInsightsResponse_proactiveInsights
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInsightsResponse_reactiveInsights
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInsights_nextToken
          Lens..~ rs
          Lens.^? listInsightsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListInsights where
  type AWSResponse ListInsights = ListInsightsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInsightsResponse'
            Prelude.<$> ( x Core..?> "ReactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ProactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInsights

instance Prelude.NFData ListInsights

instance Core.ToHeaders ListInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListInsights where
  toJSON ListInsights' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("StatusFilter" Core..= statusFilter)
          ]
      )

instance Core.ToPath ListInsights where
  toPath = Prelude.const "/insights"

instance Core.ToQuery ListInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInsightsResponse' smart constructor.
data ListInsightsResponse = ListInsightsResponse'
  { -- | The returned list of reactive insights.
    reactiveInsights :: Prelude.Maybe [ReactiveInsightSummary],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The returned list of proactive insights.
    proactiveInsights :: Prelude.Maybe [ProactiveInsightSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reactiveInsights', 'listInsightsResponse_reactiveInsights' - The returned list of reactive insights.
--
-- 'nextToken', 'listInsightsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'proactiveInsights', 'listInsightsResponse_proactiveInsights' - The returned list of proactive insights.
--
-- 'httpStatus', 'listInsightsResponse_httpStatus' - The response's http status code.
newListInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInsightsResponse
newListInsightsResponse pHttpStatus_ =
  ListInsightsResponse'
    { reactiveInsights =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      proactiveInsights = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned list of reactive insights.
listInsightsResponse_reactiveInsights :: Lens.Lens' ListInsightsResponse (Prelude.Maybe [ReactiveInsightSummary])
listInsightsResponse_reactiveInsights = Lens.lens (\ListInsightsResponse' {reactiveInsights} -> reactiveInsights) (\s@ListInsightsResponse' {} a -> s {reactiveInsights = a} :: ListInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listInsightsResponse_nextToken :: Lens.Lens' ListInsightsResponse (Prelude.Maybe Prelude.Text)
listInsightsResponse_nextToken = Lens.lens (\ListInsightsResponse' {nextToken} -> nextToken) (\s@ListInsightsResponse' {} a -> s {nextToken = a} :: ListInsightsResponse)

-- | The returned list of proactive insights.
listInsightsResponse_proactiveInsights :: Lens.Lens' ListInsightsResponse (Prelude.Maybe [ProactiveInsightSummary])
listInsightsResponse_proactiveInsights = Lens.lens (\ListInsightsResponse' {proactiveInsights} -> proactiveInsights) (\s@ListInsightsResponse' {} a -> s {proactiveInsights = a} :: ListInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInsightsResponse_httpStatus :: Lens.Lens' ListInsightsResponse Prelude.Int
listInsightsResponse_httpStatus = Lens.lens (\ListInsightsResponse' {httpStatus} -> httpStatus) (\s@ListInsightsResponse' {} a -> s {httpStatus = a} :: ListInsightsResponse)

instance Prelude.NFData ListInsightsResponse
