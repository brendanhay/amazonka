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
-- Module      : Amazonka.DevOpsGuru.ListInsights
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of insights in your Amazon Web Services account. You can
-- specify which insights are returned by their start time and status
-- (@ONGOING@, @CLOSED@, or @ANY@).
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.ListInsights
  ( -- * Creating a Request
    ListInsights (..),
    newListInsights,

    -- * Request Lenses
    listInsights_maxResults,
    listInsights_nextToken,
    listInsights_statusFilter,

    -- * Destructuring the Response
    ListInsightsResponse (..),
    newListInsightsResponse,

    -- * Response Lenses
    listInsightsResponse_nextToken,
    listInsightsResponse_proactiveInsights,
    listInsightsResponse_reactiveInsights,
    listInsightsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInsights' smart constructor.
data ListInsights = ListInsights'
  { -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listInsights_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listInsights_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'statusFilter', 'listInsights_statusFilter' - A filter used to filter the returned insights by their status. You can
-- specify one status filter.
newListInsights ::
  -- | 'statusFilter'
  ListInsightsStatusFilter ->
  ListInsights
newListInsights pStatusFilter_ =
  ListInsights'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusFilter = pStatusFilter_
    }

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listInsights_maxResults :: Lens.Lens' ListInsights (Prelude.Maybe Prelude.Natural)
listInsights_maxResults = Lens.lens (\ListInsights' {maxResults} -> maxResults) (\s@ListInsights' {} a -> s {maxResults = a} :: ListInsights)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listInsights_nextToken :: Lens.Lens' ListInsights (Prelude.Maybe Prelude.Text)
listInsights_nextToken = Lens.lens (\ListInsights' {nextToken} -> nextToken) (\s@ListInsights' {} a -> s {nextToken = a} :: ListInsights)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInsightsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ProactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "ReactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInsights where
  hashWithSalt _salt ListInsights' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` statusFilter

instance Prelude.NFData ListInsights where
  rnf ListInsights' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statusFilter

instance Data.ToHeaders ListInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInsights where
  toJSON ListInsights' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("StatusFilter" Data..= statusFilter)
          ]
      )

instance Data.ToPath ListInsights where
  toPath = Prelude.const "/insights"

instance Data.ToQuery ListInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInsightsResponse' smart constructor.
data ListInsightsResponse = ListInsightsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The returned list of proactive insights.
    proactiveInsights :: Prelude.Maybe [ProactiveInsightSummary],
    -- | The returned list of reactive insights.
    reactiveInsights :: Prelude.Maybe [ReactiveInsightSummary],
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
-- 'nextToken', 'listInsightsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'proactiveInsights', 'listInsightsResponse_proactiveInsights' - The returned list of proactive insights.
--
-- 'reactiveInsights', 'listInsightsResponse_reactiveInsights' - The returned list of reactive insights.
--
-- 'httpStatus', 'listInsightsResponse_httpStatus' - The response's http status code.
newListInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInsightsResponse
newListInsightsResponse pHttpStatus_ =
  ListInsightsResponse'
    { nextToken = Prelude.Nothing,
      proactiveInsights = Prelude.Nothing,
      reactiveInsights = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listInsightsResponse_nextToken :: Lens.Lens' ListInsightsResponse (Prelude.Maybe Prelude.Text)
listInsightsResponse_nextToken = Lens.lens (\ListInsightsResponse' {nextToken} -> nextToken) (\s@ListInsightsResponse' {} a -> s {nextToken = a} :: ListInsightsResponse)

-- | The returned list of proactive insights.
listInsightsResponse_proactiveInsights :: Lens.Lens' ListInsightsResponse (Prelude.Maybe [ProactiveInsightSummary])
listInsightsResponse_proactiveInsights = Lens.lens (\ListInsightsResponse' {proactiveInsights} -> proactiveInsights) (\s@ListInsightsResponse' {} a -> s {proactiveInsights = a} :: ListInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The returned list of reactive insights.
listInsightsResponse_reactiveInsights :: Lens.Lens' ListInsightsResponse (Prelude.Maybe [ReactiveInsightSummary])
listInsightsResponse_reactiveInsights = Lens.lens (\ListInsightsResponse' {reactiveInsights} -> reactiveInsights) (\s@ListInsightsResponse' {} a -> s {reactiveInsights = a} :: ListInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInsightsResponse_httpStatus :: Lens.Lens' ListInsightsResponse Prelude.Int
listInsightsResponse_httpStatus = Lens.lens (\ListInsightsResponse' {httpStatus} -> httpStatus) (\s@ListInsightsResponse' {} a -> s {httpStatus = a} :: ListInsightsResponse)

instance Prelude.NFData ListInsightsResponse where
  rnf ListInsightsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf proactiveInsights
      `Prelude.seq` Prelude.rnf reactiveInsights
      `Prelude.seq` Prelude.rnf httpStatus
