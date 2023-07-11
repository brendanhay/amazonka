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
-- Module      : Amazonka.DevOpsGuru.SearchInsights
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of insights in your Amazon Web Services account. You can
-- specify which insights are returned by their start time, one or more
-- statuses (@ONGOING@ or @CLOSED@), one or more severities (@LOW@,
-- @MEDIUM@, and @HIGH@), and type (@REACTIVE@ or @PROACTIVE@).
--
-- Use the @Filters@ parameter to specify status and severity search
-- parameters. Use the @Type@ parameter to specify @REACTIVE@ or
-- @PROACTIVE@ in your search.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.SearchInsights
  ( -- * Creating a Request
    SearchInsights (..),
    newSearchInsights,

    -- * Request Lenses
    searchInsights_filters,
    searchInsights_maxResults,
    searchInsights_nextToken,
    searchInsights_startTimeRange,
    searchInsights_type,

    -- * Destructuring the Response
    SearchInsightsResponse (..),
    newSearchInsightsResponse,

    -- * Response Lenses
    searchInsightsResponse_nextToken,
    searchInsightsResponse_proactiveInsights,
    searchInsightsResponse_reactiveInsights,
    searchInsightsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchInsights' smart constructor.
data SearchInsights = SearchInsights'
  { -- | A @SearchInsightsFilters@ object that is used to set the severity and
    -- status filters on your insight search.
    filters :: Prelude.Maybe SearchInsightsFilters,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The start of the time range passed in. Returned insights occurred after
    -- this time.
    startTimeRange :: StartTimeRange,
    -- | The type of insights you are searching for (@REACTIVE@ or @PROACTIVE@).
    type' :: InsightType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchInsights_filters' - A @SearchInsightsFilters@ object that is used to set the severity and
-- status filters on your insight search.
--
-- 'maxResults', 'searchInsights_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'searchInsights_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'startTimeRange', 'searchInsights_startTimeRange' - The start of the time range passed in. Returned insights occurred after
-- this time.
--
-- 'type'', 'searchInsights_type' - The type of insights you are searching for (@REACTIVE@ or @PROACTIVE@).
newSearchInsights ::
  -- | 'startTimeRange'
  StartTimeRange ->
  -- | 'type''
  InsightType ->
  SearchInsights
newSearchInsights pStartTimeRange_ pType_ =
  SearchInsights'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTimeRange = pStartTimeRange_,
      type' = pType_
    }

-- | A @SearchInsightsFilters@ object that is used to set the severity and
-- status filters on your insight search.
searchInsights_filters :: Lens.Lens' SearchInsights (Prelude.Maybe SearchInsightsFilters)
searchInsights_filters = Lens.lens (\SearchInsights' {filters} -> filters) (\s@SearchInsights' {} a -> s {filters = a} :: SearchInsights)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
searchInsights_maxResults :: Lens.Lens' SearchInsights (Prelude.Maybe Prelude.Natural)
searchInsights_maxResults = Lens.lens (\SearchInsights' {maxResults} -> maxResults) (\s@SearchInsights' {} a -> s {maxResults = a} :: SearchInsights)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
searchInsights_nextToken :: Lens.Lens' SearchInsights (Prelude.Maybe Prelude.Text)
searchInsights_nextToken = Lens.lens (\SearchInsights' {nextToken} -> nextToken) (\s@SearchInsights' {} a -> s {nextToken = a} :: SearchInsights)

-- | The start of the time range passed in. Returned insights occurred after
-- this time.
searchInsights_startTimeRange :: Lens.Lens' SearchInsights StartTimeRange
searchInsights_startTimeRange = Lens.lens (\SearchInsights' {startTimeRange} -> startTimeRange) (\s@SearchInsights' {} a -> s {startTimeRange = a} :: SearchInsights)

-- | The type of insights you are searching for (@REACTIVE@ or @PROACTIVE@).
searchInsights_type :: Lens.Lens' SearchInsights InsightType
searchInsights_type = Lens.lens (\SearchInsights' {type'} -> type') (\s@SearchInsights' {} a -> s {type' = a} :: SearchInsights)

instance Core.AWSPager SearchInsights where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchInsightsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchInsightsResponse_proactiveInsights
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchInsightsResponse_reactiveInsights
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchInsights_nextToken
          Lens..~ rs
          Lens.^? searchInsightsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchInsights where
  type
    AWSResponse SearchInsights =
      SearchInsightsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchInsightsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ProactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "ReactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchInsights where
  hashWithSalt _salt SearchInsights' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTimeRange
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SearchInsights where
  rnf SearchInsights' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTimeRange
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders SearchInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchInsights where
  toJSON SearchInsights' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("StartTimeRange" Data..= startTimeRange),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath SearchInsights where
  toPath = Prelude.const "/insights/search"

instance Data.ToQuery SearchInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchInsightsResponse' smart constructor.
data SearchInsightsResponse = SearchInsightsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The returned proactive insights.
    proactiveInsights :: Prelude.Maybe [ProactiveInsightSummary],
    -- | The returned reactive insights.
    reactiveInsights :: Prelude.Maybe [ReactiveInsightSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchInsightsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'proactiveInsights', 'searchInsightsResponse_proactiveInsights' - The returned proactive insights.
--
-- 'reactiveInsights', 'searchInsightsResponse_reactiveInsights' - The returned reactive insights.
--
-- 'httpStatus', 'searchInsightsResponse_httpStatus' - The response's http status code.
newSearchInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchInsightsResponse
newSearchInsightsResponse pHttpStatus_ =
  SearchInsightsResponse'
    { nextToken =
        Prelude.Nothing,
      proactiveInsights = Prelude.Nothing,
      reactiveInsights = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
searchInsightsResponse_nextToken :: Lens.Lens' SearchInsightsResponse (Prelude.Maybe Prelude.Text)
searchInsightsResponse_nextToken = Lens.lens (\SearchInsightsResponse' {nextToken} -> nextToken) (\s@SearchInsightsResponse' {} a -> s {nextToken = a} :: SearchInsightsResponse)

-- | The returned proactive insights.
searchInsightsResponse_proactiveInsights :: Lens.Lens' SearchInsightsResponse (Prelude.Maybe [ProactiveInsightSummary])
searchInsightsResponse_proactiveInsights = Lens.lens (\SearchInsightsResponse' {proactiveInsights} -> proactiveInsights) (\s@SearchInsightsResponse' {} a -> s {proactiveInsights = a} :: SearchInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The returned reactive insights.
searchInsightsResponse_reactiveInsights :: Lens.Lens' SearchInsightsResponse (Prelude.Maybe [ReactiveInsightSummary])
searchInsightsResponse_reactiveInsights = Lens.lens (\SearchInsightsResponse' {reactiveInsights} -> reactiveInsights) (\s@SearchInsightsResponse' {} a -> s {reactiveInsights = a} :: SearchInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchInsightsResponse_httpStatus :: Lens.Lens' SearchInsightsResponse Prelude.Int
searchInsightsResponse_httpStatus = Lens.lens (\SearchInsightsResponse' {httpStatus} -> httpStatus) (\s@SearchInsightsResponse' {} a -> s {httpStatus = a} :: SearchInsightsResponse)

instance Prelude.NFData SearchInsightsResponse where
  rnf SearchInsightsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf proactiveInsights
      `Prelude.seq` Prelude.rnf reactiveInsights
      `Prelude.seq` Prelude.rnf httpStatus
