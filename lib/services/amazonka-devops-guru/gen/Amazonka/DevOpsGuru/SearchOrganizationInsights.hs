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
-- Module      : Amazonka.DevOpsGuru.SearchOrganizationInsights
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of insights in your organization. You can specify which
-- insights are returned by their start time, one or more statuses
-- (@ONGOING@, @CLOSED@, and @CLOSED@), one or more severities (@LOW@,
-- @MEDIUM@, and @HIGH@), and type (@REACTIVE@ or @PROACTIVE@).
--
-- Use the @Filters@ parameter to specify status and severity search
-- parameters. Use the @Type@ parameter to specify @REACTIVE@ or
-- @PROACTIVE@ in your search.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.SearchOrganizationInsights
  ( -- * Creating a Request
    SearchOrganizationInsights (..),
    newSearchOrganizationInsights,

    -- * Request Lenses
    searchOrganizationInsights_nextToken,
    searchOrganizationInsights_filters,
    searchOrganizationInsights_maxResults,
    searchOrganizationInsights_accountIds,
    searchOrganizationInsights_startTimeRange,
    searchOrganizationInsights_type,

    -- * Destructuring the Response
    SearchOrganizationInsightsResponse (..),
    newSearchOrganizationInsightsResponse,

    -- * Response Lenses
    searchOrganizationInsightsResponse_nextToken,
    searchOrganizationInsightsResponse_reactiveInsights,
    searchOrganizationInsightsResponse_proactiveInsights,
    searchOrganizationInsightsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchOrganizationInsights' smart constructor.
data SearchOrganizationInsights = SearchOrganizationInsights'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A @SearchOrganizationInsightsFilters@ object that is used to set the
    -- severity and status filters on your insight search.
    filters :: Prelude.Maybe SearchOrganizationInsightsFilters,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account.
    accountIds :: Prelude.NonEmpty Prelude.Text,
    startTimeRange :: StartTimeRange,
    -- | The type of insights you are searching for (@REACTIVE@ or @PROACTIVE@).
    type' :: InsightType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchOrganizationInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchOrganizationInsights_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'filters', 'searchOrganizationInsights_filters' - A @SearchOrganizationInsightsFilters@ object that is used to set the
-- severity and status filters on your insight search.
--
-- 'maxResults', 'searchOrganizationInsights_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'accountIds', 'searchOrganizationInsights_accountIds' - The ID of the Amazon Web Services account.
--
-- 'startTimeRange', 'searchOrganizationInsights_startTimeRange' - Undocumented member.
--
-- 'type'', 'searchOrganizationInsights_type' - The type of insights you are searching for (@REACTIVE@ or @PROACTIVE@).
newSearchOrganizationInsights ::
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'startTimeRange'
  StartTimeRange ->
  -- | 'type''
  InsightType ->
  SearchOrganizationInsights
newSearchOrganizationInsights
  pAccountIds_
  pStartTimeRange_
  pType_ =
    SearchOrganizationInsights'
      { nextToken =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        accountIds = Lens.coerced Lens.# pAccountIds_,
        startTimeRange = pStartTimeRange_,
        type' = pType_
      }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
searchOrganizationInsights_nextToken :: Lens.Lens' SearchOrganizationInsights (Prelude.Maybe Prelude.Text)
searchOrganizationInsights_nextToken = Lens.lens (\SearchOrganizationInsights' {nextToken} -> nextToken) (\s@SearchOrganizationInsights' {} a -> s {nextToken = a} :: SearchOrganizationInsights)

-- | A @SearchOrganizationInsightsFilters@ object that is used to set the
-- severity and status filters on your insight search.
searchOrganizationInsights_filters :: Lens.Lens' SearchOrganizationInsights (Prelude.Maybe SearchOrganizationInsightsFilters)
searchOrganizationInsights_filters = Lens.lens (\SearchOrganizationInsights' {filters} -> filters) (\s@SearchOrganizationInsights' {} a -> s {filters = a} :: SearchOrganizationInsights)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
searchOrganizationInsights_maxResults :: Lens.Lens' SearchOrganizationInsights (Prelude.Maybe Prelude.Natural)
searchOrganizationInsights_maxResults = Lens.lens (\SearchOrganizationInsights' {maxResults} -> maxResults) (\s@SearchOrganizationInsights' {} a -> s {maxResults = a} :: SearchOrganizationInsights)

-- | The ID of the Amazon Web Services account.
searchOrganizationInsights_accountIds :: Lens.Lens' SearchOrganizationInsights (Prelude.NonEmpty Prelude.Text)
searchOrganizationInsights_accountIds = Lens.lens (\SearchOrganizationInsights' {accountIds} -> accountIds) (\s@SearchOrganizationInsights' {} a -> s {accountIds = a} :: SearchOrganizationInsights) Prelude.. Lens.coerced

-- | Undocumented member.
searchOrganizationInsights_startTimeRange :: Lens.Lens' SearchOrganizationInsights StartTimeRange
searchOrganizationInsights_startTimeRange = Lens.lens (\SearchOrganizationInsights' {startTimeRange} -> startTimeRange) (\s@SearchOrganizationInsights' {} a -> s {startTimeRange = a} :: SearchOrganizationInsights)

-- | The type of insights you are searching for (@REACTIVE@ or @PROACTIVE@).
searchOrganizationInsights_type :: Lens.Lens' SearchOrganizationInsights InsightType
searchOrganizationInsights_type = Lens.lens (\SearchOrganizationInsights' {type'} -> type') (\s@SearchOrganizationInsights' {} a -> s {type' = a} :: SearchOrganizationInsights)

instance Core.AWSPager SearchOrganizationInsights where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchOrganizationInsightsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchOrganizationInsightsResponse_proactiveInsights
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchOrganizationInsightsResponse_reactiveInsights
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchOrganizationInsights_nextToken
          Lens..~ rs
          Lens.^? searchOrganizationInsightsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchOrganizationInsights where
  type
    AWSResponse SearchOrganizationInsights =
      SearchOrganizationInsightsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchOrganizationInsightsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ReactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "ProactiveInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchOrganizationInsights where
  hashWithSalt _salt SearchOrganizationInsights' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` startTimeRange
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SearchOrganizationInsights where
  rnf SearchOrganizationInsights' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf startTimeRange
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders SearchOrganizationInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchOrganizationInsights where
  toJSON SearchOrganizationInsights' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("AccountIds" Data..= accountIds),
            Prelude.Just
              ("StartTimeRange" Data..= startTimeRange),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath SearchOrganizationInsights where
  toPath =
    Prelude.const "/organization/insights/search"

instance Data.ToQuery SearchOrganizationInsights where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchOrganizationInsightsResponse' smart constructor.
data SearchOrganizationInsightsResponse = SearchOrganizationInsightsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An integer that specifies the number of open reactive insights in your
    -- Amazon Web Services account.
    reactiveInsights :: Prelude.Maybe [ReactiveInsightSummary],
    -- | An integer that specifies the number of open proactive insights in your
    -- Amazon Web Services account.
    proactiveInsights :: Prelude.Maybe [ProactiveInsightSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchOrganizationInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchOrganizationInsightsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'reactiveInsights', 'searchOrganizationInsightsResponse_reactiveInsights' - An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account.
--
-- 'proactiveInsights', 'searchOrganizationInsightsResponse_proactiveInsights' - An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account.
--
-- 'httpStatus', 'searchOrganizationInsightsResponse_httpStatus' - The response's http status code.
newSearchOrganizationInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchOrganizationInsightsResponse
newSearchOrganizationInsightsResponse pHttpStatus_ =
  SearchOrganizationInsightsResponse'
    { nextToken =
        Prelude.Nothing,
      reactiveInsights = Prelude.Nothing,
      proactiveInsights = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
searchOrganizationInsightsResponse_nextToken :: Lens.Lens' SearchOrganizationInsightsResponse (Prelude.Maybe Prelude.Text)
searchOrganizationInsightsResponse_nextToken = Lens.lens (\SearchOrganizationInsightsResponse' {nextToken} -> nextToken) (\s@SearchOrganizationInsightsResponse' {} a -> s {nextToken = a} :: SearchOrganizationInsightsResponse)

-- | An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account.
searchOrganizationInsightsResponse_reactiveInsights :: Lens.Lens' SearchOrganizationInsightsResponse (Prelude.Maybe [ReactiveInsightSummary])
searchOrganizationInsightsResponse_reactiveInsights = Lens.lens (\SearchOrganizationInsightsResponse' {reactiveInsights} -> reactiveInsights) (\s@SearchOrganizationInsightsResponse' {} a -> s {reactiveInsights = a} :: SearchOrganizationInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account.
searchOrganizationInsightsResponse_proactiveInsights :: Lens.Lens' SearchOrganizationInsightsResponse (Prelude.Maybe [ProactiveInsightSummary])
searchOrganizationInsightsResponse_proactiveInsights = Lens.lens (\SearchOrganizationInsightsResponse' {proactiveInsights} -> proactiveInsights) (\s@SearchOrganizationInsightsResponse' {} a -> s {proactiveInsights = a} :: SearchOrganizationInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchOrganizationInsightsResponse_httpStatus :: Lens.Lens' SearchOrganizationInsightsResponse Prelude.Int
searchOrganizationInsightsResponse_httpStatus = Lens.lens (\SearchOrganizationInsightsResponse' {httpStatus} -> httpStatus) (\s@SearchOrganizationInsightsResponse' {} a -> s {httpStatus = a} :: SearchOrganizationInsightsResponse)

instance
  Prelude.NFData
    SearchOrganizationInsightsResponse
  where
  rnf SearchOrganizationInsightsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reactiveInsights
      `Prelude.seq` Prelude.rnf proactiveInsights
      `Prelude.seq` Prelude.rnf httpStatus
