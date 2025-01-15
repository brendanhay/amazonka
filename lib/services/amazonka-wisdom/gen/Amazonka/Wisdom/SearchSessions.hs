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
-- Module      : Amazonka.Wisdom.SearchSessions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for sessions.
--
-- This operation returns paginated results.
module Amazonka.Wisdom.SearchSessions
  ( -- * Creating a Request
    SearchSessions (..),
    newSearchSessions,

    -- * Request Lenses
    searchSessions_maxResults,
    searchSessions_nextToken,
    searchSessions_assistantId,
    searchSessions_searchExpression,

    -- * Destructuring the Response
    SearchSessionsResponse (..),
    newSearchSessionsResponse,

    -- * Response Lenses
    searchSessionsResponse_nextToken,
    searchSessionsResponse_httpStatus,
    searchSessionsResponse_sessionSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newSearchSessions' smart constructor.
data SearchSessions = SearchSessions'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text,
    -- | The search expression to filter results.
    searchExpression :: SearchExpression
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchSessions_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchSessions_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'assistantId', 'searchSessions_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'searchExpression', 'searchSessions_searchExpression' - The search expression to filter results.
newSearchSessions ::
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'searchExpression'
  SearchExpression ->
  SearchSessions
newSearchSessions pAssistantId_ pSearchExpression_ =
  SearchSessions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assistantId = pAssistantId_,
      searchExpression = pSearchExpression_
    }

-- | The maximum number of results to return per page.
searchSessions_maxResults :: Lens.Lens' SearchSessions (Prelude.Maybe Prelude.Natural)
searchSessions_maxResults = Lens.lens (\SearchSessions' {maxResults} -> maxResults) (\s@SearchSessions' {} a -> s {maxResults = a} :: SearchSessions)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchSessions_nextToken :: Lens.Lens' SearchSessions (Prelude.Maybe Prelude.Text)
searchSessions_nextToken = Lens.lens (\SearchSessions' {nextToken} -> nextToken) (\s@SearchSessions' {} a -> s {nextToken = a} :: SearchSessions)

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
searchSessions_assistantId :: Lens.Lens' SearchSessions Prelude.Text
searchSessions_assistantId = Lens.lens (\SearchSessions' {assistantId} -> assistantId) (\s@SearchSessions' {} a -> s {assistantId = a} :: SearchSessions)

-- | The search expression to filter results.
searchSessions_searchExpression :: Lens.Lens' SearchSessions SearchExpression
searchSessions_searchExpression = Lens.lens (\SearchSessions' {searchExpression} -> searchExpression) (\s@SearchSessions' {} a -> s {searchExpression = a} :: SearchSessions)

instance Core.AWSPager SearchSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchSessionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. searchSessionsResponse_sessionSummaries) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& searchSessions_nextToken
              Lens..~ rs
              Lens.^? searchSessionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest SearchSessions where
  type
    AWSResponse SearchSessions =
      SearchSessionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchSessionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "sessionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable SearchSessions where
  hashWithSalt _salt SearchSessions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` searchExpression

instance Prelude.NFData SearchSessions where
  rnf SearchSessions' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf assistantId `Prelude.seq`
          Prelude.rnf searchExpression

instance Data.ToHeaders SearchSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchSessions where
  toJSON SearchSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("searchExpression" Data..= searchExpression)
          ]
      )

instance Data.ToPath SearchSessions where
  toPath SearchSessions' {..} =
    Prelude.mconcat
      [ "/assistants/",
        Data.toBS assistantId,
        "/searchSessions"
      ]

instance Data.ToQuery SearchSessions where
  toQuery SearchSessions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newSearchSessionsResponse' smart constructor.
data SearchSessionsResponse = SearchSessionsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary information about the sessions.
    sessionSummaries :: [SessionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchSessionsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'searchSessionsResponse_httpStatus' - The response's http status code.
--
-- 'sessionSummaries', 'searchSessionsResponse_sessionSummaries' - Summary information about the sessions.
newSearchSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchSessionsResponse
newSearchSessionsResponse pHttpStatus_ =
  SearchSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      sessionSummaries = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
searchSessionsResponse_nextToken :: Lens.Lens' SearchSessionsResponse (Prelude.Maybe Prelude.Text)
searchSessionsResponse_nextToken = Lens.lens (\SearchSessionsResponse' {nextToken} -> nextToken) (\s@SearchSessionsResponse' {} a -> s {nextToken = a} :: SearchSessionsResponse)

-- | The response's http status code.
searchSessionsResponse_httpStatus :: Lens.Lens' SearchSessionsResponse Prelude.Int
searchSessionsResponse_httpStatus = Lens.lens (\SearchSessionsResponse' {httpStatus} -> httpStatus) (\s@SearchSessionsResponse' {} a -> s {httpStatus = a} :: SearchSessionsResponse)

-- | Summary information about the sessions.
searchSessionsResponse_sessionSummaries :: Lens.Lens' SearchSessionsResponse [SessionSummary]
searchSessionsResponse_sessionSummaries = Lens.lens (\SearchSessionsResponse' {sessionSummaries} -> sessionSummaries) (\s@SearchSessionsResponse' {} a -> s {sessionSummaries = a} :: SearchSessionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData SearchSessionsResponse where
  rnf SearchSessionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf sessionSummaries
