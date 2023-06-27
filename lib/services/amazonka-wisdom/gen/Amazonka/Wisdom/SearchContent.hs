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
-- Module      : Amazonka.Wisdom.SearchContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for content in a specified knowledge base. Can be used to get a
-- specific content resource by its name.
--
-- This operation returns paginated results.
module Amazonka.Wisdom.SearchContent
  ( -- * Creating a Request
    SearchContent (..),
    newSearchContent,

    -- * Request Lenses
    searchContent_maxResults,
    searchContent_nextToken,
    searchContent_knowledgeBaseId,
    searchContent_searchExpression,

    -- * Destructuring the Response
    SearchContentResponse (..),
    newSearchContentResponse,

    -- * Response Lenses
    searchContentResponse_nextToken,
    searchContentResponse_httpStatus,
    searchContentResponse_contentSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newSearchContent' smart constructor.
data SearchContent = SearchContent'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text,
    -- | The search expression to filter results.
    searchExpression :: SearchExpression
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchContent_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchContent_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'knowledgeBaseId', 'searchContent_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'searchExpression', 'searchContent_searchExpression' - The search expression to filter results.
newSearchContent ::
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  -- | 'searchExpression'
  SearchExpression ->
  SearchContent
newSearchContent pKnowledgeBaseId_ pSearchExpression_ =
  SearchContent'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      knowledgeBaseId = pKnowledgeBaseId_,
      searchExpression = pSearchExpression_
    }

-- | The maximum number of results to return per page.
searchContent_maxResults :: Lens.Lens' SearchContent (Prelude.Maybe Prelude.Natural)
searchContent_maxResults = Lens.lens (\SearchContent' {maxResults} -> maxResults) (\s@SearchContent' {} a -> s {maxResults = a} :: SearchContent)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchContent_nextToken :: Lens.Lens' SearchContent (Prelude.Maybe Prelude.Text)
searchContent_nextToken = Lens.lens (\SearchContent' {nextToken} -> nextToken) (\s@SearchContent' {} a -> s {nextToken = a} :: SearchContent)

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
searchContent_knowledgeBaseId :: Lens.Lens' SearchContent Prelude.Text
searchContent_knowledgeBaseId = Lens.lens (\SearchContent' {knowledgeBaseId} -> knowledgeBaseId) (\s@SearchContent' {} a -> s {knowledgeBaseId = a} :: SearchContent)

-- | The search expression to filter results.
searchContent_searchExpression :: Lens.Lens' SearchContent SearchExpression
searchContent_searchExpression = Lens.lens (\SearchContent' {searchExpression} -> searchExpression) (\s@SearchContent' {} a -> s {searchExpression = a} :: SearchContent)

instance Core.AWSPager SearchContent where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchContentResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. searchContentResponse_contentSummaries) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchContent_nextToken
          Lens..~ rs
          Lens.^? searchContentResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchContent where
  type
    AWSResponse SearchContent =
      SearchContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchContentResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "contentSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable SearchContent where
  hashWithSalt _salt SearchContent' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` knowledgeBaseId
      `Prelude.hashWithSalt` searchExpression

instance Prelude.NFData SearchContent where
  rnf SearchContent' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf knowledgeBaseId
      `Prelude.seq` Prelude.rnf searchExpression

instance Data.ToHeaders SearchContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchContent where
  toJSON SearchContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("searchExpression" Data..= searchExpression)
          ]
      )

instance Data.ToPath SearchContent where
  toPath SearchContent' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/search"
      ]

instance Data.ToQuery SearchContent where
  toQuery SearchContent' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newSearchContentResponse' smart constructor.
data SearchContentResponse = SearchContentResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary information about the content.
    contentSummaries :: [ContentSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchContentResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'searchContentResponse_httpStatus' - The response's http status code.
--
-- 'contentSummaries', 'searchContentResponse_contentSummaries' - Summary information about the content.
newSearchContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchContentResponse
newSearchContentResponse pHttpStatus_ =
  SearchContentResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      contentSummaries = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
searchContentResponse_nextToken :: Lens.Lens' SearchContentResponse (Prelude.Maybe Prelude.Text)
searchContentResponse_nextToken = Lens.lens (\SearchContentResponse' {nextToken} -> nextToken) (\s@SearchContentResponse' {} a -> s {nextToken = a} :: SearchContentResponse)

-- | The response's http status code.
searchContentResponse_httpStatus :: Lens.Lens' SearchContentResponse Prelude.Int
searchContentResponse_httpStatus = Lens.lens (\SearchContentResponse' {httpStatus} -> httpStatus) (\s@SearchContentResponse' {} a -> s {httpStatus = a} :: SearchContentResponse)

-- | Summary information about the content.
searchContentResponse_contentSummaries :: Lens.Lens' SearchContentResponse [ContentSummary]
searchContentResponse_contentSummaries = Lens.lens (\SearchContentResponse' {contentSummaries} -> contentSummaries) (\s@SearchContentResponse' {} a -> s {contentSummaries = a} :: SearchContentResponse) Prelude.. Lens.coerced

instance Prelude.NFData SearchContentResponse where
  rnf SearchContentResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf contentSummaries
