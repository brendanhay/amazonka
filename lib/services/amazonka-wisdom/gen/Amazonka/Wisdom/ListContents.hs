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
-- Module      : Amazonka.Wisdom.ListContents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the content.
--
-- This operation returns paginated results.
module Amazonka.Wisdom.ListContents
  ( -- * Creating a Request
    ListContents (..),
    newListContents,

    -- * Request Lenses
    listContents_maxResults,
    listContents_nextToken,
    listContents_knowledgeBaseId,

    -- * Destructuring the Response
    ListContentsResponse (..),
    newListContentsResponse,

    -- * Response Lenses
    listContentsResponse_nextToken,
    listContentsResponse_httpStatus,
    listContentsResponse_contentSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newListContents' smart constructor.
data ListContents = ListContents'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listContents_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listContents_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'knowledgeBaseId', 'listContents_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newListContents ::
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  ListContents
newListContents pKnowledgeBaseId_ =
  ListContents'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      knowledgeBaseId = pKnowledgeBaseId_
    }

-- | The maximum number of results to return per page.
listContents_maxResults :: Lens.Lens' ListContents (Prelude.Maybe Prelude.Natural)
listContents_maxResults = Lens.lens (\ListContents' {maxResults} -> maxResults) (\s@ListContents' {} a -> s {maxResults = a} :: ListContents)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listContents_nextToken :: Lens.Lens' ListContents (Prelude.Maybe Prelude.Text)
listContents_nextToken = Lens.lens (\ListContents' {nextToken} -> nextToken) (\s@ListContents' {} a -> s {nextToken = a} :: ListContents)

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
listContents_knowledgeBaseId :: Lens.Lens' ListContents Prelude.Text
listContents_knowledgeBaseId = Lens.lens (\ListContents' {knowledgeBaseId} -> knowledgeBaseId) (\s@ListContents' {} a -> s {knowledgeBaseId = a} :: ListContents)

instance Core.AWSPager ListContents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listContentsResponse_contentSummaries) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listContents_nextToken
          Lens..~ rs
          Lens.^? listContentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListContents where
  type AWSResponse ListContents = ListContentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "contentSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListContents where
  hashWithSalt _salt ListContents' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData ListContents where
  rnf ListContents' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf knowledgeBaseId

instance Data.ToHeaders ListContents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListContents where
  toPath ListContents' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/contents"
      ]

instance Data.ToQuery ListContents where
  toQuery ListContents' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListContentsResponse' smart constructor.
data ListContentsResponse = ListContentsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the content.
    contentSummaries :: [ContentSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContentsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listContentsResponse_httpStatus' - The response's http status code.
--
-- 'contentSummaries', 'listContentsResponse_contentSummaries' - Information about the content.
newListContentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContentsResponse
newListContentsResponse pHttpStatus_ =
  ListContentsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      contentSummaries = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listContentsResponse_nextToken :: Lens.Lens' ListContentsResponse (Prelude.Maybe Prelude.Text)
listContentsResponse_nextToken = Lens.lens (\ListContentsResponse' {nextToken} -> nextToken) (\s@ListContentsResponse' {} a -> s {nextToken = a} :: ListContentsResponse)

-- | The response's http status code.
listContentsResponse_httpStatus :: Lens.Lens' ListContentsResponse Prelude.Int
listContentsResponse_httpStatus = Lens.lens (\ListContentsResponse' {httpStatus} -> httpStatus) (\s@ListContentsResponse' {} a -> s {httpStatus = a} :: ListContentsResponse)

-- | Information about the content.
listContentsResponse_contentSummaries :: Lens.Lens' ListContentsResponse [ContentSummary]
listContentsResponse_contentSummaries = Lens.lens (\ListContentsResponse' {contentSummaries} -> contentSummaries) (\s@ListContentsResponse' {} a -> s {contentSummaries = a} :: ListContentsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListContentsResponse where
  rnf ListContentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf contentSummaries
