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
-- Module      : Amazonka.Wisdom.ListKnowledgeBases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the knowledge bases.
--
-- This operation returns paginated results.
module Amazonka.Wisdom.ListKnowledgeBases
  ( -- * Creating a Request
    ListKnowledgeBases (..),
    newListKnowledgeBases,

    -- * Request Lenses
    listKnowledgeBases_maxResults,
    listKnowledgeBases_nextToken,

    -- * Destructuring the Response
    ListKnowledgeBasesResponse (..),
    newListKnowledgeBasesResponse,

    -- * Response Lenses
    listKnowledgeBasesResponse_nextToken,
    listKnowledgeBasesResponse_httpStatus,
    listKnowledgeBasesResponse_knowledgeBaseSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newListKnowledgeBases' smart constructor.
data ListKnowledgeBases = ListKnowledgeBases'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKnowledgeBases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listKnowledgeBases_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listKnowledgeBases_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
newListKnowledgeBases ::
  ListKnowledgeBases
newListKnowledgeBases =
  ListKnowledgeBases'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return per page.
listKnowledgeBases_maxResults :: Lens.Lens' ListKnowledgeBases (Prelude.Maybe Prelude.Natural)
listKnowledgeBases_maxResults = Lens.lens (\ListKnowledgeBases' {maxResults} -> maxResults) (\s@ListKnowledgeBases' {} a -> s {maxResults = a} :: ListKnowledgeBases)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listKnowledgeBases_nextToken :: Lens.Lens' ListKnowledgeBases (Prelude.Maybe Prelude.Text)
listKnowledgeBases_nextToken = Lens.lens (\ListKnowledgeBases' {nextToken} -> nextToken) (\s@ListKnowledgeBases' {} a -> s {nextToken = a} :: ListKnowledgeBases)

instance Core.AWSPager ListKnowledgeBases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKnowledgeBasesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listKnowledgeBasesResponse_knowledgeBaseSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listKnowledgeBases_nextToken
          Lens..~ rs
          Lens.^? listKnowledgeBasesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListKnowledgeBases where
  type
    AWSResponse ListKnowledgeBases =
      ListKnowledgeBasesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKnowledgeBasesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "knowledgeBaseSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListKnowledgeBases where
  hashWithSalt _salt ListKnowledgeBases' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListKnowledgeBases where
  rnf ListKnowledgeBases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListKnowledgeBases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKnowledgeBases where
  toPath = Prelude.const "/knowledgeBases"

instance Data.ToQuery ListKnowledgeBases where
  toQuery ListKnowledgeBases' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListKnowledgeBasesResponse' smart constructor.
data ListKnowledgeBasesResponse = ListKnowledgeBasesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the knowledge bases.
    knowledgeBaseSummaries :: [KnowledgeBaseSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKnowledgeBasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKnowledgeBasesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listKnowledgeBasesResponse_httpStatus' - The response's http status code.
--
-- 'knowledgeBaseSummaries', 'listKnowledgeBasesResponse_knowledgeBaseSummaries' - Information about the knowledge bases.
newListKnowledgeBasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKnowledgeBasesResponse
newListKnowledgeBasesResponse pHttpStatus_ =
  ListKnowledgeBasesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      knowledgeBaseSummaries = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listKnowledgeBasesResponse_nextToken :: Lens.Lens' ListKnowledgeBasesResponse (Prelude.Maybe Prelude.Text)
listKnowledgeBasesResponse_nextToken = Lens.lens (\ListKnowledgeBasesResponse' {nextToken} -> nextToken) (\s@ListKnowledgeBasesResponse' {} a -> s {nextToken = a} :: ListKnowledgeBasesResponse)

-- | The response's http status code.
listKnowledgeBasesResponse_httpStatus :: Lens.Lens' ListKnowledgeBasesResponse Prelude.Int
listKnowledgeBasesResponse_httpStatus = Lens.lens (\ListKnowledgeBasesResponse' {httpStatus} -> httpStatus) (\s@ListKnowledgeBasesResponse' {} a -> s {httpStatus = a} :: ListKnowledgeBasesResponse)

-- | Information about the knowledge bases.
listKnowledgeBasesResponse_knowledgeBaseSummaries :: Lens.Lens' ListKnowledgeBasesResponse [KnowledgeBaseSummary]
listKnowledgeBasesResponse_knowledgeBaseSummaries = Lens.lens (\ListKnowledgeBasesResponse' {knowledgeBaseSummaries} -> knowledgeBaseSummaries) (\s@ListKnowledgeBasesResponse' {} a -> s {knowledgeBaseSummaries = a} :: ListKnowledgeBasesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListKnowledgeBasesResponse where
  rnf ListKnowledgeBasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf knowledgeBaseSummaries
