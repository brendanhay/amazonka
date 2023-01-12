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
-- Module      : Amazonka.Wisdom.QueryAssistant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs a manual search against the specified assistant. To retrieve
-- recommendations for an assistant, use
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_GetRecommendations.html GetRecommendations>.
--
-- This operation returns paginated results.
module Amazonka.Wisdom.QueryAssistant
  ( -- * Creating a Request
    QueryAssistant (..),
    newQueryAssistant,

    -- * Request Lenses
    queryAssistant_maxResults,
    queryAssistant_nextToken,
    queryAssistant_assistantId,
    queryAssistant_queryText,

    -- * Destructuring the Response
    QueryAssistantResponse (..),
    newQueryAssistantResponse,

    -- * Response Lenses
    queryAssistantResponse_nextToken,
    queryAssistantResponse_httpStatus,
    queryAssistantResponse_results,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newQueryAssistant' smart constructor.
data QueryAssistant = QueryAssistant'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text,
    -- | The text to search for.
    queryText :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryAssistant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'queryAssistant_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'queryAssistant_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'assistantId', 'queryAssistant_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'queryText', 'queryAssistant_queryText' - The text to search for.
newQueryAssistant ::
  -- | 'assistantId'
  Prelude.Text ->
  -- | 'queryText'
  Prelude.Text ->
  QueryAssistant
newQueryAssistant pAssistantId_ pQueryText_ =
  QueryAssistant'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assistantId = pAssistantId_,
      queryText = Data._Sensitive Lens.# pQueryText_
    }

-- | The maximum number of results to return per page.
queryAssistant_maxResults :: Lens.Lens' QueryAssistant (Prelude.Maybe Prelude.Natural)
queryAssistant_maxResults = Lens.lens (\QueryAssistant' {maxResults} -> maxResults) (\s@QueryAssistant' {} a -> s {maxResults = a} :: QueryAssistant)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
queryAssistant_nextToken :: Lens.Lens' QueryAssistant (Prelude.Maybe Prelude.Text)
queryAssistant_nextToken = Lens.lens (\QueryAssistant' {nextToken} -> nextToken) (\s@QueryAssistant' {} a -> s {nextToken = a} :: QueryAssistant)

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
queryAssistant_assistantId :: Lens.Lens' QueryAssistant Prelude.Text
queryAssistant_assistantId = Lens.lens (\QueryAssistant' {assistantId} -> assistantId) (\s@QueryAssistant' {} a -> s {assistantId = a} :: QueryAssistant)

-- | The text to search for.
queryAssistant_queryText :: Lens.Lens' QueryAssistant Prelude.Text
queryAssistant_queryText = Lens.lens (\QueryAssistant' {queryText} -> queryText) (\s@QueryAssistant' {} a -> s {queryText = a} :: QueryAssistant) Prelude.. Data._Sensitive

instance Core.AWSPager QueryAssistant where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? queryAssistantResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. queryAssistantResponse_results) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& queryAssistant_nextToken
          Lens..~ rs
          Lens.^? queryAssistantResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest QueryAssistant where
  type
    AWSResponse QueryAssistant =
      QueryAssistantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryAssistantResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "results" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable QueryAssistant where
  hashWithSalt _salt QueryAssistant' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assistantId
      `Prelude.hashWithSalt` queryText

instance Prelude.NFData QueryAssistant where
  rnf QueryAssistant' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assistantId
      `Prelude.seq` Prelude.rnf queryText

instance Data.ToHeaders QueryAssistant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON QueryAssistant where
  toJSON QueryAssistant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("queryText" Data..= queryText)
          ]
      )

instance Data.ToPath QueryAssistant where
  toPath QueryAssistant' {..} =
    Prelude.mconcat
      ["/assistants/", Data.toBS assistantId, "/query"]

instance Data.ToQuery QueryAssistant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newQueryAssistantResponse' smart constructor.
data QueryAssistantResponse = QueryAssistantResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The results of the query.
    results :: [ResultData]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryAssistantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'queryAssistantResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'queryAssistantResponse_httpStatus' - The response's http status code.
--
-- 'results', 'queryAssistantResponse_results' - The results of the query.
newQueryAssistantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  QueryAssistantResponse
newQueryAssistantResponse pHttpStatus_ =
  QueryAssistantResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      results = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
queryAssistantResponse_nextToken :: Lens.Lens' QueryAssistantResponse (Prelude.Maybe Prelude.Text)
queryAssistantResponse_nextToken = Lens.lens (\QueryAssistantResponse' {nextToken} -> nextToken) (\s@QueryAssistantResponse' {} a -> s {nextToken = a} :: QueryAssistantResponse)

-- | The response's http status code.
queryAssistantResponse_httpStatus :: Lens.Lens' QueryAssistantResponse Prelude.Int
queryAssistantResponse_httpStatus = Lens.lens (\QueryAssistantResponse' {httpStatus} -> httpStatus) (\s@QueryAssistantResponse' {} a -> s {httpStatus = a} :: QueryAssistantResponse)

-- | The results of the query.
queryAssistantResponse_results :: Lens.Lens' QueryAssistantResponse [ResultData]
queryAssistantResponse_results = Lens.lens (\QueryAssistantResponse' {results} -> results) (\s@QueryAssistantResponse' {} a -> s {results = a} :: QueryAssistantResponse) Prelude.. Lens.coerced

instance Prelude.NFData QueryAssistantResponse where
  rnf QueryAssistantResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf results
