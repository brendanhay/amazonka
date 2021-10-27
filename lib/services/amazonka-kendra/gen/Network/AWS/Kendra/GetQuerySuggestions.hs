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
-- Module      : Network.AWS.Kendra.GetQuerySuggestions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the queries that are suggested to your users.
module Network.AWS.Kendra.GetQuerySuggestions
  ( -- * Creating a Request
    GetQuerySuggestions (..),
    newGetQuerySuggestions,

    -- * Request Lenses
    getQuerySuggestions_maxSuggestionsCount,
    getQuerySuggestions_indexId,
    getQuerySuggestions_queryText,

    -- * Destructuring the Response
    GetQuerySuggestionsResponse (..),
    newGetQuerySuggestionsResponse,

    -- * Response Lenses
    getQuerySuggestionsResponse_suggestions,
    getQuerySuggestionsResponse_querySuggestionsId,
    getQuerySuggestionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetQuerySuggestions' smart constructor.
data GetQuerySuggestions = GetQuerySuggestions'
  { -- | The maximum number of query suggestions you want to show to your users.
    maxSuggestionsCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the index you want to get query suggestions from.
    indexId :: Prelude.Text,
    -- | The text of a user\'s query to generate query suggestions.
    --
    -- A query is suggested if the query prefix matches what a user starts to
    -- type as their query.
    --
    -- Amazon Kendra does not show any suggestions if a user types fewer than
    -- two characters or more than 60 characters. A query must also have at
    -- least one search result and contain at least one word of more than four
    -- characters.
    queryText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQuerySuggestions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxSuggestionsCount', 'getQuerySuggestions_maxSuggestionsCount' - The maximum number of query suggestions you want to show to your users.
--
-- 'indexId', 'getQuerySuggestions_indexId' - The identifier of the index you want to get query suggestions from.
--
-- 'queryText', 'getQuerySuggestions_queryText' - The text of a user\'s query to generate query suggestions.
--
-- A query is suggested if the query prefix matches what a user starts to
-- type as their query.
--
-- Amazon Kendra does not show any suggestions if a user types fewer than
-- two characters or more than 60 characters. A query must also have at
-- least one search result and contain at least one word of more than four
-- characters.
newGetQuerySuggestions ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'queryText'
  Prelude.Text ->
  GetQuerySuggestions
newGetQuerySuggestions pIndexId_ pQueryText_ =
  GetQuerySuggestions'
    { maxSuggestionsCount =
        Prelude.Nothing,
      indexId = pIndexId_,
      queryText = pQueryText_
    }

-- | The maximum number of query suggestions you want to show to your users.
getQuerySuggestions_maxSuggestionsCount :: Lens.Lens' GetQuerySuggestions (Prelude.Maybe Prelude.Int)
getQuerySuggestions_maxSuggestionsCount = Lens.lens (\GetQuerySuggestions' {maxSuggestionsCount} -> maxSuggestionsCount) (\s@GetQuerySuggestions' {} a -> s {maxSuggestionsCount = a} :: GetQuerySuggestions)

-- | The identifier of the index you want to get query suggestions from.
getQuerySuggestions_indexId :: Lens.Lens' GetQuerySuggestions Prelude.Text
getQuerySuggestions_indexId = Lens.lens (\GetQuerySuggestions' {indexId} -> indexId) (\s@GetQuerySuggestions' {} a -> s {indexId = a} :: GetQuerySuggestions)

-- | The text of a user\'s query to generate query suggestions.
--
-- A query is suggested if the query prefix matches what a user starts to
-- type as their query.
--
-- Amazon Kendra does not show any suggestions if a user types fewer than
-- two characters or more than 60 characters. A query must also have at
-- least one search result and contain at least one word of more than four
-- characters.
getQuerySuggestions_queryText :: Lens.Lens' GetQuerySuggestions Prelude.Text
getQuerySuggestions_queryText = Lens.lens (\GetQuerySuggestions' {queryText} -> queryText) (\s@GetQuerySuggestions' {} a -> s {queryText = a} :: GetQuerySuggestions)

instance Core.AWSRequest GetQuerySuggestions where
  type
    AWSResponse GetQuerySuggestions =
      GetQuerySuggestionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQuerySuggestionsResponse'
            Prelude.<$> (x Core..?> "Suggestions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "QuerySuggestionsId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQuerySuggestions

instance Prelude.NFData GetQuerySuggestions

instance Core.ToHeaders GetQuerySuggestions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.GetQuerySuggestions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetQuerySuggestions where
  toJSON GetQuerySuggestions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxSuggestionsCount" Core..=)
              Prelude.<$> maxSuggestionsCount,
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("QueryText" Core..= queryText)
          ]
      )

instance Core.ToPath GetQuerySuggestions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetQuerySuggestions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQuerySuggestionsResponse' smart constructor.
data GetQuerySuggestionsResponse = GetQuerySuggestionsResponse'
  { -- | A list of query suggestions for an index.
    suggestions :: Prelude.Maybe [Suggestion],
    -- | The unique identifier for a list of query suggestions for an index.
    querySuggestionsId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQuerySuggestionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suggestions', 'getQuerySuggestionsResponse_suggestions' - A list of query suggestions for an index.
--
-- 'querySuggestionsId', 'getQuerySuggestionsResponse_querySuggestionsId' - The unique identifier for a list of query suggestions for an index.
--
-- 'httpStatus', 'getQuerySuggestionsResponse_httpStatus' - The response's http status code.
newGetQuerySuggestionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQuerySuggestionsResponse
newGetQuerySuggestionsResponse pHttpStatus_ =
  GetQuerySuggestionsResponse'
    { suggestions =
        Prelude.Nothing,
      querySuggestionsId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of query suggestions for an index.
getQuerySuggestionsResponse_suggestions :: Lens.Lens' GetQuerySuggestionsResponse (Prelude.Maybe [Suggestion])
getQuerySuggestionsResponse_suggestions = Lens.lens (\GetQuerySuggestionsResponse' {suggestions} -> suggestions) (\s@GetQuerySuggestionsResponse' {} a -> s {suggestions = a} :: GetQuerySuggestionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for a list of query suggestions for an index.
getQuerySuggestionsResponse_querySuggestionsId :: Lens.Lens' GetQuerySuggestionsResponse (Prelude.Maybe Prelude.Text)
getQuerySuggestionsResponse_querySuggestionsId = Lens.lens (\GetQuerySuggestionsResponse' {querySuggestionsId} -> querySuggestionsId) (\s@GetQuerySuggestionsResponse' {} a -> s {querySuggestionsId = a} :: GetQuerySuggestionsResponse)

-- | The response's http status code.
getQuerySuggestionsResponse_httpStatus :: Lens.Lens' GetQuerySuggestionsResponse Prelude.Int
getQuerySuggestionsResponse_httpStatus = Lens.lens (\GetQuerySuggestionsResponse' {httpStatus} -> httpStatus) (\s@GetQuerySuggestionsResponse' {} a -> s {httpStatus = a} :: GetQuerySuggestionsResponse)

instance Prelude.NFData GetQuerySuggestionsResponse
