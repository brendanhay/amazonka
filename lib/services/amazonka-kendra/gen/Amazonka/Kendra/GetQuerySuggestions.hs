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
-- Module      : Amazonka.Kendra.GetQuerySuggestions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the queries that are suggested to your users.
--
-- @GetQuerySuggestions@ is currently not supported in the Amazon Web
-- Services GovCloud (US-West) region.
module Amazonka.Kendra.GetQuerySuggestions
  ( -- * Creating a Request
    GetQuerySuggestions (..),
    newGetQuerySuggestions,

    -- * Request Lenses
    getQuerySuggestions_attributeSuggestionsConfig,
    getQuerySuggestions_maxSuggestionsCount,
    getQuerySuggestions_suggestionTypes,
    getQuerySuggestions_indexId,
    getQuerySuggestions_queryText,

    -- * Destructuring the Response
    GetQuerySuggestionsResponse (..),
    newGetQuerySuggestionsResponse,

    -- * Response Lenses
    getQuerySuggestionsResponse_querySuggestionsId,
    getQuerySuggestionsResponse_suggestions,
    getQuerySuggestionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQuerySuggestions' smart constructor.
data GetQuerySuggestions = GetQuerySuggestions'
  { -- | Configuration information for the document fields\/attributes that you
    -- want to base query suggestions on.
    attributeSuggestionsConfig :: Prelude.Maybe AttributeSuggestionsGetConfig,
    -- | The maximum number of query suggestions you want to show to your users.
    maxSuggestionsCount :: Prelude.Maybe Prelude.Int,
    -- | The suggestions type to base query suggestions on. The suggestion types
    -- are query history or document fields\/attributes. You can set one type
    -- or the other.
    --
    -- If you set query history as your suggestions type, Amazon Kendra
    -- suggests queries relevant to your users based on popular queries in the
    -- query history.
    --
    -- If you set document fields\/attributes as your suggestions type, Amazon
    -- Kendra suggests queries relevant to your users based on the contents of
    -- document fields.
    suggestionTypes :: Prelude.Maybe [SuggestionType],
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
-- 'attributeSuggestionsConfig', 'getQuerySuggestions_attributeSuggestionsConfig' - Configuration information for the document fields\/attributes that you
-- want to base query suggestions on.
--
-- 'maxSuggestionsCount', 'getQuerySuggestions_maxSuggestionsCount' - The maximum number of query suggestions you want to show to your users.
--
-- 'suggestionTypes', 'getQuerySuggestions_suggestionTypes' - The suggestions type to base query suggestions on. The suggestion types
-- are query history or document fields\/attributes. You can set one type
-- or the other.
--
-- If you set query history as your suggestions type, Amazon Kendra
-- suggests queries relevant to your users based on popular queries in the
-- query history.
--
-- If you set document fields\/attributes as your suggestions type, Amazon
-- Kendra suggests queries relevant to your users based on the contents of
-- document fields.
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
    { attributeSuggestionsConfig =
        Prelude.Nothing,
      maxSuggestionsCount = Prelude.Nothing,
      suggestionTypes = Prelude.Nothing,
      indexId = pIndexId_,
      queryText = pQueryText_
    }

-- | Configuration information for the document fields\/attributes that you
-- want to base query suggestions on.
getQuerySuggestions_attributeSuggestionsConfig :: Lens.Lens' GetQuerySuggestions (Prelude.Maybe AttributeSuggestionsGetConfig)
getQuerySuggestions_attributeSuggestionsConfig = Lens.lens (\GetQuerySuggestions' {attributeSuggestionsConfig} -> attributeSuggestionsConfig) (\s@GetQuerySuggestions' {} a -> s {attributeSuggestionsConfig = a} :: GetQuerySuggestions)

-- | The maximum number of query suggestions you want to show to your users.
getQuerySuggestions_maxSuggestionsCount :: Lens.Lens' GetQuerySuggestions (Prelude.Maybe Prelude.Int)
getQuerySuggestions_maxSuggestionsCount = Lens.lens (\GetQuerySuggestions' {maxSuggestionsCount} -> maxSuggestionsCount) (\s@GetQuerySuggestions' {} a -> s {maxSuggestionsCount = a} :: GetQuerySuggestions)

-- | The suggestions type to base query suggestions on. The suggestion types
-- are query history or document fields\/attributes. You can set one type
-- or the other.
--
-- If you set query history as your suggestions type, Amazon Kendra
-- suggests queries relevant to your users based on popular queries in the
-- query history.
--
-- If you set document fields\/attributes as your suggestions type, Amazon
-- Kendra suggests queries relevant to your users based on the contents of
-- document fields.
getQuerySuggestions_suggestionTypes :: Lens.Lens' GetQuerySuggestions (Prelude.Maybe [SuggestionType])
getQuerySuggestions_suggestionTypes = Lens.lens (\GetQuerySuggestions' {suggestionTypes} -> suggestionTypes) (\s@GetQuerySuggestions' {} a -> s {suggestionTypes = a} :: GetQuerySuggestions) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQuerySuggestionsResponse'
            Prelude.<$> (x Data..?> "QuerySuggestionsId")
            Prelude.<*> (x Data..?> "Suggestions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQuerySuggestions where
  hashWithSalt _salt GetQuerySuggestions' {..} =
    _salt
      `Prelude.hashWithSalt` attributeSuggestionsConfig
      `Prelude.hashWithSalt` maxSuggestionsCount
      `Prelude.hashWithSalt` suggestionTypes
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` queryText

instance Prelude.NFData GetQuerySuggestions where
  rnf GetQuerySuggestions' {..} =
    Prelude.rnf attributeSuggestionsConfig
      `Prelude.seq` Prelude.rnf maxSuggestionsCount
      `Prelude.seq` Prelude.rnf suggestionTypes
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf queryText

instance Data.ToHeaders GetQuerySuggestions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.GetQuerySuggestions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetQuerySuggestions where
  toJSON GetQuerySuggestions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeSuggestionsConfig" Data..=)
              Prelude.<$> attributeSuggestionsConfig,
            ("MaxSuggestionsCount" Data..=)
              Prelude.<$> maxSuggestionsCount,
            ("SuggestionTypes" Data..=)
              Prelude.<$> suggestionTypes,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("QueryText" Data..= queryText)
          ]
      )

instance Data.ToPath GetQuerySuggestions where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQuerySuggestions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQuerySuggestionsResponse' smart constructor.
data GetQuerySuggestionsResponse = GetQuerySuggestionsResponse'
  { -- | The identifier for a list of query suggestions for an index.
    querySuggestionsId :: Prelude.Maybe Prelude.Text,
    -- | A list of query suggestions for an index.
    suggestions :: Prelude.Maybe [Suggestion],
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
-- 'querySuggestionsId', 'getQuerySuggestionsResponse_querySuggestionsId' - The identifier for a list of query suggestions for an index.
--
-- 'suggestions', 'getQuerySuggestionsResponse_suggestions' - A list of query suggestions for an index.
--
-- 'httpStatus', 'getQuerySuggestionsResponse_httpStatus' - The response's http status code.
newGetQuerySuggestionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQuerySuggestionsResponse
newGetQuerySuggestionsResponse pHttpStatus_ =
  GetQuerySuggestionsResponse'
    { querySuggestionsId =
        Prelude.Nothing,
      suggestions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for a list of query suggestions for an index.
getQuerySuggestionsResponse_querySuggestionsId :: Lens.Lens' GetQuerySuggestionsResponse (Prelude.Maybe Prelude.Text)
getQuerySuggestionsResponse_querySuggestionsId = Lens.lens (\GetQuerySuggestionsResponse' {querySuggestionsId} -> querySuggestionsId) (\s@GetQuerySuggestionsResponse' {} a -> s {querySuggestionsId = a} :: GetQuerySuggestionsResponse)

-- | A list of query suggestions for an index.
getQuerySuggestionsResponse_suggestions :: Lens.Lens' GetQuerySuggestionsResponse (Prelude.Maybe [Suggestion])
getQuerySuggestionsResponse_suggestions = Lens.lens (\GetQuerySuggestionsResponse' {suggestions} -> suggestions) (\s@GetQuerySuggestionsResponse' {} a -> s {suggestions = a} :: GetQuerySuggestionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getQuerySuggestionsResponse_httpStatus :: Lens.Lens' GetQuerySuggestionsResponse Prelude.Int
getQuerySuggestionsResponse_httpStatus = Lens.lens (\GetQuerySuggestionsResponse' {httpStatus} -> httpStatus) (\s@GetQuerySuggestionsResponse' {} a -> s {httpStatus = a} :: GetQuerySuggestionsResponse)

instance Prelude.NFData GetQuerySuggestionsResponse where
  rnf GetQuerySuggestionsResponse' {..} =
    Prelude.rnf querySuggestionsId
      `Prelude.seq` Prelude.rnf suggestions
      `Prelude.seq` Prelude.rnf httpStatus
