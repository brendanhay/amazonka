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
-- Module      : Amazonka.Transcribe.ListVocabularyFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of custom vocabulary filters that match the specified
-- criteria. If no criteria are specified, all custom vocabularies are
-- returned.
--
-- To get detailed information about a specific custom vocabulary filter,
-- use the operation.
module Amazonka.Transcribe.ListVocabularyFilters
  ( -- * Creating a Request
    ListVocabularyFilters (..),
    newListVocabularyFilters,

    -- * Request Lenses
    listVocabularyFilters_maxResults,
    listVocabularyFilters_nameContains,
    listVocabularyFilters_nextToken,

    -- * Destructuring the Response
    ListVocabularyFiltersResponse (..),
    newListVocabularyFiltersResponse,

    -- * Response Lenses
    listVocabularyFiltersResponse_nextToken,
    listVocabularyFiltersResponse_vocabularyFilters,
    listVocabularyFiltersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListVocabularyFilters' smart constructor.
data ListVocabularyFilters = ListVocabularyFilters'
  { -- | The maximum number of custom vocabulary filters to return in each page
    -- of results. If there are fewer results than the value that you specify,
    -- only the actual results are returned. If you don\'t specify a value, a
    -- default of 5 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Returns only the custom vocabulary filters that contain the specified
    -- string. The search is not case sensitive.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If your @ListVocabularyFilters@ request returns more results than can be
    -- displayed, @NextToken@ is displayed in the response with an associated
    -- string. To get the next page of results, copy this string and repeat
    -- your request, including @NextToken@ with the value of the copied string.
    -- Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVocabularyFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVocabularyFilters_maxResults' - The maximum number of custom vocabulary filters to return in each page
-- of results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
--
-- 'nameContains', 'listVocabularyFilters_nameContains' - Returns only the custom vocabulary filters that contain the specified
-- string. The search is not case sensitive.
--
-- 'nextToken', 'listVocabularyFilters_nextToken' - If your @ListVocabularyFilters@ request returns more results than can be
-- displayed, @NextToken@ is displayed in the response with an associated
-- string. To get the next page of results, copy this string and repeat
-- your request, including @NextToken@ with the value of the copied string.
-- Repeat as needed to view all your results.
newListVocabularyFilters ::
  ListVocabularyFilters
newListVocabularyFilters =
  ListVocabularyFilters'
    { maxResults =
        Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of custom vocabulary filters to return in each page
-- of results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
listVocabularyFilters_maxResults :: Lens.Lens' ListVocabularyFilters (Prelude.Maybe Prelude.Natural)
listVocabularyFilters_maxResults = Lens.lens (\ListVocabularyFilters' {maxResults} -> maxResults) (\s@ListVocabularyFilters' {} a -> s {maxResults = a} :: ListVocabularyFilters)

-- | Returns only the custom vocabulary filters that contain the specified
-- string. The search is not case sensitive.
listVocabularyFilters_nameContains :: Lens.Lens' ListVocabularyFilters (Prelude.Maybe Prelude.Text)
listVocabularyFilters_nameContains = Lens.lens (\ListVocabularyFilters' {nameContains} -> nameContains) (\s@ListVocabularyFilters' {} a -> s {nameContains = a} :: ListVocabularyFilters)

-- | If your @ListVocabularyFilters@ request returns more results than can be
-- displayed, @NextToken@ is displayed in the response with an associated
-- string. To get the next page of results, copy this string and repeat
-- your request, including @NextToken@ with the value of the copied string.
-- Repeat as needed to view all your results.
listVocabularyFilters_nextToken :: Lens.Lens' ListVocabularyFilters (Prelude.Maybe Prelude.Text)
listVocabularyFilters_nextToken = Lens.lens (\ListVocabularyFilters' {nextToken} -> nextToken) (\s@ListVocabularyFilters' {} a -> s {nextToken = a} :: ListVocabularyFilters)

instance Core.AWSRequest ListVocabularyFilters where
  type
    AWSResponse ListVocabularyFilters =
      ListVocabularyFiltersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVocabularyFiltersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "VocabularyFilters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVocabularyFilters where
  hashWithSalt _salt ListVocabularyFilters' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVocabularyFilters where
  rnf ListVocabularyFilters' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListVocabularyFilters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.ListVocabularyFilters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVocabularyFilters where
  toJSON ListVocabularyFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListVocabularyFilters where
  toPath = Prelude.const "/"

instance Data.ToQuery ListVocabularyFilters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVocabularyFiltersResponse' smart constructor.
data ListVocabularyFiltersResponse = ListVocabularyFiltersResponse'
  { -- | If @NextToken@ is present in your response, it indicates that not all
    -- results are displayed. To view the next set of results, copy the string
    -- associated with the @NextToken@ parameter in your results output, then
    -- run your request again including @NextToken@ with the value of the
    -- copied string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the custom vocabulary filters that match the
    -- criteria specified in your request.
    vocabularyFilters :: Prelude.Maybe [VocabularyFilterInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVocabularyFiltersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVocabularyFiltersResponse_nextToken' - If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
--
-- 'vocabularyFilters', 'listVocabularyFiltersResponse_vocabularyFilters' - Provides information about the custom vocabulary filters that match the
-- criteria specified in your request.
--
-- 'httpStatus', 'listVocabularyFiltersResponse_httpStatus' - The response's http status code.
newListVocabularyFiltersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVocabularyFiltersResponse
newListVocabularyFiltersResponse pHttpStatus_ =
  ListVocabularyFiltersResponse'
    { nextToken =
        Prelude.Nothing,
      vocabularyFilters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
listVocabularyFiltersResponse_nextToken :: Lens.Lens' ListVocabularyFiltersResponse (Prelude.Maybe Prelude.Text)
listVocabularyFiltersResponse_nextToken = Lens.lens (\ListVocabularyFiltersResponse' {nextToken} -> nextToken) (\s@ListVocabularyFiltersResponse' {} a -> s {nextToken = a} :: ListVocabularyFiltersResponse)

-- | Provides information about the custom vocabulary filters that match the
-- criteria specified in your request.
listVocabularyFiltersResponse_vocabularyFilters :: Lens.Lens' ListVocabularyFiltersResponse (Prelude.Maybe [VocabularyFilterInfo])
listVocabularyFiltersResponse_vocabularyFilters = Lens.lens (\ListVocabularyFiltersResponse' {vocabularyFilters} -> vocabularyFilters) (\s@ListVocabularyFiltersResponse' {} a -> s {vocabularyFilters = a} :: ListVocabularyFiltersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVocabularyFiltersResponse_httpStatus :: Lens.Lens' ListVocabularyFiltersResponse Prelude.Int
listVocabularyFiltersResponse_httpStatus = Lens.lens (\ListVocabularyFiltersResponse' {httpStatus} -> httpStatus) (\s@ListVocabularyFiltersResponse' {} a -> s {httpStatus = a} :: ListVocabularyFiltersResponse)

instance Prelude.NFData ListVocabularyFiltersResponse where
  rnf ListVocabularyFiltersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vocabularyFilters
      `Prelude.seq` Prelude.rnf httpStatus
