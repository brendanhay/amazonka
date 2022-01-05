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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about vocabulary filters.
module Amazonka.Transcribe.ListVocabularyFilters
  ( -- * Creating a Request
    ListVocabularyFilters (..),
    newListVocabularyFilters,

    -- * Request Lenses
    listVocabularyFilters_nameContains,
    listVocabularyFilters_nextToken,
    listVocabularyFilters_maxResults,

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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListVocabularyFilters' smart constructor.
data ListVocabularyFilters = ListVocabularyFilters'
  { -- | Filters the response so that it only contains vocabulary filters whose
    -- name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous request to @ListVocabularyFilters@ was
    -- truncated, include the @NextToken@ to fetch the next set of collections.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of filters to return in each page of results. If
    -- there are fewer results than the value you specify, only the actual
    -- results are returned. If you do not specify a value, the default of 5 is
    -- used.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nameContains', 'listVocabularyFilters_nameContains' - Filters the response so that it only contains vocabulary filters whose
-- name contains the specified string.
--
-- 'nextToken', 'listVocabularyFilters_nextToken' - If the result of the previous request to @ListVocabularyFilters@ was
-- truncated, include the @NextToken@ to fetch the next set of collections.
--
-- 'maxResults', 'listVocabularyFilters_maxResults' - The maximum number of filters to return in each page of results. If
-- there are fewer results than the value you specify, only the actual
-- results are returned. If you do not specify a value, the default of 5 is
-- used.
newListVocabularyFilters ::
  ListVocabularyFilters
newListVocabularyFilters =
  ListVocabularyFilters'
    { nameContains =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Filters the response so that it only contains vocabulary filters whose
-- name contains the specified string.
listVocabularyFilters_nameContains :: Lens.Lens' ListVocabularyFilters (Prelude.Maybe Prelude.Text)
listVocabularyFilters_nameContains = Lens.lens (\ListVocabularyFilters' {nameContains} -> nameContains) (\s@ListVocabularyFilters' {} a -> s {nameContains = a} :: ListVocabularyFilters)

-- | If the result of the previous request to @ListVocabularyFilters@ was
-- truncated, include the @NextToken@ to fetch the next set of collections.
listVocabularyFilters_nextToken :: Lens.Lens' ListVocabularyFilters (Prelude.Maybe Prelude.Text)
listVocabularyFilters_nextToken = Lens.lens (\ListVocabularyFilters' {nextToken} -> nextToken) (\s@ListVocabularyFilters' {} a -> s {nextToken = a} :: ListVocabularyFilters)

-- | The maximum number of filters to return in each page of results. If
-- there are fewer results than the value you specify, only the actual
-- results are returned. If you do not specify a value, the default of 5 is
-- used.
listVocabularyFilters_maxResults :: Lens.Lens' ListVocabularyFilters (Prelude.Maybe Prelude.Natural)
listVocabularyFilters_maxResults = Lens.lens (\ListVocabularyFilters' {maxResults} -> maxResults) (\s@ListVocabularyFilters' {} a -> s {maxResults = a} :: ListVocabularyFilters)

instance Core.AWSRequest ListVocabularyFilters where
  type
    AWSResponse ListVocabularyFilters =
      ListVocabularyFiltersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVocabularyFiltersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "VocabularyFilters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVocabularyFilters where
  hashWithSalt _salt ListVocabularyFilters' {..} =
    _salt `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListVocabularyFilters where
  rnf ListVocabularyFilters' {..} =
    Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListVocabularyFilters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListVocabularyFilters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListVocabularyFilters where
  toJSON ListVocabularyFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListVocabularyFilters where
  toPath = Prelude.const "/"

instance Core.ToQuery ListVocabularyFilters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVocabularyFiltersResponse' smart constructor.
data ListVocabularyFiltersResponse = ListVocabularyFiltersResponse'
  { -- | The @ListVocabularyFilters@ operation returns a page of collections at a
    -- time. The maximum size of the page is set by the @MaxResults@ parameter.
    -- If there are more jobs in the list than the page size, Amazon Transcribe
    -- returns the @NextPage@ token. Include the token in the next request to
    -- the @ListVocabularyFilters@ operation to return in the next page of
    -- jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of vocabulary filters. It contains at most @MaxResults@ number
    -- of filters. If there are more filters, call the @ListVocabularyFilters@
    -- operation again with the @NextToken@ parameter in the request set to the
    -- value of the @NextToken@ field in the response.
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
-- 'nextToken', 'listVocabularyFiltersResponse_nextToken' - The @ListVocabularyFilters@ operation returns a page of collections at a
-- time. The maximum size of the page is set by the @MaxResults@ parameter.
-- If there are more jobs in the list than the page size, Amazon Transcribe
-- returns the @NextPage@ token. Include the token in the next request to
-- the @ListVocabularyFilters@ operation to return in the next page of
-- jobs.
--
-- 'vocabularyFilters', 'listVocabularyFiltersResponse_vocabularyFilters' - The list of vocabulary filters. It contains at most @MaxResults@ number
-- of filters. If there are more filters, call the @ListVocabularyFilters@
-- operation again with the @NextToken@ parameter in the request set to the
-- value of the @NextToken@ field in the response.
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

-- | The @ListVocabularyFilters@ operation returns a page of collections at a
-- time. The maximum size of the page is set by the @MaxResults@ parameter.
-- If there are more jobs in the list than the page size, Amazon Transcribe
-- returns the @NextPage@ token. Include the token in the next request to
-- the @ListVocabularyFilters@ operation to return in the next page of
-- jobs.
listVocabularyFiltersResponse_nextToken :: Lens.Lens' ListVocabularyFiltersResponse (Prelude.Maybe Prelude.Text)
listVocabularyFiltersResponse_nextToken = Lens.lens (\ListVocabularyFiltersResponse' {nextToken} -> nextToken) (\s@ListVocabularyFiltersResponse' {} a -> s {nextToken = a} :: ListVocabularyFiltersResponse)

-- | The list of vocabulary filters. It contains at most @MaxResults@ number
-- of filters. If there are more filters, call the @ListVocabularyFilters@
-- operation again with the @NextToken@ parameter in the request set to the
-- value of the @NextToken@ field in the response.
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
