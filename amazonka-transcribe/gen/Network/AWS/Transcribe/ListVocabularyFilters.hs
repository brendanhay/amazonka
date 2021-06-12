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
-- Module      : Network.AWS.Transcribe.ListVocabularyFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about vocabulary filters.
module Network.AWS.Transcribe.ListVocabularyFilters
  ( -- * Creating a Request
    ListVocabularyFilters (..),
    newListVocabularyFilters,

    -- * Request Lenses
    listVocabularyFilters_nextToken,
    listVocabularyFilters_nameContains,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newListVocabularyFilters' smart constructor.
data ListVocabularyFilters = ListVocabularyFilters'
  { -- | If the result of the previous request to @ListVocabularyFilters@ was
    -- truncated, include the @NextToken@ to fetch the next set of collections.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the response so that it only contains vocabulary filters whose
    -- name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of filters to return in the response. If there are
    -- fewer results in the list, this response contains only the actual
    -- results.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVocabularyFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVocabularyFilters_nextToken' - If the result of the previous request to @ListVocabularyFilters@ was
-- truncated, include the @NextToken@ to fetch the next set of collections.
--
-- 'nameContains', 'listVocabularyFilters_nameContains' - Filters the response so that it only contains vocabulary filters whose
-- name contains the specified string.
--
-- 'maxResults', 'listVocabularyFilters_maxResults' - The maximum number of filters to return in the response. If there are
-- fewer results in the list, this response contains only the actual
-- results.
newListVocabularyFilters ::
  ListVocabularyFilters
newListVocabularyFilters =
  ListVocabularyFilters'
    { nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If the result of the previous request to @ListVocabularyFilters@ was
-- truncated, include the @NextToken@ to fetch the next set of collections.
listVocabularyFilters_nextToken :: Lens.Lens' ListVocabularyFilters (Core.Maybe Core.Text)
listVocabularyFilters_nextToken = Lens.lens (\ListVocabularyFilters' {nextToken} -> nextToken) (\s@ListVocabularyFilters' {} a -> s {nextToken = a} :: ListVocabularyFilters)

-- | Filters the response so that it only contains vocabulary filters whose
-- name contains the specified string.
listVocabularyFilters_nameContains :: Lens.Lens' ListVocabularyFilters (Core.Maybe Core.Text)
listVocabularyFilters_nameContains = Lens.lens (\ListVocabularyFilters' {nameContains} -> nameContains) (\s@ListVocabularyFilters' {} a -> s {nameContains = a} :: ListVocabularyFilters)

-- | The maximum number of filters to return in the response. If there are
-- fewer results in the list, this response contains only the actual
-- results.
listVocabularyFilters_maxResults :: Lens.Lens' ListVocabularyFilters (Core.Maybe Core.Natural)
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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "VocabularyFilters" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListVocabularyFilters

instance Core.NFData ListVocabularyFilters

instance Core.ToHeaders ListVocabularyFilters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListVocabularyFilters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListVocabularyFilters where
  toJSON ListVocabularyFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListVocabularyFilters where
  toPath = Core.const "/"

instance Core.ToQuery ListVocabularyFilters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListVocabularyFiltersResponse' smart constructor.
data ListVocabularyFiltersResponse = ListVocabularyFiltersResponse'
  { -- | The @ListVocabularyFilters@ operation returns a page of collections at a
    -- time. The maximum size of the page is set by the @MaxResults@ parameter.
    -- If there are more jobs in the list than the page size, Amazon Transcribe
    -- returns the @NextPage@ token. Include the token in the next request to
    -- the @ListVocabularyFilters@ operation to return in the next page of
    -- jobs.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of vocabulary filters. It contains at most @MaxResults@ number
    -- of filters. If there are more filters, call the @ListVocabularyFilters@
    -- operation again with the @NextToken@ parameter in the request set to the
    -- value of the @NextToken@ field in the response.
    vocabularyFilters :: Core.Maybe [VocabularyFilterInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListVocabularyFiltersResponse
newListVocabularyFiltersResponse pHttpStatus_ =
  ListVocabularyFiltersResponse'
    { nextToken =
        Core.Nothing,
      vocabularyFilters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ListVocabularyFilters@ operation returns a page of collections at a
-- time. The maximum size of the page is set by the @MaxResults@ parameter.
-- If there are more jobs in the list than the page size, Amazon Transcribe
-- returns the @NextPage@ token. Include the token in the next request to
-- the @ListVocabularyFilters@ operation to return in the next page of
-- jobs.
listVocabularyFiltersResponse_nextToken :: Lens.Lens' ListVocabularyFiltersResponse (Core.Maybe Core.Text)
listVocabularyFiltersResponse_nextToken = Lens.lens (\ListVocabularyFiltersResponse' {nextToken} -> nextToken) (\s@ListVocabularyFiltersResponse' {} a -> s {nextToken = a} :: ListVocabularyFiltersResponse)

-- | The list of vocabulary filters. It contains at most @MaxResults@ number
-- of filters. If there are more filters, call the @ListVocabularyFilters@
-- operation again with the @NextToken@ parameter in the request set to the
-- value of the @NextToken@ field in the response.
listVocabularyFiltersResponse_vocabularyFilters :: Lens.Lens' ListVocabularyFiltersResponse (Core.Maybe [VocabularyFilterInfo])
listVocabularyFiltersResponse_vocabularyFilters = Lens.lens (\ListVocabularyFiltersResponse' {vocabularyFilters} -> vocabularyFilters) (\s@ListVocabularyFiltersResponse' {} a -> s {vocabularyFilters = a} :: ListVocabularyFiltersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listVocabularyFiltersResponse_httpStatus :: Lens.Lens' ListVocabularyFiltersResponse Core.Int
listVocabularyFiltersResponse_httpStatus = Lens.lens (\ListVocabularyFiltersResponse' {httpStatus} -> httpStatus) (\s@ListVocabularyFiltersResponse' {} a -> s {httpStatus = a} :: ListVocabularyFiltersResponse)

instance Core.NFData ListVocabularyFiltersResponse
