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
-- Module      : Amazonka.SageMaker.Search
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds Amazon SageMaker resources that match a search query. Matching
-- resources are returned as a list of @SearchRecord@ objects in the
-- response. You can sort the search results by any resource property in a
-- ascending or descending order.
--
-- You can query against the following value types: numeric, text, Boolean,
-- and timestamp.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.Search
  ( -- * Creating a Request
    Search (..),
    newSearch,

    -- * Request Lenses
    search_nextToken,
    search_searchExpression,
    search_sortOrder,
    search_maxResults,
    search_sortBy,
    search_resource,

    -- * Destructuring the Response
    SearchResponse (..),
    newSearchResponse,

    -- * Response Lenses
    searchResponse_results,
    searchResponse_nextToken,
    searchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newSearch' smart constructor.
data Search = Search'
  { -- | If more than @MaxResults@ resources match the specified
    -- @SearchExpression@, the response includes a @NextToken@. The @NextToken@
    -- can be passed to the next @SearchRequest@ to continue retrieving
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A Boolean conditional statement. Resources must satisfy this condition
    -- to be included in search results. You must provide at least one
    -- subexpression, filter, or nested filter. The maximum number of recursive
    -- @SubExpressions@, @NestedFilters@, and @Filters@ that can be included in
    -- a @SearchExpression@ object is 50.
    searchExpression :: Prelude.Maybe SearchExpression,
    -- | How @SearchResults@ are ordered. Valid values are @Ascending@ or
    -- @Descending@. The default is @Descending@.
    sortOrder :: Prelude.Maybe SearchSortOrder,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the resource property used to sort the @SearchResults@. The
    -- default is @LastModifiedTime@.
    sortBy :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon SageMaker resource to search for.
    resource :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Search' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'search_nextToken' - If more than @MaxResults@ resources match the specified
-- @SearchExpression@, the response includes a @NextToken@. The @NextToken@
-- can be passed to the next @SearchRequest@ to continue retrieving
-- results.
--
-- 'searchExpression', 'search_searchExpression' - A Boolean conditional statement. Resources must satisfy this condition
-- to be included in search results. You must provide at least one
-- subexpression, filter, or nested filter. The maximum number of recursive
-- @SubExpressions@, @NestedFilters@, and @Filters@ that can be included in
-- a @SearchExpression@ object is 50.
--
-- 'sortOrder', 'search_sortOrder' - How @SearchResults@ are ordered. Valid values are @Ascending@ or
-- @Descending@. The default is @Descending@.
--
-- 'maxResults', 'search_maxResults' - The maximum number of results to return.
--
-- 'sortBy', 'search_sortBy' - The name of the resource property used to sort the @SearchResults@. The
-- default is @LastModifiedTime@.
--
-- 'resource', 'search_resource' - The name of the Amazon SageMaker resource to search for.
newSearch ::
  -- | 'resource'
  ResourceType ->
  Search
newSearch pResource_ =
  Search'
    { nextToken = Prelude.Nothing,
      searchExpression = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      resource = pResource_
    }

-- | If more than @MaxResults@ resources match the specified
-- @SearchExpression@, the response includes a @NextToken@. The @NextToken@
-- can be passed to the next @SearchRequest@ to continue retrieving
-- results.
search_nextToken :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_nextToken = Lens.lens (\Search' {nextToken} -> nextToken) (\s@Search' {} a -> s {nextToken = a} :: Search)

-- | A Boolean conditional statement. Resources must satisfy this condition
-- to be included in search results. You must provide at least one
-- subexpression, filter, or nested filter. The maximum number of recursive
-- @SubExpressions@, @NestedFilters@, and @Filters@ that can be included in
-- a @SearchExpression@ object is 50.
search_searchExpression :: Lens.Lens' Search (Prelude.Maybe SearchExpression)
search_searchExpression = Lens.lens (\Search' {searchExpression} -> searchExpression) (\s@Search' {} a -> s {searchExpression = a} :: Search)

-- | How @SearchResults@ are ordered. Valid values are @Ascending@ or
-- @Descending@. The default is @Descending@.
search_sortOrder :: Lens.Lens' Search (Prelude.Maybe SearchSortOrder)
search_sortOrder = Lens.lens (\Search' {sortOrder} -> sortOrder) (\s@Search' {} a -> s {sortOrder = a} :: Search)

-- | The maximum number of results to return.
search_maxResults :: Lens.Lens' Search (Prelude.Maybe Prelude.Natural)
search_maxResults = Lens.lens (\Search' {maxResults} -> maxResults) (\s@Search' {} a -> s {maxResults = a} :: Search)

-- | The name of the resource property used to sort the @SearchResults@. The
-- default is @LastModifiedTime@.
search_sortBy :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_sortBy = Lens.lens (\Search' {sortBy} -> sortBy) (\s@Search' {} a -> s {sortBy = a} :: Search)

-- | The name of the Amazon SageMaker resource to search for.
search_resource :: Lens.Lens' Search ResourceType
search_resource = Lens.lens (\Search' {resource} -> resource) (\s@Search' {} a -> s {resource = a} :: Search)

instance Core.AWSPager Search where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchResponse_results Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& search_nextToken
          Lens..~ rs
          Lens.^? searchResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest Search where
  type AWSResponse Search = SearchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResponse'
            Prelude.<$> (x Core..?> "Results" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Search

instance Prelude.NFData Search

instance Core.ToHeaders Search where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.Search" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON Search where
  toJSON Search' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SearchExpression" Core..=)
              Prelude.<$> searchExpression,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            Prelude.Just ("Resource" Core..= resource)
          ]
      )

instance Core.ToPath Search where
  toPath = Prelude.const "/"

instance Core.ToQuery Search where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchResponse' smart constructor.
data SearchResponse = SearchResponse'
  { -- | A list of @SearchRecord@ objects.
    results :: Prelude.Maybe [SearchRecord],
    -- | If the result of the previous @Search@ request was truncated, the
    -- response includes a NextToken. To retrieve the next set of results, use
    -- the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'results', 'searchResponse_results' - A list of @SearchRecord@ objects.
--
-- 'nextToken', 'searchResponse_nextToken' - If the result of the previous @Search@ request was truncated, the
-- response includes a NextToken. To retrieve the next set of results, use
-- the token in the next request.
--
-- 'httpStatus', 'searchResponse_httpStatus' - The response's http status code.
newSearchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchResponse
newSearchResponse pHttpStatus_ =
  SearchResponse'
    { results = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @SearchRecord@ objects.
searchResponse_results :: Lens.Lens' SearchResponse (Prelude.Maybe [SearchRecord])
searchResponse_results = Lens.lens (\SearchResponse' {results} -> results) (\s@SearchResponse' {} a -> s {results = a} :: SearchResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the result of the previous @Search@ request was truncated, the
-- response includes a NextToken. To retrieve the next set of results, use
-- the token in the next request.
searchResponse_nextToken :: Lens.Lens' SearchResponse (Prelude.Maybe Prelude.Text)
searchResponse_nextToken = Lens.lens (\SearchResponse' {nextToken} -> nextToken) (\s@SearchResponse' {} a -> s {nextToken = a} :: SearchResponse)

-- | The response's http status code.
searchResponse_httpStatus :: Lens.Lens' SearchResponse Prelude.Int
searchResponse_httpStatus = Lens.lens (\SearchResponse' {httpStatus} -> httpStatus) (\s@SearchResponse' {} a -> s {httpStatus = a} :: SearchResponse)

instance Prelude.NFData SearchResponse
