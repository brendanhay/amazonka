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
-- Module      : Amazonka.ResourceExplorer2.Search
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for resources and displays details about all resources that
-- match the specified criteria. You must specify a query string.
--
-- All search queries must use a view. If you don\'t explicitly specify a
-- view, then Amazon Web Services Resource Explorer uses the default view
-- for the Amazon Web Services Region in which you call this operation. The
-- results are the logical intersection of the results that match both the
-- @QueryString@ parameter supplied to this operation and the
-- @SearchFilter@ parameter attached to the view.
--
-- For the complete syntax supported by the @QueryString@ parameter, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/APIReference/about-query-syntax.html Search query syntax reference for Resource Explorer>.
--
-- If your search results are empty, or are missing results that you think
-- should be there, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/troubleshooting_search.html Troubleshooting Resource Explorer search>.
--
-- This operation returns paginated results.
module Amazonka.ResourceExplorer2.Search
  ( -- * Creating a Request
    Search (..),
    newSearch,

    -- * Request Lenses
    search_maxResults,
    search_nextToken,
    search_viewArn,
    search_queryString,

    -- * Destructuring the Response
    SearchResponse (..),
    newSearchResponse,

    -- * Response Lenses
    searchResponse_count,
    searchResponse_nextToken,
    searchResponse_resources,
    searchResponse_viewArn,
    searchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearch' smart constructor.
data Search = Search'
  { -- | The maximum number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- appropriate to the operation. If additional items exist beyond those
    -- included in the current response, the @NextToken@ response element is
    -- present and has a value (is not null). Include that value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results.
    --
    -- An API operation can return fewer results than the maximum even when
    -- there are more results available. You should check @NextToken@ after
    -- every operation to ensure that you receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view to use for the query. If you don\'t specify a value for this
    -- parameter, then the operation automatically uses the default view for
    -- the Amazon Web Services Region in which you called this operation. If
    -- the Region either doesn\'t have a default view or if you don\'t have
    -- permission to use the default view, then the operation fails with a
    -- @401 Unauthorized@ exception.
    viewArn :: Prelude.Maybe Prelude.Text,
    -- | A string that includes keywords and filters that specify the resources
    -- that you want to include in the results.
    --
    -- For the complete syntax supported by the @QueryString@ parameter, see
    -- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html Search query syntax reference for Resource Explorer>.
    --
    -- The search is completely case insensitive. You can specify an empty
    -- string to return all results up to the limit of 1,000 total results.
    --
    -- The operation can return only the first 1,000 results. If the resource
    -- you want is not included, then use a different value for @QueryString@
    -- to refine the results.
    queryString :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Search' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'search_maxResults' - The maximum number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- appropriate to the operation. If additional items exist beyond those
-- included in the current response, the @NextToken@ response element is
-- present and has a value (is not null). Include that value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results.
--
-- An API operation can return fewer results than the maximum even when
-- there are more results available. You should check @NextToken@ after
-- every operation to ensure that you receive all of the results.
--
-- 'nextToken', 'search_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'viewArn', 'search_viewArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view to use for the query. If you don\'t specify a value for this
-- parameter, then the operation automatically uses the default view for
-- the Amazon Web Services Region in which you called this operation. If
-- the Region either doesn\'t have a default view or if you don\'t have
-- permission to use the default view, then the operation fails with a
-- @401 Unauthorized@ exception.
--
-- 'queryString', 'search_queryString' - A string that includes keywords and filters that specify the resources
-- that you want to include in the results.
--
-- For the complete syntax supported by the @QueryString@ parameter, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html Search query syntax reference for Resource Explorer>.
--
-- The search is completely case insensitive. You can specify an empty
-- string to return all results up to the limit of 1,000 total results.
--
-- The operation can return only the first 1,000 results. If the resource
-- you want is not included, then use a different value for @QueryString@
-- to refine the results.
newSearch ::
  -- | 'queryString'
  Prelude.Text ->
  Search
newSearch pQueryString_ =
  Search'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      viewArn = Prelude.Nothing,
      queryString = Data._Sensitive Lens.# pQueryString_
    }

-- | The maximum number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- appropriate to the operation. If additional items exist beyond those
-- included in the current response, the @NextToken@ response element is
-- present and has a value (is not null). Include that value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results.
--
-- An API operation can return fewer results than the maximum even when
-- there are more results available. You should check @NextToken@ after
-- every operation to ensure that you receive all of the results.
search_maxResults :: Lens.Lens' Search (Prelude.Maybe Prelude.Natural)
search_maxResults = Lens.lens (\Search' {maxResults} -> maxResults) (\s@Search' {} a -> s {maxResults = a} :: Search)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
search_nextToken :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_nextToken = Lens.lens (\Search' {nextToken} -> nextToken) (\s@Search' {} a -> s {nextToken = a} :: Search)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view to use for the query. If you don\'t specify a value for this
-- parameter, then the operation automatically uses the default view for
-- the Amazon Web Services Region in which you called this operation. If
-- the Region either doesn\'t have a default view or if you don\'t have
-- permission to use the default view, then the operation fails with a
-- @401 Unauthorized@ exception.
search_viewArn :: Lens.Lens' Search (Prelude.Maybe Prelude.Text)
search_viewArn = Lens.lens (\Search' {viewArn} -> viewArn) (\s@Search' {} a -> s {viewArn = a} :: Search)

-- | A string that includes keywords and filters that specify the resources
-- that you want to include in the results.
--
-- For the complete syntax supported by the @QueryString@ parameter, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html Search query syntax reference for Resource Explorer>.
--
-- The search is completely case insensitive. You can specify an empty
-- string to return all results up to the limit of 1,000 total results.
--
-- The operation can return only the first 1,000 results. If the resource
-- you want is not included, then use a different value for @QueryString@
-- to refine the results.
search_queryString :: Lens.Lens' Search Prelude.Text
search_queryString = Lens.lens (\Search' {queryString} -> queryString) (\s@Search' {} a -> s {queryString = a} :: Search) Prelude.. Data._Sensitive

instance Core.AWSPager Search where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchResponse_resources Prelude.. Lens._Just
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResponse'
            Prelude.<$> (x Data..?> "Count")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ViewArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Search where
  hashWithSalt _salt Search' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` viewArn
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData Search where
  rnf Search' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf viewArn
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders Search where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Search where
  toJSON Search' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ViewArn" Data..=) Prelude.<$> viewArn,
            Prelude.Just ("QueryString" Data..= queryString)
          ]
      )

instance Data.ToPath Search where
  toPath = Prelude.const "/Search"

instance Data.ToQuery Search where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchResponse' smart constructor.
data SearchResponse = SearchResponse'
  { -- | The number of resources that match the query.
    count :: Prelude.Maybe ResourceCount,
    -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of structures that describe the resources that match the query.
    resources :: Prelude.Maybe [Resource],
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view that this operation used to perform the search.
    viewArn :: Prelude.Maybe Prelude.Text,
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
-- 'count', 'searchResponse_count' - The number of resources that match the query.
--
-- 'nextToken', 'searchResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'resources', 'searchResponse_resources' - The list of structures that describe the resources that match the query.
--
-- 'viewArn', 'searchResponse_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that this operation used to perform the search.
--
-- 'httpStatus', 'searchResponse_httpStatus' - The response's http status code.
newSearchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchResponse
newSearchResponse pHttpStatus_ =
  SearchResponse'
    { count = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resources = Prelude.Nothing,
      viewArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of resources that match the query.
searchResponse_count :: Lens.Lens' SearchResponse (Prelude.Maybe ResourceCount)
searchResponse_count = Lens.lens (\SearchResponse' {count} -> count) (\s@SearchResponse' {} a -> s {count = a} :: SearchResponse)

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
searchResponse_nextToken :: Lens.Lens' SearchResponse (Prelude.Maybe Prelude.Text)
searchResponse_nextToken = Lens.lens (\SearchResponse' {nextToken} -> nextToken) (\s@SearchResponse' {} a -> s {nextToken = a} :: SearchResponse)

-- | The list of structures that describe the resources that match the query.
searchResponse_resources :: Lens.Lens' SearchResponse (Prelude.Maybe [Resource])
searchResponse_resources = Lens.lens (\SearchResponse' {resources} -> resources) (\s@SearchResponse' {} a -> s {resources = a} :: SearchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that this operation used to perform the search.
searchResponse_viewArn :: Lens.Lens' SearchResponse (Prelude.Maybe Prelude.Text)
searchResponse_viewArn = Lens.lens (\SearchResponse' {viewArn} -> viewArn) (\s@SearchResponse' {} a -> s {viewArn = a} :: SearchResponse)

-- | The response's http status code.
searchResponse_httpStatus :: Lens.Lens' SearchResponse Prelude.Int
searchResponse_httpStatus = Lens.lens (\SearchResponse' {httpStatus} -> httpStatus) (\s@SearchResponse' {} a -> s {httpStatus = a} :: SearchResponse)

instance Prelude.NFData SearchResponse where
  rnf SearchResponse' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf viewArn
      `Prelude.seq` Prelude.rnf httpStatus
