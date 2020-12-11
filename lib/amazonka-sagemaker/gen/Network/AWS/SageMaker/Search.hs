{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Search
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds Amazon SageMaker resources that match a search query. Matching resources are returned as a list of @SearchRecord@ objects in the response. You can sort the search results by any resource property in a ascending or descending order.
--
-- You can query against the following value types: numeric, text, Boolean, and timestamp.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.Search
  ( -- * Creating a request
    Search (..),
    mkSearch,

    -- ** Request lenses
    sNextToken,
    sSearchExpression,
    sSortOrder,
    sMaxResults,
    sSortBy,
    sResource,

    -- * Destructuring the response
    SearchResponse (..),
    mkSearchResponse,

    -- ** Response lenses
    srsResults,
    srsNextToken,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkSearch' smart constructor.
data Search = Search'
  { nextToken :: Lude.Maybe Lude.Text,
    searchExpression :: Lude.Maybe SearchExpression,
    sortOrder :: Lude.Maybe SearchSortOrder,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe Lude.Text,
    resource :: ResourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Search' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return.
-- * 'nextToken' - If more than @MaxResults@ resources match the specified @SearchExpression@ , the response includes a @NextToken@ . The @NextToken@ can be passed to the next @SearchRequest@ to continue retrieving results.
-- * 'resource' - The name of the Amazon SageMaker resource to search for.
-- * 'searchExpression' - A Boolean conditional statement. Resources must satisfy this condition to be included in search results. You must provide at least one subexpression, filter, or nested filter. The maximum number of recursive @SubExpressions@ , @NestedFilters@ , and @Filters@ that can be included in a @SearchExpression@ object is 50.
-- * 'sortBy' - The name of the resource property used to sort the @SearchResults@ . The default is @LastModifiedTime@ .
-- * 'sortOrder' - How @SearchResults@ are ordered. Valid values are @Ascending@ or @Descending@ . The default is @Descending@ .
mkSearch ::
  -- | 'resource'
  ResourceType ->
  Search
mkSearch pResource_ =
  Search'
    { nextToken = Lude.Nothing,
      searchExpression = Lude.Nothing,
      sortOrder = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing,
      resource = pResource_
    }

-- | If more than @MaxResults@ resources match the specified @SearchExpression@ , the response includes a @NextToken@ . The @NextToken@ can be passed to the next @SearchRequest@ to continue retrieving results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNextToken :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sNextToken = Lens.lens (nextToken :: Search -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: Search)
{-# DEPRECATED sNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A Boolean conditional statement. Resources must satisfy this condition to be included in search results. You must provide at least one subexpression, filter, or nested filter. The maximum number of recursive @SubExpressions@ , @NestedFilters@ , and @Filters@ that can be included in a @SearchExpression@ object is 50.
--
-- /Note:/ Consider using 'searchExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSearchExpression :: Lens.Lens' Search (Lude.Maybe SearchExpression)
sSearchExpression = Lens.lens (searchExpression :: Search -> Lude.Maybe SearchExpression) (\s a -> s {searchExpression = a} :: Search)
{-# DEPRECATED sSearchExpression "Use generic-lens or generic-optics with 'searchExpression' instead." #-}

-- | How @SearchResults@ are ordered. Valid values are @Ascending@ or @Descending@ . The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSortOrder :: Lens.Lens' Search (Lude.Maybe SearchSortOrder)
sSortOrder = Lens.lens (sortOrder :: Search -> Lude.Maybe SearchSortOrder) (\s a -> s {sortOrder = a} :: Search)
{-# DEPRECATED sSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxResults :: Lens.Lens' Search (Lude.Maybe Lude.Natural)
sMaxResults = Lens.lens (maxResults :: Search -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: Search)
{-# DEPRECATED sMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the resource property used to sort the @SearchResults@ . The default is @LastModifiedTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSortBy :: Lens.Lens' Search (Lude.Maybe Lude.Text)
sSortBy = Lens.lens (sortBy :: Search -> Lude.Maybe Lude.Text) (\s a -> s {sortBy = a} :: Search)
{-# DEPRECATED sSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The name of the Amazon SageMaker resource to search for.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResource :: Lens.Lens' Search ResourceType
sResource = Lens.lens (resource :: Search -> ResourceType) (\s a -> s {resource = a} :: Search)
{-# DEPRECATED sResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Page.AWSPager Search where
  page rq rs
    | Page.stop (rs Lens.^. srsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. srsResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& sNextToken Lens..~ rs Lens.^. srsNextToken

instance Lude.AWSRequest Search where
  type Rs Search = SearchResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchResponse'
            Lude.<$> (x Lude..?> "Results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Search where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("SageMaker.Search" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON Search where
  toJSON Search' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SearchExpression" Lude..=) Lude.<$> searchExpression,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy,
            Lude.Just ("Resource" Lude..= resource)
          ]
      )

instance Lude.ToPath Search where
  toPath = Lude.const "/"

instance Lude.ToQuery Search where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchResponse' smart constructor.
data SearchResponse = SearchResponse'
  { results ::
      Lude.Maybe [SearchRecord],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the result of the previous @Search@ request was truncated, the response includes a NextToken. To retrieve the next set of results, use the token in the next request.
-- * 'responseStatus' - The response status code.
-- * 'results' - A list of @SearchRecord@ objects.
mkSearchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchResponse
mkSearchResponse pResponseStatus_ =
  SearchResponse'
    { results = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @SearchRecord@ objects.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResults :: Lens.Lens' SearchResponse (Lude.Maybe [SearchRecord])
srsResults = Lens.lens (results :: SearchResponse -> Lude.Maybe [SearchRecord]) (\s a -> s {results = a} :: SearchResponse)
{-# DEPRECATED srsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | If the result of the previous @Search@ request was truncated, the response includes a NextToken. To retrieve the next set of results, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsNextToken :: Lens.Lens' SearchResponse (Lude.Maybe Lude.Text)
srsNextToken = Lens.lens (nextToken :: SearchResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchResponse)
{-# DEPRECATED srsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' SearchResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: SearchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
