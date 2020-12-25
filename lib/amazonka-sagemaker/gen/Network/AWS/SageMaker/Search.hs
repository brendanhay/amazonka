{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    sResource,
    sMaxResults,
    sNextToken,
    sSearchExpression,
    sSortBy,
    sSortOrder,

    -- * Destructuring the response
    SearchResponse (..),
    mkSearchResponse,

    -- ** Response lenses
    srrsNextToken,
    srrsResults,
    srrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkSearch' smart constructor.
data Search = Search'
  { -- | The name of the Amazon SageMaker resource to search for.
    resource :: Types.ResourceType,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | If more than @MaxResults@ resources match the specified @SearchExpression@ , the response includes a @NextToken@ . The @NextToken@ can be passed to the next @SearchRequest@ to continue retrieving results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A Boolean conditional statement. Resources must satisfy this condition to be included in search results. You must provide at least one subexpression, filter, or nested filter. The maximum number of recursive @SubExpressions@ , @NestedFilters@ , and @Filters@ that can be included in a @SearchExpression@ object is 50.
    searchExpression :: Core.Maybe Types.SearchExpression,
    -- | The name of the resource property used to sort the @SearchResults@ . The default is @LastModifiedTime@ .
    sortBy :: Core.Maybe Types.ResourcePropertyName,
    -- | How @SearchResults@ are ordered. Valid values are @Ascending@ or @Descending@ . The default is @Descending@ .
    sortOrder :: Core.Maybe Types.SearchSortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Search' value with any optional fields omitted.
mkSearch ::
  -- | 'resource'
  Types.ResourceType ->
  Search
mkSearch resource =
  Search'
    { resource,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      searchExpression = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | The name of the Amazon SageMaker resource to search for.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResource :: Lens.Lens' Search Types.ResourceType
sResource = Lens.field @"resource"
{-# DEPRECATED sResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxResults :: Lens.Lens' Search (Core.Maybe Core.Natural)
sMaxResults = Lens.field @"maxResults"
{-# DEPRECATED sMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If more than @MaxResults@ resources match the specified @SearchExpression@ , the response includes a @NextToken@ . The @NextToken@ can be passed to the next @SearchRequest@ to continue retrieving results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNextToken :: Lens.Lens' Search (Core.Maybe Types.NextToken)
sNextToken = Lens.field @"nextToken"
{-# DEPRECATED sNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A Boolean conditional statement. Resources must satisfy this condition to be included in search results. You must provide at least one subexpression, filter, or nested filter. The maximum number of recursive @SubExpressions@ , @NestedFilters@ , and @Filters@ that can be included in a @SearchExpression@ object is 50.
--
-- /Note:/ Consider using 'searchExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSearchExpression :: Lens.Lens' Search (Core.Maybe Types.SearchExpression)
sSearchExpression = Lens.field @"searchExpression"
{-# DEPRECATED sSearchExpression "Use generic-lens or generic-optics with 'searchExpression' instead." #-}

-- | The name of the resource property used to sort the @SearchResults@ . The default is @LastModifiedTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSortBy :: Lens.Lens' Search (Core.Maybe Types.ResourcePropertyName)
sSortBy = Lens.field @"sortBy"
{-# DEPRECATED sSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | How @SearchResults@ are ordered. Valid values are @Ascending@ or @Descending@ . The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSortOrder :: Lens.Lens' Search (Core.Maybe Types.SearchSortOrder)
sSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED sSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON Search where
  toJSON Search {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Resource" Core..= resource),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SearchExpression" Core..=) Core.<$> searchExpression,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest Search where
  type Rs Search = SearchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.Search")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Results")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager Search where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"results" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkSearchResponse' smart constructor.
data SearchResponse = SearchResponse'
  { -- | If the result of the previous @Search@ request was truncated, the response includes a NextToken. To retrieve the next set of results, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @SearchRecord@ objects.
    results :: Core.Maybe [Types.SearchRecord],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SearchResponse' value with any optional fields omitted.
mkSearchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchResponse
mkSearchResponse responseStatus =
  SearchResponse'
    { nextToken = Core.Nothing,
      results = Core.Nothing,
      responseStatus
    }

-- | If the result of the previous @Search@ request was truncated, the response includes a NextToken. To retrieve the next set of results, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsNextToken :: Lens.Lens' SearchResponse (Core.Maybe Types.NextToken)
srrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED srrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @SearchRecord@ objects.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResults :: Lens.Lens' SearchResponse (Core.Maybe [Types.SearchRecord])
srrsResults = Lens.field @"results"
{-# DEPRECATED srrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' SearchResponse Core.Int
srrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
