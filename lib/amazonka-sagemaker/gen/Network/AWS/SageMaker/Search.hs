{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      Search (..)
    , mkSearch
    -- ** Request lenses
    , sResource
    , sMaxResults
    , sNextToken
    , sSearchExpression
    , sSortBy
    , sSortOrder

    -- * Destructuring the response
    , SearchResponse (..)
    , mkSearchResponse
    -- ** Response lenses
    , srrsNextToken
    , srrsResults
    , srrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkSearch' smart constructor.
data Search = Search'
  { resource :: Types.ResourceType
    -- ^ The name of the Amazon SageMaker resource to search for.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If more than @MaxResults@ resources match the specified @SearchExpression@ , the response includes a @NextToken@ . The @NextToken@ can be passed to the next @SearchRequest@ to continue retrieving results.
  , searchExpression :: Core.Maybe Types.SearchExpression
    -- ^ A Boolean conditional statement. Resources must satisfy this condition to be included in search results. You must provide at least one subexpression, filter, or nested filter. The maximum number of recursive @SubExpressions@ , @NestedFilters@ , and @Filters@ that can be included in a @SearchExpression@ object is 50.
  , sortBy :: Core.Maybe Types.ResourcePropertyName
    -- ^ The name of the resource property used to sort the @SearchResults@ . The default is @LastModifiedTime@ .
  , sortOrder :: Core.Maybe Types.SearchSortOrder
    -- ^ How @SearchResults@ are ordered. Valid values are @Ascending@ or @Descending@ . The default is @Descending@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Search' value with any optional fields omitted.
mkSearch
    :: Types.ResourceType -- ^ 'resource'
    -> Search
mkSearch resource
  = Search'{resource, maxResults = Core.Nothing,
            nextToken = Core.Nothing, searchExpression = Core.Nothing,
            sortBy = Core.Nothing, sortOrder = Core.Nothing}

-- | The name of the Amazon SageMaker resource to search for.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResource :: Lens.Lens' Search Types.ResourceType
sResource = Lens.field @"resource"
{-# INLINEABLE sResource #-}
{-# DEPRECATED resource "Use generic-lens or generic-optics with 'resource' instead"  #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMaxResults :: Lens.Lens' Search (Core.Maybe Core.Natural)
sMaxResults = Lens.field @"maxResults"
{-# INLINEABLE sMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If more than @MaxResults@ resources match the specified @SearchExpression@ , the response includes a @NextToken@ . The @NextToken@ can be passed to the next @SearchRequest@ to continue retrieving results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNextToken :: Lens.Lens' Search (Core.Maybe Types.NextToken)
sNextToken = Lens.field @"nextToken"
{-# INLINEABLE sNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A Boolean conditional statement. Resources must satisfy this condition to be included in search results. You must provide at least one subexpression, filter, or nested filter. The maximum number of recursive @SubExpressions@ , @NestedFilters@ , and @Filters@ that can be included in a @SearchExpression@ object is 50.
--
-- /Note:/ Consider using 'searchExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSearchExpression :: Lens.Lens' Search (Core.Maybe Types.SearchExpression)
sSearchExpression = Lens.field @"searchExpression"
{-# INLINEABLE sSearchExpression #-}
{-# DEPRECATED searchExpression "Use generic-lens or generic-optics with 'searchExpression' instead"  #-}

-- | The name of the resource property used to sort the @SearchResults@ . The default is @LastModifiedTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSortBy :: Lens.Lens' Search (Core.Maybe Types.ResourcePropertyName)
sSortBy = Lens.field @"sortBy"
{-# INLINEABLE sSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | How @SearchResults@ are ordered. Valid values are @Ascending@ or @Descending@ . The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSortOrder :: Lens.Lens' Search (Core.Maybe Types.SearchSortOrder)
sSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE sSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery Search where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders Search where
        toHeaders Search{..}
          = Core.pure ("X-Amz-Target", "SageMaker.Search") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON Search where
        toJSON Search{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Resource" Core..= resource),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SearchExpression" Core..=) Core.<$> searchExpression,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest Search where
        type Rs Search = SearchResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Results" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager Search where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"results" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkSearchResponse' smart constructor.
data SearchResponse = SearchResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous @Search@ request was truncated, the response includes a NextToken. To retrieve the next set of results, use the token in the next request.
  , results :: Core.Maybe [Types.SearchRecord]
    -- ^ A list of @SearchRecord@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SearchResponse' value with any optional fields omitted.
mkSearchResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchResponse
mkSearchResponse responseStatus
  = SearchResponse'{nextToken = Core.Nothing, results = Core.Nothing,
                    responseStatus}

-- | If the result of the previous @Search@ request was truncated, the response includes a NextToken. To retrieve the next set of results, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsNextToken :: Lens.Lens' SearchResponse (Core.Maybe Types.NextToken)
srrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE srrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @SearchRecord@ objects.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResults :: Lens.Lens' SearchResponse (Core.Maybe [Types.SearchRecord])
srrsResults = Lens.field @"results"
{-# INLINEABLE srrsResults #-}
{-# DEPRECATED results "Use generic-lens or generic-optics with 'results' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' SearchResponse Core.Int
srrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
