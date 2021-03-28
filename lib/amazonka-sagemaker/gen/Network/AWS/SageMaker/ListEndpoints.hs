{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoints.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEndpoints
    (
    -- * Creating a request
      ListEndpoints (..)
    , mkListEndpoints
    -- ** Request lenses
    , lesCreationTimeAfter
    , lesCreationTimeBefore
    , lesLastModifiedTimeAfter
    , lesLastModifiedTimeBefore
    , lesMaxResults
    , lesNameContains
    , lesNextToken
    , lesSortBy
    , lesSortOrder
    , lesStatusEquals

    -- * Destructuring the response
    , ListEndpointsResponse (..)
    , mkListEndpointsResponse
    -- ** Response lenses
    , lerfrsEndpoints
    , lerfrsNextToken
    , lerfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only endpoints that were created before the specified time (timestamp).
  , lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only endpoints that were modified after the specified timestamp. 
  , lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only endpoints that were modified before the specified timestamp. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of endpoints to return in the response.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
  , sortBy :: Core.Maybe Types.EndpointSortKey
    -- ^ Sorts the list of results. The default is @CreationTime@ .
  , sortOrder :: Core.Maybe Types.OrderKey
    -- ^ The sort order for results. The default is @Descending@ .
  , statusEquals :: Core.Maybe Types.EndpointStatus
    -- ^ A filter that returns only endpoints with the specified status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEndpoints' value with any optional fields omitted.
mkListEndpoints
    :: ListEndpoints
mkListEndpoints
  = ListEndpoints'{creationTimeAfter = Core.Nothing,
                   creationTimeBefore = Core.Nothing,
                   lastModifiedTimeAfter = Core.Nothing,
                   lastModifiedTimeBefore = Core.Nothing, maxResults = Core.Nothing,
                   nameContains = Core.Nothing, nextToken = Core.Nothing,
                   sortBy = Core.Nothing, sortOrder = Core.Nothing,
                   statusEquals = Core.Nothing}

-- | A filter that returns only endpoints with a creation time greater than or equal to the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesCreationTimeAfter :: Lens.Lens' ListEndpoints (Core.Maybe Core.NominalDiffTime)
lesCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lesCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only endpoints that were created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesCreationTimeBefore :: Lens.Lens' ListEndpoints (Core.Maybe Core.NominalDiffTime)
lesCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lesCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | A filter that returns only endpoints that were modified after the specified timestamp. 
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesLastModifiedTimeAfter :: Lens.Lens' ListEndpoints (Core.Maybe Core.NominalDiffTime)
lesLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# INLINEABLE lesLastModifiedTimeAfter #-}
{-# DEPRECATED lastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead"  #-}

-- | A filter that returns only endpoints that were modified before the specified timestamp. 
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesLastModifiedTimeBefore :: Lens.Lens' ListEndpoints (Core.Maybe Core.NominalDiffTime)
lesLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# INLINEABLE lesLastModifiedTimeBefore #-}
{-# DEPRECATED lastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead"  #-}

-- | The maximum number of endpoints to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesMaxResults :: Lens.Lens' ListEndpoints (Core.Maybe Core.Natural)
lesMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lesMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A string in endpoint names. This filter returns only endpoints whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNameContains :: Lens.Lens' ListEndpoints (Core.Maybe Types.NameContains)
lesNameContains = Lens.field @"nameContains"
{-# INLINEABLE lesNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the result of a @ListEndpoints@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoints, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNextToken :: Lens.Lens' ListEndpoints (Core.Maybe Types.PaginationToken)
lesNextToken = Lens.field @"nextToken"
{-# INLINEABLE lesNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Sorts the list of results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesSortBy :: Lens.Lens' ListEndpoints (Core.Maybe Types.EndpointSortKey)
lesSortBy = Lens.field @"sortBy"
{-# INLINEABLE lesSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for results. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesSortOrder :: Lens.Lens' ListEndpoints (Core.Maybe Types.OrderKey)
lesSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lesSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | A filter that returns only endpoints with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesStatusEquals :: Lens.Lens' ListEndpoints (Core.Maybe Types.EndpointStatus)
lesStatusEquals = Lens.field @"statusEquals"
{-# INLINEABLE lesStatusEquals #-}
{-# DEPRECATED statusEquals "Use generic-lens or generic-optics with 'statusEquals' instead"  #-}

instance Core.ToQuery ListEndpoints where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListEndpoints where
        toHeaders ListEndpoints{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListEndpoints") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListEndpoints where
        toJSON ListEndpoints{..}
          = Core.object
              (Core.catMaybes
                 [("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
                  ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NameContains" Core..=) Core.<$> nameContains,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder,
                  ("StatusEquals" Core..=) Core.<$> statusEquals])

instance Core.AWSRequest ListEndpoints where
        type Rs ListEndpoints = ListEndpointsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListEndpointsResponse' Core.<$>
                   (x Core..:? "Endpoints" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListEndpoints where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"endpoints") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { endpoints :: [Types.EndpointSummary]
    -- ^ An array or endpoint objects. 
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEndpointsResponse' value with any optional fields omitted.
mkListEndpointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListEndpointsResponse
mkListEndpointsResponse responseStatus
  = ListEndpointsResponse'{endpoints = Core.mempty,
                           nextToken = Core.Nothing, responseStatus}

-- | An array or endpoint objects. 
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerfrsEndpoints :: Lens.Lens' ListEndpointsResponse [Types.EndpointSummary]
lerfrsEndpoints = Lens.field @"endpoints"
{-# INLINEABLE lerfrsEndpoints #-}
{-# DEPRECATED endpoints "Use generic-lens or generic-optics with 'endpoints' instead"  #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerfrsNextToken :: Lens.Lens' ListEndpointsResponse (Core.Maybe Types.PaginationToken)
lerfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lerfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerfrsResponseStatus :: Lens.Lens' ListEndpointsResponse Core.Int
lerfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lerfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
