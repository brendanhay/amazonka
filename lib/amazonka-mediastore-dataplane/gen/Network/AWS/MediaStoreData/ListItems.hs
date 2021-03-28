{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.ListItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of metadata entries about folders and objects in the specified folder.
--
-- This operation returns paginated results.
module Network.AWS.MediaStoreData.ListItems
    (
    -- * Creating a request
      ListItems (..)
    , mkListItems
    -- ** Request lenses
    , liMaxResults
    , liNextToken
    , liPath

    -- * Destructuring the response
    , ListItemsResponse (..)
    , mkListItemsResponse
    -- ** Response lenses
    , lirrsItems
    , lirrsNextToken
    , lirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStoreData.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListItems' smart constructor.
data ListItems = ListItems'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return per API request. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. Although 2,000 items match your request, the service returns no more than the first 500 items. (The service also returns a @NextToken@ value that you can use to fetch the next batch of results.) The service might return fewer results than the @MaxResults@ value.
--
-- If @MaxResults@ is not included in the request, the service defaults to pagination with a maximum of 1,000 results per page.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token that identifies which batch of results that you want to see. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value. To see the next batch of results, you can submit the @ListItems@ request a second time and specify the @NextToken@ value.
--
-- Tokens expire after 15 minutes.
  , path :: Core.Maybe Types.Path
    -- ^ The path in the container from which to retrieve items. Format: <folder name>/<folder name>/<file name>
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListItems' value with any optional fields omitted.
mkListItems
    :: ListItems
mkListItems
  = ListItems'{maxResults = Core.Nothing, nextToken = Core.Nothing,
               path = Core.Nothing}

-- | The maximum number of results to return per API request. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. Although 2,000 items match your request, the service returns no more than the first 500 items. (The service also returns a @NextToken@ value that you can use to fetch the next batch of results.) The service might return fewer results than the @MaxResults@ value.
--
-- If @MaxResults@ is not included in the request, the service defaults to pagination with a maximum of 1,000 results per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListItems (Core.Maybe Core.Natural)
liMaxResults = Lens.field @"maxResults"
{-# INLINEABLE liMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that identifies which batch of results that you want to see. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value. To see the next batch of results, you can submit the @ListItems@ request a second time and specify the @NextToken@ value.
--
-- Tokens expire after 15 minutes.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListItems (Core.Maybe Types.PaginationToken)
liNextToken = Lens.field @"nextToken"
{-# INLINEABLE liNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The path in the container from which to retrieve items. Format: <folder name>/<folder name>/<file name>
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liPath :: Lens.Lens' ListItems (Core.Maybe Types.Path)
liPath = Lens.field @"path"
{-# INLINEABLE liPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.ToQuery ListItems where
        toQuery ListItems{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Path") path

instance Core.ToHeaders ListItems where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListItems where
        type Rs ListItems = ListItemsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListItemsResponse' Core.<$>
                   (x Core..:? "Items") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListItems where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListItemsResponse' smart constructor.
data ListItemsResponse = ListItemsResponse'
  { items :: Core.Maybe [Types.Item]
    -- ^ The metadata entries for the folders and objects at the requested path.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token that can be used in a request to view the next set of results. For example, you submit a @ListItems@ request that matches 2,000 items with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value that can be used to fetch the next batch of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListItemsResponse' value with any optional fields omitted.
mkListItemsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListItemsResponse
mkListItemsResponse responseStatus
  = ListItemsResponse'{items = Core.Nothing,
                       nextToken = Core.Nothing, responseStatus}

-- | The metadata entries for the folders and objects at the requested path.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsItems :: Lens.Lens' ListItemsResponse (Core.Maybe [Types.Item])
lirrsItems = Lens.field @"items"
{-# INLINEABLE lirrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | The token that can be used in a request to view the next set of results. For example, you submit a @ListItems@ request that matches 2,000 items with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value that can be used to fetch the next batch of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListItemsResponse (Core.Maybe Types.PaginationToken)
lirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListItemsResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
