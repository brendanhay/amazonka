{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListTagOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified TagOptions or all TagOptions.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListTagOptions
    (
    -- * Creating a request
      ListTagOptions (..)
    , mkListTagOptions
    -- ** Request lenses
    , ltoFilters
    , ltoPageSize
    , ltoPageToken

    -- * Destructuring the response
    , ListTagOptionsResponse (..)
    , mkListTagOptionsResponse
    -- ** Response lenses
    , ltorrsPageToken
    , ltorrsTagOptionDetails
    , ltorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListTagOptions' smart constructor.
data ListTagOptions = ListTagOptions'
  { filters :: Core.Maybe Types.ListTagOptionsFilters
    -- ^ The search filters. If no search filters are specified, the output includes all TagOptions.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagOptions' value with any optional fields omitted.
mkListTagOptions
    :: ListTagOptions
mkListTagOptions
  = ListTagOptions'{filters = Core.Nothing, pageSize = Core.Nothing,
                    pageToken = Core.Nothing}

-- | The search filters. If no search filters are specified, the output includes all TagOptions.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoFilters :: Lens.Lens' ListTagOptions (Core.Maybe Types.ListTagOptionsFilters)
ltoFilters = Lens.field @"filters"
{-# INLINEABLE ltoFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoPageSize :: Lens.Lens' ListTagOptions (Core.Maybe Core.Natural)
ltoPageSize = Lens.field @"pageSize"
{-# INLINEABLE ltoPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoPageToken :: Lens.Lens' ListTagOptions (Core.Maybe Types.PageToken)
ltoPageToken = Lens.field @"pageToken"
{-# INLINEABLE ltoPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListTagOptions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagOptions where
        toHeaders ListTagOptions{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.ListTagOptions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagOptions where
        toJSON ListTagOptions{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListTagOptions where
        type Rs ListTagOptions = ListTagOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagOptionsResponse' Core.<$>
                   (x Core..:? "PageToken") Core.<*> x Core..:? "TagOptionDetails"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTagOptions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"pageToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"tagOptionDetails" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~ rs Lens.^. Lens.field @"pageToken")

-- | /See:/ 'mkListTagOptionsResponse' smart constructor.
data ListTagOptionsResponse = ListTagOptionsResponse'
  { pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  , tagOptionDetails :: Core.Maybe [Types.TagOptionDetail]
    -- ^ Information about the TagOptions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagOptionsResponse' value with any optional fields omitted.
mkListTagOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagOptionsResponse
mkListTagOptionsResponse responseStatus
  = ListTagOptionsResponse'{pageToken = Core.Nothing,
                            tagOptionDetails = Core.Nothing, responseStatus}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorrsPageToken :: Lens.Lens' ListTagOptionsResponse (Core.Maybe Types.PageToken)
ltorrsPageToken = Lens.field @"pageToken"
{-# INLINEABLE ltorrsPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

-- | Information about the TagOptions.
--
-- /Note:/ Consider using 'tagOptionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorrsTagOptionDetails :: Lens.Lens' ListTagOptionsResponse (Core.Maybe [Types.TagOptionDetail])
ltorrsTagOptionDetails = Lens.field @"tagOptionDetails"
{-# INLINEABLE ltorrsTagOptionDetails #-}
{-# DEPRECATED tagOptionDetails "Use generic-lens or generic-optics with 'tagOptionDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorrsResponseStatus :: Lens.Lens' ListTagOptionsResponse Core.Int
ltorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
