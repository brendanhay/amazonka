{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of the current filters.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListFilters
    (
    -- * Creating a request
      ListFilters (..)
    , mkListFilters
    -- ** Request lenses
    , lDetectorId
    , lMaxResults
    , lNextToken

    -- * Destructuring the response
    , ListFiltersResponse (..)
    , mkListFiltersResponse
    -- ** Response lenses
    , lrsFilterNames
    , lrsNextToken
    , lrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFilters' smart constructor.
data ListFilters = ListFilters'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector that the filter is associated with.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
  , nextToken :: Core.Maybe Core.Text
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFilters' value with any optional fields omitted.
mkListFilters
    :: Types.DetectorId -- ^ 'detectorId'
    -> ListFilters
mkListFilters detectorId
  = ListFilters'{detectorId, maxResults = Core.Nothing,
                 nextToken = Core.Nothing}

-- | The unique ID of the detector that the filter is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDetectorId :: Lens.Lens' ListFilters Types.DetectorId
lDetectorId = Lens.field @"detectorId"
{-# INLINEABLE lDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListFilters (Core.Maybe Core.Natural)
lMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListFilters (Core.Maybe Core.Text)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListFilters where
        toQuery ListFilters{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListFilters where
        toHeaders ListFilters{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListFilters where
        type Rs ListFilters = ListFiltersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/filter",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListFiltersResponse' Core.<$>
                   (x Core..:? "filterNames" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListFilters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"filterNames") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListFiltersResponse' smart constructor.
data ListFiltersResponse = ListFiltersResponse'
  { filterNames :: [Types.FilterName]
    -- ^ A list of filter names.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination parameter to be used on the next list operation to retrieve more items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFiltersResponse' value with any optional fields omitted.
mkListFiltersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFiltersResponse
mkListFiltersResponse responseStatus
  = ListFiltersResponse'{filterNames = Core.mempty,
                         nextToken = Core.Nothing, responseStatus}

-- | A list of filter names.
--
-- /Note:/ Consider using 'filterNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsFilterNames :: Lens.Lens' ListFiltersResponse [Types.FilterName]
lrsFilterNames = Lens.field @"filterNames"
{-# INLINEABLE lrsFilterNames #-}
{-# DEPRECATED filterNames "Use generic-lens or generic-optics with 'filterNames' instead"  #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListFiltersResponse (Core.Maybe Core.Text)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListFiltersResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
