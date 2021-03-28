{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListWorkerBlocks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListWorkersBlocks@ operation retrieves a list of Workers who are blocked from working on your HITs.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListWorkerBlocks
    (
    -- * Creating a request
      ListWorkerBlocks (..)
    , mkListWorkerBlocks
    -- ** Request lenses
    , lwbMaxResults
    , lwbNextToken

    -- * Destructuring the response
    , ListWorkerBlocksResponse (..)
    , mkListWorkerBlocksResponse
    -- ** Response lenses
    , lwbrrsNextToken
    , lwbrrsNumResults
    , lwbrrsWorkerBlocks
    , lwbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListWorkerBlocks' smart constructor.
data ListWorkerBlocks = ListWorkerBlocks'
  { maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Pagination token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWorkerBlocks' value with any optional fields omitted.
mkListWorkerBlocks
    :: ListWorkerBlocks
mkListWorkerBlocks
  = ListWorkerBlocks'{maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbMaxResults :: Lens.Lens' ListWorkerBlocks (Core.Maybe Core.Natural)
lwbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lwbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbNextToken :: Lens.Lens' ListWorkerBlocks (Core.Maybe Types.PaginationToken)
lwbNextToken = Lens.field @"nextToken"
{-# INLINEABLE lwbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListWorkerBlocks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListWorkerBlocks where
        toHeaders ListWorkerBlocks{..}
          = Core.pure
              ("X-Amz-Target", "MTurkRequesterServiceV20170117.ListWorkerBlocks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListWorkerBlocks where
        toJSON ListWorkerBlocks{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListWorkerBlocks where
        type Rs ListWorkerBlocks = ListWorkerBlocksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListWorkerBlocksResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "NumResults" Core.<*>
                     x Core..:? "WorkerBlocks"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListWorkerBlocks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"workerBlocks" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListWorkerBlocksResponse' smart constructor.
data ListWorkerBlocksResponse = ListWorkerBlocksResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
  , numResults :: Core.Maybe Core.Int
    -- ^ The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
  , workerBlocks :: Core.Maybe [Types.WorkerBlock]
    -- ^ The list of WorkerBlocks, containing the collection of Worker IDs and reasons for blocking.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWorkerBlocksResponse' value with any optional fields omitted.
mkListWorkerBlocksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListWorkerBlocksResponse
mkListWorkerBlocksResponse responseStatus
  = ListWorkerBlocksResponse'{nextToken = Core.Nothing,
                              numResults = Core.Nothing, workerBlocks = Core.Nothing,
                              responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbrrsNextToken :: Lens.Lens' ListWorkerBlocksResponse (Core.Maybe Types.PaginationToken)
lwbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lwbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbrrsNumResults :: Lens.Lens' ListWorkerBlocksResponse (Core.Maybe Core.Int)
lwbrrsNumResults = Lens.field @"numResults"
{-# INLINEABLE lwbrrsNumResults #-}
{-# DEPRECATED numResults "Use generic-lens or generic-optics with 'numResults' instead"  #-}

-- | The list of WorkerBlocks, containing the collection of Worker IDs and reasons for blocking.
--
-- /Note:/ Consider using 'workerBlocks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbrrsWorkerBlocks :: Lens.Lens' ListWorkerBlocksResponse (Core.Maybe [Types.WorkerBlock])
lwbrrsWorkerBlocks = Lens.field @"workerBlocks"
{-# INLINEABLE lwbrrsWorkerBlocks #-}
{-# DEPRECATED workerBlocks "Use generic-lens or generic-optics with 'workerBlocks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwbrrsResponseStatus :: Lens.Lens' ListWorkerBlocksResponse Core.Int
lwbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lwbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
