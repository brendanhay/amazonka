{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListWorkforces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to list all private and vendor workforces in an AWS Region. Note that you can only have one private workforce per AWS Region.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListWorkforces
    (
    -- * Creating a request
      ListWorkforces (..)
    , mkListWorkforces
    -- ** Request lenses
    , lwsMaxResults
    , lwsNameContains
    , lwsNextToken
    , lwsSortBy
    , lwsSortOrder

    -- * Destructuring the response
    , ListWorkforcesResponse (..)
    , mkListWorkforcesResponse
    -- ** Response lenses
    , lwrrsWorkforces
    , lwrrsNextToken
    , lwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListWorkforces' smart constructor.
data ListWorkforces = ListWorkforces'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of workforces returned in the response.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ A filter you can use to search for workforces using part of the workforce name.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to resume pagination.
  , sortBy :: Core.Maybe Types.ListWorkforcesSortByOptions
    -- ^ Sort workforces using the workforce name or creation date.
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ Sort workforces in ascending or descending order.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListWorkforces' value with any optional fields omitted.
mkListWorkforces
    :: ListWorkforces
mkListWorkforces
  = ListWorkforces'{maxResults = Core.Nothing,
                    nameContains = Core.Nothing, nextToken = Core.Nothing,
                    sortBy = Core.Nothing, sortOrder = Core.Nothing}

-- | The maximum number of workforces returned in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsMaxResults :: Lens.Lens' ListWorkforces (Core.Maybe Core.Natural)
lwsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lwsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A filter you can use to search for workforces using part of the workforce name.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsNameContains :: Lens.Lens' ListWorkforces (Core.Maybe Types.NameContains)
lwsNameContains = Lens.field @"nameContains"
{-# INLINEABLE lwsNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsNextToken :: Lens.Lens' ListWorkforces (Core.Maybe Types.NextToken)
lwsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lwsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Sort workforces using the workforce name or creation date.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsSortBy :: Lens.Lens' ListWorkforces (Core.Maybe Types.ListWorkforcesSortByOptions)
lwsSortBy = Lens.field @"sortBy"
{-# INLINEABLE lwsSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | Sort workforces in ascending or descending order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwsSortOrder :: Lens.Lens' ListWorkforces (Core.Maybe Types.SortOrder)
lwsSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lwsSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListWorkforces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListWorkforces where
        toHeaders ListWorkforces{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListWorkforces") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListWorkforces where
        toJSON ListWorkforces{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NameContains" Core..=) Core.<$> nameContains,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListWorkforces where
        type Rs ListWorkforces = ListWorkforcesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListWorkforcesResponse' Core.<$>
                   (x Core..:? "Workforces" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListWorkforces where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"workforces") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListWorkforcesResponse' smart constructor.
data ListWorkforcesResponse = ListWorkforcesResponse'
  { workforces :: [Types.Workforce]
    -- ^ A list containing information about your workforce.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to resume pagination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListWorkforcesResponse' value with any optional fields omitted.
mkListWorkforcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListWorkforcesResponse
mkListWorkforcesResponse responseStatus
  = ListWorkforcesResponse'{workforces = Core.mempty,
                            nextToken = Core.Nothing, responseStatus}

-- | A list containing information about your workforce.
--
-- /Note:/ Consider using 'workforces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrrsWorkforces :: Lens.Lens' ListWorkforcesResponse [Types.Workforce]
lwrrsWorkforces = Lens.field @"workforces"
{-# INLINEABLE lwrrsWorkforces #-}
{-# DEPRECATED workforces "Use generic-lens or generic-optics with 'workforces' instead"  #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrrsNextToken :: Lens.Lens' ListWorkforcesResponse (Core.Maybe Types.NextToken)
lwrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lwrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lwrrsResponseStatus :: Lens.Lens' ListWorkforcesResponse Core.Int
lwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
