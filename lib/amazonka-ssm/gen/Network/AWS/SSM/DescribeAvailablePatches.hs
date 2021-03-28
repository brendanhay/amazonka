{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAvailablePatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patches eligible to be included in a patch baseline.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAvailablePatches
    (
    -- * Creating a request
      DescribeAvailablePatches (..)
    , mkDescribeAvailablePatches
    -- ** Request lenses
    , dapFilters
    , dapMaxResults
    , dapNextToken

    -- * Destructuring the response
    , DescribeAvailablePatchesResponse (..)
    , mkDescribeAvailablePatchesResponse
    -- ** Response lenses
    , daprrsNextToken
    , daprrsPatches
    , daprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeAvailablePatches' smart constructor.
data DescribeAvailablePatches = DescribeAvailablePatches'
  { filters :: Core.Maybe [Types.PatchOrchestratorFilter]
    -- ^ Filters used to scope down the returned patches.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of patches to return (per page).
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAvailablePatches' value with any optional fields omitted.
mkDescribeAvailablePatches
    :: DescribeAvailablePatches
mkDescribeAvailablePatches
  = DescribeAvailablePatches'{filters = Core.Nothing,
                              maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Filters used to scope down the returned patches.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapFilters :: Lens.Lens' DescribeAvailablePatches (Core.Maybe [Types.PatchOrchestratorFilter])
dapFilters = Lens.field @"filters"
{-# INLINEABLE dapFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapMaxResults :: Lens.Lens' DescribeAvailablePatches (Core.Maybe Core.Natural)
dapMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dapMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapNextToken :: Lens.Lens' DescribeAvailablePatches (Core.Maybe Types.NextToken)
dapNextToken = Lens.field @"nextToken"
{-# INLINEABLE dapNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeAvailablePatches where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAvailablePatches where
        toHeaders DescribeAvailablePatches{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DescribeAvailablePatches")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAvailablePatches where
        toJSON DescribeAvailablePatches{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeAvailablePatches where
        type Rs DescribeAvailablePatches = DescribeAvailablePatchesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAvailablePatchesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Patches" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeAvailablePatches where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"patches" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeAvailablePatchesResponse' smart constructor.
data DescribeAvailablePatchesResponse = DescribeAvailablePatchesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , patches :: Core.Maybe [Types.Patch]
    -- ^ An array of patches. Each entry in the array is a patch structure.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAvailablePatchesResponse' value with any optional fields omitted.
mkDescribeAvailablePatchesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAvailablePatchesResponse
mkDescribeAvailablePatchesResponse responseStatus
  = DescribeAvailablePatchesResponse'{nextToken = Core.Nothing,
                                      patches = Core.Nothing, responseStatus}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsNextToken :: Lens.Lens' DescribeAvailablePatchesResponse (Core.Maybe Types.NextToken)
daprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE daprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of patches. Each entry in the array is a patch structure.
--
-- /Note:/ Consider using 'patches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsPatches :: Lens.Lens' DescribeAvailablePatchesResponse (Core.Maybe [Types.Patch])
daprrsPatches = Lens.field @"patches"
{-# INLINEABLE daprrsPatches #-}
{-# DEPRECATED patches "Use generic-lens or generic-optics with 'patches' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsResponseStatus :: Lens.Lens' DescribeAvailablePatchesResponse Core.Int
daprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
