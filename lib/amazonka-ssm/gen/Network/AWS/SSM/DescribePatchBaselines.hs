{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchBaselines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the patch baselines in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchBaselines
    (
    -- * Creating a request
      DescribePatchBaselines (..)
    , mkDescribePatchBaselines
    -- ** Request lenses
    , dpbFilters
    , dpbMaxResults
    , dpbNextToken

    -- * Destructuring the response
    , DescribePatchBaselinesResponse (..)
    , mkDescribePatchBaselinesResponse
    -- ** Response lenses
    , dpbrfrsBaselineIdentities
    , dpbrfrsNextToken
    , dpbrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribePatchBaselines' smart constructor.
data DescribePatchBaselines = DescribePatchBaselines'
  { filters :: Core.Maybe [Types.PatchOrchestratorFilter]
    -- ^ Each element in the array is a structure containing: 
--
-- Key: (string, "NAME_PREFIX" or "OWNER")
-- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of patch baselines to return (per page).
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchBaselines' value with any optional fields omitted.
mkDescribePatchBaselines
    :: DescribePatchBaselines
mkDescribePatchBaselines
  = DescribePatchBaselines'{filters = Core.Nothing,
                            maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Each element in the array is a structure containing: 
--
-- Key: (string, "NAME_PREFIX" or "OWNER")
-- Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbFilters :: Lens.Lens' DescribePatchBaselines (Core.Maybe [Types.PatchOrchestratorFilter])
dpbFilters = Lens.field @"filters"
{-# INLINEABLE dpbFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of patch baselines to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbMaxResults :: Lens.Lens' DescribePatchBaselines (Core.Maybe Core.Natural)
dpbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dpbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbNextToken :: Lens.Lens' DescribePatchBaselines (Core.Maybe Types.NextToken)
dpbNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribePatchBaselines where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePatchBaselines where
        toHeaders DescribePatchBaselines{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DescribePatchBaselines")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePatchBaselines where
        toJSON DescribePatchBaselines{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribePatchBaselines where
        type Rs DescribePatchBaselines = DescribePatchBaselinesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePatchBaselinesResponse' Core.<$>
                   (x Core..:? "BaselineIdentities") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribePatchBaselines where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"baselineIdentities" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribePatchBaselinesResponse' smart constructor.
data DescribePatchBaselinesResponse = DescribePatchBaselinesResponse'
  { baselineIdentities :: Core.Maybe [Types.PatchBaselineIdentity]
    -- ^ An array of PatchBaselineIdentity elements.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchBaselinesResponse' value with any optional fields omitted.
mkDescribePatchBaselinesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePatchBaselinesResponse
mkDescribePatchBaselinesResponse responseStatus
  = DescribePatchBaselinesResponse'{baselineIdentities =
                                      Core.Nothing,
                                    nextToken = Core.Nothing, responseStatus}

-- | An array of PatchBaselineIdentity elements.
--
-- /Note:/ Consider using 'baselineIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrfrsBaselineIdentities :: Lens.Lens' DescribePatchBaselinesResponse (Core.Maybe [Types.PatchBaselineIdentity])
dpbrfrsBaselineIdentities = Lens.field @"baselineIdentities"
{-# INLINEABLE dpbrfrsBaselineIdentities #-}
{-# DEPRECATED baselineIdentities "Use generic-lens or generic-optics with 'baselineIdentities' instead"  #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrfrsNextToken :: Lens.Lens' DescribePatchBaselinesResponse (Core.Maybe Types.NextToken)
dpbrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpbrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpbrfrsResponseStatus :: Lens.Lens' DescribePatchBaselinesResponse Core.Int
dpbrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpbrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
