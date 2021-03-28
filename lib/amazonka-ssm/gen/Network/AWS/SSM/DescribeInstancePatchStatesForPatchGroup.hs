{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state for the instances in the specified patch group.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
    (
    -- * Creating a request
      DescribeInstancePatchStatesForPatchGroup (..)
    , mkDescribeInstancePatchStatesForPatchGroup
    -- ** Request lenses
    , dipsfpgPatchGroup
    , dipsfpgFilters
    , dipsfpgMaxResults
    , dipsfpgNextToken

    -- * Destructuring the response
    , DescribeInstancePatchStatesForPatchGroupResponse (..)
    , mkDescribeInstancePatchStatesForPatchGroupResponse
    -- ** Response lenses
    , dipsfpgrrsInstancePatchStates
    , dipsfpgrrsNextToken
    , dipsfpgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeInstancePatchStatesForPatchGroup' smart constructor.
data DescribeInstancePatchStatesForPatchGroup = DescribeInstancePatchStatesForPatchGroup'
  { patchGroup :: Types.PatchGroup
    -- ^ The name of the patch group for which the patch state information should be retrieved.
  , filters :: Core.Maybe [Types.InstancePatchStateFilter]
    -- ^ Each entry in the array is a structure containing:
--
-- Key (string between 1 and 200 characters)
-- Values (array containing a single string)
-- Type (string "Equal", "NotEqual", "LessThan", "GreaterThan")
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of patches to return (per page).
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstancePatchStatesForPatchGroup' value with any optional fields omitted.
mkDescribeInstancePatchStatesForPatchGroup
    :: Types.PatchGroup -- ^ 'patchGroup'
    -> DescribeInstancePatchStatesForPatchGroup
mkDescribeInstancePatchStatesForPatchGroup patchGroup
  = DescribeInstancePatchStatesForPatchGroup'{patchGroup,
                                              filters = Core.Nothing, maxResults = Core.Nothing,
                                              nextToken = Core.Nothing}

-- | The name of the patch group for which the patch state information should be retrieved.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgPatchGroup :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup Types.PatchGroup
dipsfpgPatchGroup = Lens.field @"patchGroup"
{-# INLINEABLE dipsfpgPatchGroup #-}
{-# DEPRECATED patchGroup "Use generic-lens or generic-optics with 'patchGroup' instead"  #-}

-- | Each entry in the array is a structure containing:
--
-- Key (string between 1 and 200 characters)
-- Values (array containing a single string)
-- Type (string "Equal", "NotEqual", "LessThan", "GreaterThan")
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgFilters :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Core.Maybe [Types.InstancePatchStateFilter])
dipsfpgFilters = Lens.field @"filters"
{-# INLINEABLE dipsfpgFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgMaxResults :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Core.Maybe Core.Natural)
dipsfpgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dipsfpgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgNextToken :: Lens.Lens' DescribeInstancePatchStatesForPatchGroup (Core.Maybe Types.NextToken)
dipsfpgNextToken = Lens.field @"nextToken"
{-# INLINEABLE dipsfpgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInstancePatchStatesForPatchGroup
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeInstancePatchStatesForPatchGroup
         where
        toHeaders DescribeInstancePatchStatesForPatchGroup{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonSSM.DescribeInstancePatchStatesForPatchGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeInstancePatchStatesForPatchGroup
         where
        toJSON DescribeInstancePatchStatesForPatchGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PatchGroup" Core..= patchGroup),
                  ("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeInstancePatchStatesForPatchGroup
         where
        type Rs DescribeInstancePatchStatesForPatchGroup =
             DescribeInstancePatchStatesForPatchGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInstancePatchStatesForPatchGroupResponse' Core.<$>
                   (x Core..:? "InstancePatchStates") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeInstancePatchStatesForPatchGroup
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"instancePatchStates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeInstancePatchStatesForPatchGroupResponse' smart constructor.
data DescribeInstancePatchStatesForPatchGroupResponse = DescribeInstancePatchStatesForPatchGroupResponse'
  { instancePatchStates :: Core.Maybe (Core.NonEmpty Types.InstancePatchState)
    -- ^ The high-level patch state for the requested instances. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeInstancePatchStatesForPatchGroupResponse' value with any optional fields omitted.
mkDescribeInstancePatchStatesForPatchGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstancePatchStatesForPatchGroupResponse
mkDescribeInstancePatchStatesForPatchGroupResponse responseStatus
  = DescribeInstancePatchStatesForPatchGroupResponse'{instancePatchStates
                                                        = Core.Nothing,
                                                      nextToken = Core.Nothing, responseStatus}

-- | The high-level patch state for the requested instances. 
--
-- /Note:/ Consider using 'instancePatchStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgrrsInstancePatchStates :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse (Core.Maybe (Core.NonEmpty Types.InstancePatchState))
dipsfpgrrsInstancePatchStates = Lens.field @"instancePatchStates"
{-# INLINEABLE dipsfpgrrsInstancePatchStates #-}
{-# DEPRECATED instancePatchStates "Use generic-lens or generic-optics with 'instancePatchStates' instead"  #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgrrsNextToken :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse (Core.Maybe Types.NextToken)
dipsfpgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dipsfpgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsfpgrrsResponseStatus :: Lens.Lens' DescribeInstancePatchStatesForPatchGroupResponse Core.Int
dipsfpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dipsfpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
