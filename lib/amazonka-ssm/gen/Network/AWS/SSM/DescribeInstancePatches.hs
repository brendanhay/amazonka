{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstancePatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the patches on the specified instance and their state relative to the patch baseline being used for the instance.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatches
    (
    -- * Creating a request
      DescribeInstancePatches (..)
    , mkDescribeInstancePatches
    -- ** Request lenses
    , dipInstanceId
    , dipFilters
    , dipMaxResults
    , dipNextToken

    -- * Destructuring the response
    , DescribeInstancePatchesResponse (..)
    , mkDescribeInstancePatchesResponse
    -- ** Response lenses
    , diprrsNextToken
    , diprrsPatches
    , diprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeInstancePatches' smart constructor.
data DescribeInstancePatches = DescribeInstancePatches'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance whose patch state information should be retrieved.
  , filters :: Core.Maybe [Types.PatchOrchestratorFilter]
    -- ^ An array of structures. Each entry in the array is a structure containing a Key, Value combination. Valid values for Key are @Classification@ | @KBId@ | @Severity@ | @State@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of patches to return (per page).
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstancePatches' value with any optional fields omitted.
mkDescribeInstancePatches
    :: Types.InstanceId -- ^ 'instanceId'
    -> DescribeInstancePatches
mkDescribeInstancePatches instanceId
  = DescribeInstancePatches'{instanceId, filters = Core.Nothing,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the instance whose patch state information should be retrieved.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipInstanceId :: Lens.Lens' DescribeInstancePatches Types.InstanceId
dipInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dipInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | An array of structures. Each entry in the array is a structure containing a Key, Value combination. Valid values for Key are @Classification@ | @KBId@ | @Severity@ | @State@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipFilters :: Lens.Lens' DescribeInstancePatches (Core.Maybe [Types.PatchOrchestratorFilter])
dipFilters = Lens.field @"filters"
{-# INLINEABLE dipFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipMaxResults :: Lens.Lens' DescribeInstancePatches (Core.Maybe Core.Natural)
dipMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dipMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipNextToken :: Lens.Lens' DescribeInstancePatches (Core.Maybe Types.NextToken)
dipNextToken = Lens.field @"nextToken"
{-# INLINEABLE dipNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInstancePatches where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeInstancePatches where
        toHeaders DescribeInstancePatches{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DescribeInstancePatches")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeInstancePatches where
        toJSON DescribeInstancePatches{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceId" Core..= instanceId),
                  ("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeInstancePatches where
        type Rs DescribeInstancePatches = DescribeInstancePatchesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInstancePatchesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Patches" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeInstancePatches where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"patches" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeInstancePatchesResponse' smart constructor.
data DescribeInstancePatchesResponse = DescribeInstancePatchesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , patches :: Core.Maybe [Types.PatchComplianceData]
    -- ^ Each entry in the array is a structure containing:
--
-- Title (string)
-- KBId (string)
-- Classification (string)
-- Severity (string)
-- State (string, such as "INSTALLED" or "FAILED")
-- InstalledTime (DateTime)
-- InstalledBy (string)
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeInstancePatchesResponse' value with any optional fields omitted.
mkDescribeInstancePatchesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstancePatchesResponse
mkDescribeInstancePatchesResponse responseStatus
  = DescribeInstancePatchesResponse'{nextToken = Core.Nothing,
                                     patches = Core.Nothing, responseStatus}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsNextToken :: Lens.Lens' DescribeInstancePatchesResponse (Core.Maybe Types.NextToken)
diprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE diprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Each entry in the array is a structure containing:
--
-- Title (string)
-- KBId (string)
-- Classification (string)
-- Severity (string)
-- State (string, such as "INSTALLED" or "FAILED")
-- InstalledTime (DateTime)
-- InstalledBy (string)
--
-- /Note:/ Consider using 'patches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsPatches :: Lens.Lens' DescribeInstancePatchesResponse (Core.Maybe [Types.PatchComplianceData])
diprrsPatches = Lens.field @"patches"
{-# INLINEABLE diprrsPatches #-}
{-# DEPRECATED patches "Use generic-lens or generic-optics with 'patches' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsResponseStatus :: Lens.Lens' DescribeInstancePatchesResponse Core.Int
diprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
