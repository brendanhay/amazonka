{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeAvailablePatches (..),
    mkDescribeAvailablePatches,

    -- ** Request lenses
    dapFilters,
    dapMaxResults,
    dapNextToken,

    -- * Destructuring the response
    DescribeAvailablePatchesResponse (..),
    mkDescribeAvailablePatchesResponse,

    -- ** Response lenses
    daprrsNextToken,
    daprrsPatches,
    daprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeAvailablePatches' smart constructor.
data DescribeAvailablePatches = DescribeAvailablePatches'
  { -- | Filters used to scope down the returned patches.
    filters :: Core.Maybe [Types.PatchOrchestratorFilter],
    -- | The maximum number of patches to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAvailablePatches' value with any optional fields omitted.
mkDescribeAvailablePatches ::
  DescribeAvailablePatches
mkDescribeAvailablePatches =
  DescribeAvailablePatches'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters used to scope down the returned patches.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapFilters :: Lens.Lens' DescribeAvailablePatches (Core.Maybe [Types.PatchOrchestratorFilter])
dapFilters = Lens.field @"filters"
{-# DEPRECATED dapFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapMaxResults :: Lens.Lens' DescribeAvailablePatches (Core.Maybe Core.Natural)
dapMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dapMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dapNextToken :: Lens.Lens' DescribeAvailablePatches (Core.Maybe Types.NextToken)
dapNextToken = Lens.field @"nextToken"
{-# DEPRECATED dapNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAvailablePatches where
  toJSON DescribeAvailablePatches {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeAvailablePatches where
  type Rs DescribeAvailablePatches = DescribeAvailablePatchesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribeAvailablePatches")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAvailablePatchesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Patches")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAvailablePatches where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"patches" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAvailablePatchesResponse' smart constructor.
data DescribeAvailablePatchesResponse = DescribeAvailablePatchesResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | An array of patches. Each entry in the array is a patch structure.
    patches :: Core.Maybe [Types.Patch],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAvailablePatchesResponse' value with any optional fields omitted.
mkDescribeAvailablePatchesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAvailablePatchesResponse
mkDescribeAvailablePatchesResponse responseStatus =
  DescribeAvailablePatchesResponse'
    { nextToken = Core.Nothing,
      patches = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsNextToken :: Lens.Lens' DescribeAvailablePatchesResponse (Core.Maybe Types.NextToken)
daprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED daprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of patches. Each entry in the array is a patch structure.
--
-- /Note:/ Consider using 'patches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsPatches :: Lens.Lens' DescribeAvailablePatchesResponse (Core.Maybe [Types.Patch])
daprrsPatches = Lens.field @"patches"
{-# DEPRECATED daprrsPatches "Use generic-lens or generic-optics with 'patches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daprrsResponseStatus :: Lens.Lens' DescribeAvailablePatchesResponse Core.Int
daprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
