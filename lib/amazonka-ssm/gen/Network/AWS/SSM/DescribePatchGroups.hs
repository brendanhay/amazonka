{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patch groups that have been registered with patch baselines.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchGroups
  ( -- * Creating a request
    DescribePatchGroups (..),
    mkDescribePatchGroups,

    -- ** Request lenses
    dpgFilters,
    dpgMaxResults,
    dpgNextToken,

    -- * Destructuring the response
    DescribePatchGroupsResponse (..),
    mkDescribePatchGroupsResponse,

    -- ** Response lenses
    dpgrrsMappings,
    dpgrrsNextToken,
    dpgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribePatchGroups' smart constructor.
data DescribePatchGroups = DescribePatchGroups'
  { -- | One or more filters. Use a filter to return a more specific list of results.
    --
    -- For @DescribePatchGroups@ ,valid filter keys include the following:
    --
    --     * @NAME_PREFIX@ : The name of the patch group. Wildcards (*) are accepted.
    --
    --
    --     * @OPERATING_SYSTEM@ : The supported operating system type to return results for. For valid operating system values, see 'GetDefaultPatchBaselineRequest$OperatingSystem' in 'CreatePatchBaseline' .
    -- Examples:
    --
    --     * @--filters Key=NAME_PREFIX,Values=MyPatchGroup*@
    --
    --
    --     * @--filters Key=OPERATING_SYSTEM,Values=AMAZON_LINUX_2@
    filters :: Core.Maybe [Types.PatchOrchestratorFilter],
    -- | The maximum number of patch groups to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchGroups' value with any optional fields omitted.
mkDescribePatchGroups ::
  DescribePatchGroups
mkDescribePatchGroups =
  DescribePatchGroups'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- For @DescribePatchGroups@ ,valid filter keys include the following:
--
--     * @NAME_PREFIX@ : The name of the patch group. Wildcards (*) are accepted.
--
--
--     * @OPERATING_SYSTEM@ : The supported operating system type to return results for. For valid operating system values, see 'GetDefaultPatchBaselineRequest$OperatingSystem' in 'CreatePatchBaseline' .
-- Examples:
--
--     * @--filters Key=NAME_PREFIX,Values=MyPatchGroup*@
--
--
--     * @--filters Key=OPERATING_SYSTEM,Values=AMAZON_LINUX_2@
--
--
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgFilters :: Lens.Lens' DescribePatchGroups (Core.Maybe [Types.PatchOrchestratorFilter])
dpgFilters = Lens.field @"filters"
{-# DEPRECATED dpgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of patch groups to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgMaxResults :: Lens.Lens' DescribePatchGroups (Core.Maybe Core.Natural)
dpgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dpgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgNextToken :: Lens.Lens' DescribePatchGroups (Core.Maybe Types.NextToken)
dpgNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribePatchGroups where
  toJSON DescribePatchGroups {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribePatchGroups where
  type Rs DescribePatchGroups = DescribePatchGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribePatchGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePatchGroupsResponse'
            Core.<$> (x Core..:? "Mappings")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribePatchGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"mappings" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribePatchGroupsResponse' smart constructor.
data DescribePatchGroupsResponse = DescribePatchGroupsResponse'
  { -- | Each entry in the array contains:
    --
    -- PatchGroup: string (between 1 and 256 characters, Regex: ^([\p{L}\p{Z}\p{N}_.:/=+\-@]*)$)
    -- PatchBaselineIdentity: A PatchBaselineIdentity element.
    mappings :: Core.Maybe [Types.PatchGroupPatchBaselineMapping],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePatchGroupsResponse' value with any optional fields omitted.
mkDescribePatchGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePatchGroupsResponse
mkDescribePatchGroupsResponse responseStatus =
  DescribePatchGroupsResponse'
    { mappings = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Each entry in the array contains:
--
-- PatchGroup: string (between 1 and 256 characters, Regex: ^([\p{L}\p{Z}\p{N}_.:/=+\-@]*)$)
-- PatchBaselineIdentity: A PatchBaselineIdentity element.
--
-- /Note:/ Consider using 'mappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrrsMappings :: Lens.Lens' DescribePatchGroupsResponse (Core.Maybe [Types.PatchGroupPatchBaselineMapping])
dpgrrsMappings = Lens.field @"mappings"
{-# DEPRECATED dpgrrsMappings "Use generic-lens or generic-optics with 'mappings' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrrsNextToken :: Lens.Lens' DescribePatchGroupsResponse (Core.Maybe Types.NextToken)
dpgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrrsResponseStatus :: Lens.Lens' DescribePatchGroupsResponse Core.Int
dpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
