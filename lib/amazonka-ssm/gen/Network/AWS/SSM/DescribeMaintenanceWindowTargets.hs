{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets registered with the maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowTargets
  ( -- * Creating a request
    DescribeMaintenanceWindowTargets (..),
    mkDescribeMaintenanceWindowTargets,

    -- ** Request lenses
    dmwtWindowId,
    dmwtFilters,
    dmwtMaxResults,
    dmwtNextToken,

    -- * Destructuring the response
    DescribeMaintenanceWindowTargetsResponse (..),
    mkDescribeMaintenanceWindowTargetsResponse,

    -- ** Response lenses
    dmwtrrsNextToken,
    dmwtrrsTargets,
    dmwtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeMaintenanceWindowTargets' smart constructor.
data DescribeMaintenanceWindowTargets = DescribeMaintenanceWindowTargets'
  { -- | The ID of the maintenance window whose targets should be retrieved.
    windowId :: Types.WindowId,
    -- | Optional filters that can be used to narrow down the scope of the returned window targets. The supported filter keys are Type, WindowTargetId and OwnerInformation.
    filters :: Core.Maybe [Types.MaintenanceWindowFilter],
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowTargets' value with any optional fields omitted.
mkDescribeMaintenanceWindowTargets ::
  -- | 'windowId'
  Types.WindowId ->
  DescribeMaintenanceWindowTargets
mkDescribeMaintenanceWindowTargets windowId =
  DescribeMaintenanceWindowTargets'
    { windowId,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the maintenance window whose targets should be retrieved.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtWindowId :: Lens.Lens' DescribeMaintenanceWindowTargets Types.WindowId
dmwtWindowId = Lens.field @"windowId"
{-# DEPRECATED dmwtWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | Optional filters that can be used to narrow down the scope of the returned window targets. The supported filter keys are Type, WindowTargetId and OwnerInformation.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtFilters :: Lens.Lens' DescribeMaintenanceWindowTargets (Core.Maybe [Types.MaintenanceWindowFilter])
dmwtFilters = Lens.field @"filters"
{-# DEPRECATED dmwtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtMaxResults :: Lens.Lens' DescribeMaintenanceWindowTargets (Core.Maybe Core.Natural)
dmwtMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dmwtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtNextToken :: Lens.Lens' DescribeMaintenanceWindowTargets (Core.Maybe Types.NextToken)
dmwtNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeMaintenanceWindowTargets where
  toJSON DescribeMaintenanceWindowTargets {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeMaintenanceWindowTargets where
  type
    Rs DescribeMaintenanceWindowTargets =
      DescribeMaintenanceWindowTargetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DescribeMaintenanceWindowTargets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowTargetsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Targets")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMaintenanceWindowTargets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"targets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeMaintenanceWindowTargetsResponse' smart constructor.
data DescribeMaintenanceWindowTargetsResponse = DescribeMaintenanceWindowTargetsResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the targets in the maintenance window.
    targets :: Core.Maybe [Types.MaintenanceWindowTarget],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowTargetsResponse' value with any optional fields omitted.
mkDescribeMaintenanceWindowTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMaintenanceWindowTargetsResponse
mkDescribeMaintenanceWindowTargetsResponse responseStatus =
  DescribeMaintenanceWindowTargetsResponse'
    { nextToken =
        Core.Nothing,
      targets = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrrsNextToken :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse (Core.Maybe Types.NextToken)
dmwtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the targets in the maintenance window.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrrsTargets :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse (Core.Maybe [Types.MaintenanceWindowTarget])
dmwtrrsTargets = Lens.field @"targets"
{-# DEPRECATED dmwtrrsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwtrrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowTargetsResponse Core.Int
dmwtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmwtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
