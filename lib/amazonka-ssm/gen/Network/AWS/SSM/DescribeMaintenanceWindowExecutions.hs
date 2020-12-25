{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a maintenance window. This includes information about when the maintenance window was scheduled to be active, and information about tasks registered and run with the maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowExecutions
  ( -- * Creating a request
    DescribeMaintenanceWindowExecutions (..),
    mkDescribeMaintenanceWindowExecutions,

    -- ** Request lenses
    dmweWindowId,
    dmweFilters,
    dmweMaxResults,
    dmweNextToken,

    -- * Destructuring the response
    DescribeMaintenanceWindowExecutionsResponse (..),
    mkDescribeMaintenanceWindowExecutionsResponse,

    -- ** Response lenses
    dmwerrsNextToken,
    dmwerrsWindowExecutions,
    dmwerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeMaintenanceWindowExecutions' smart constructor.
data DescribeMaintenanceWindowExecutions = DescribeMaintenanceWindowExecutions'
  { -- | The ID of the maintenance window whose executions should be retrieved.
    windowId :: Types.WindowId,
    -- | Each entry in the array is a structure containing:
    --
    -- Key (string, between 1 and 128 characters)
    -- Values (array of strings, each string is between 1 and 256 characters)
    -- The supported Keys are ExecutedBefore and ExecutedAfter with the value being a date/time string such as 2016-11-04T05:00:00Z.
    filters :: Core.Maybe [Types.MaintenanceWindowFilter],
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowExecutions' value with any optional fields omitted.
mkDescribeMaintenanceWindowExecutions ::
  -- | 'windowId'
  Types.WindowId ->
  DescribeMaintenanceWindowExecutions
mkDescribeMaintenanceWindowExecutions windowId =
  DescribeMaintenanceWindowExecutions'
    { windowId,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the maintenance window whose executions should be retrieved.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmweWindowId :: Lens.Lens' DescribeMaintenanceWindowExecutions Types.WindowId
dmweWindowId = Lens.field @"windowId"
{-# DEPRECATED dmweWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | Each entry in the array is a structure containing:
--
-- Key (string, between 1 and 128 characters)
-- Values (array of strings, each string is between 1 and 256 characters)
-- The supported Keys are ExecutedBefore and ExecutedAfter with the value being a date/time string such as 2016-11-04T05:00:00Z.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmweFilters :: Lens.Lens' DescribeMaintenanceWindowExecutions (Core.Maybe [Types.MaintenanceWindowFilter])
dmweFilters = Lens.field @"filters"
{-# DEPRECATED dmweFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmweMaxResults :: Lens.Lens' DescribeMaintenanceWindowExecutions (Core.Maybe Core.Natural)
dmweMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dmweMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmweNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutions (Core.Maybe Types.NextToken)
dmweNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmweNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeMaintenanceWindowExecutions where
  toJSON DescribeMaintenanceWindowExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeMaintenanceWindowExecutions where
  type
    Rs DescribeMaintenanceWindowExecutions =
      DescribeMaintenanceWindowExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DescribeMaintenanceWindowExecutions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "WindowExecutions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMaintenanceWindowExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"windowExecutions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeMaintenanceWindowExecutionsResponse' smart constructor.
data DescribeMaintenanceWindowExecutionsResponse = DescribeMaintenanceWindowExecutionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the maintenance window executions.
    windowExecutions :: Core.Maybe [Types.MaintenanceWindowExecution],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowExecutionsResponse' value with any optional fields omitted.
mkDescribeMaintenanceWindowExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMaintenanceWindowExecutionsResponse
mkDescribeMaintenanceWindowExecutionsResponse responseStatus =
  DescribeMaintenanceWindowExecutionsResponse'
    { nextToken =
        Core.Nothing,
      windowExecutions = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwerrsNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse (Core.Maybe Types.NextToken)
dmwerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the maintenance window executions.
--
-- /Note:/ Consider using 'windowExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwerrsWindowExecutions :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse (Core.Maybe [Types.MaintenanceWindowExecution])
dmwerrsWindowExecutions = Lens.field @"windowExecutions"
{-# DEPRECATED dmwerrsWindowExecutions "Use generic-lens or generic-optics with 'windowExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwerrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionsResponse Core.Int
dmwerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmwerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
