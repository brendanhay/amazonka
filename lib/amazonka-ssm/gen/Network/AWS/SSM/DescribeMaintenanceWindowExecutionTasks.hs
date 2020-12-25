{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given maintenance window execution, lists the tasks that were run.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
  ( -- * Creating a request
    DescribeMaintenanceWindowExecutionTasks (..),
    mkDescribeMaintenanceWindowExecutionTasks,

    -- ** Request lenses
    dmwetWindowExecutionId,
    dmwetFilters,
    dmwetMaxResults,
    dmwetNextToken,

    -- * Destructuring the response
    DescribeMaintenanceWindowExecutionTasksResponse (..),
    mkDescribeMaintenanceWindowExecutionTasksResponse,

    -- ** Response lenses
    dmwetrrsNextToken,
    dmwetrrsWindowExecutionTaskIdentities,
    dmwetrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeMaintenanceWindowExecutionTasks' smart constructor.
data DescribeMaintenanceWindowExecutionTasks = DescribeMaintenanceWindowExecutionTasks'
  { -- | The ID of the maintenance window execution whose task executions should be retrieved.
    windowExecutionId :: Types.MaintenanceWindowExecutionId,
    -- | Optional filters used to scope down the returned tasks. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
    filters :: Core.Maybe [Types.MaintenanceWindowFilter],
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowExecutionTasks' value with any optional fields omitted.
mkDescribeMaintenanceWindowExecutionTasks ::
  -- | 'windowExecutionId'
  Types.MaintenanceWindowExecutionId ->
  DescribeMaintenanceWindowExecutionTasks
mkDescribeMaintenanceWindowExecutionTasks windowExecutionId =
  DescribeMaintenanceWindowExecutionTasks'
    { windowExecutionId,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the maintenance window execution whose task executions should be retrieved.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetWindowExecutionId :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks Types.MaintenanceWindowExecutionId
dmwetWindowExecutionId = Lens.field @"windowExecutionId"
{-# DEPRECATED dmwetWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | Optional filters used to scope down the returned tasks. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetFilters :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Core.Maybe [Types.MaintenanceWindowFilter])
dmwetFilters = Lens.field @"filters"
{-# DEPRECATED dmwetFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetMaxResults :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Core.Maybe Core.Natural)
dmwetMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dmwetMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Core.Maybe Types.NextToken)
dmwetNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwetNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeMaintenanceWindowExecutionTasks where
  toJSON DescribeMaintenanceWindowExecutionTasks {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowExecutionId" Core..= windowExecutionId),
            ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeMaintenanceWindowExecutionTasks where
  type
    Rs DescribeMaintenanceWindowExecutionTasks =
      DescribeMaintenanceWindowExecutionTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonSSM.DescribeMaintenanceWindowExecutionTasks"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionTasksResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "WindowExecutionTaskIdentities")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMaintenanceWindowExecutionTasks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"windowExecutionTaskIdentities" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeMaintenanceWindowExecutionTasksResponse' smart constructor.
data DescribeMaintenanceWindowExecutionTasksResponse = DescribeMaintenanceWindowExecutionTasksResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the task executions.
    windowExecutionTaskIdentities :: Core.Maybe [Types.MaintenanceWindowExecutionTaskIdentity],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowExecutionTasksResponse' value with any optional fields omitted.
mkDescribeMaintenanceWindowExecutionTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMaintenanceWindowExecutionTasksResponse
mkDescribeMaintenanceWindowExecutionTasksResponse responseStatus =
  DescribeMaintenanceWindowExecutionTasksResponse'
    { nextToken =
        Core.Nothing,
      windowExecutionTaskIdentities = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetrrsNextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse (Core.Maybe Types.NextToken)
dmwetrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwetrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the task executions.
--
-- /Note:/ Consider using 'windowExecutionTaskIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetrrsWindowExecutionTaskIdentities :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse (Core.Maybe [Types.MaintenanceWindowExecutionTaskIdentity])
dmwetrrsWindowExecutionTaskIdentities = Lens.field @"windowExecutionTaskIdentities"
{-# DEPRECATED dmwetrrsWindowExecutionTaskIdentities "Use generic-lens or generic-optics with 'windowExecutionTaskIdentities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwetrrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse Core.Int
dmwetrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmwetrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
