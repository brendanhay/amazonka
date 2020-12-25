{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about upcoming executions of a maintenance window.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowSchedule
  ( -- * Creating a request
    DescribeMaintenanceWindowSchedule (..),
    mkDescribeMaintenanceWindowSchedule,

    -- ** Request lenses
    dmwsFilters,
    dmwsMaxResults,
    dmwsNextToken,
    dmwsResourceType,
    dmwsTargets,
    dmwsWindowId,

    -- * Destructuring the response
    DescribeMaintenanceWindowScheduleResponse (..),
    mkDescribeMaintenanceWindowScheduleResponse,

    -- ** Response lenses
    dmwsrrsNextToken,
    dmwsrrsScheduledWindowExecutions,
    dmwsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeMaintenanceWindowSchedule' smart constructor.
data DescribeMaintenanceWindowSchedule = DescribeMaintenanceWindowSchedule'
  { -- | Filters used to limit the range of results. For example, you can limit maintenance window executions to only those scheduled before or after a certain date and time.
    filters :: Core.Maybe [Types.PatchOrchestratorFilter],
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | The type of resource you want to retrieve information about. For example, "INSTANCE".
    resourceType :: Core.Maybe Types.MaintenanceWindowResourceType,
    -- | The instance ID or key/value pair to retrieve information about.
    targets :: Core.Maybe [Types.Target],
    -- | The ID of the maintenance window to retrieve information about.
    windowId :: Core.Maybe Types.WindowId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowSchedule' value with any optional fields omitted.
mkDescribeMaintenanceWindowSchedule ::
  DescribeMaintenanceWindowSchedule
mkDescribeMaintenanceWindowSchedule =
  DescribeMaintenanceWindowSchedule'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      resourceType = Core.Nothing,
      targets = Core.Nothing,
      windowId = Core.Nothing
    }

-- | Filters used to limit the range of results. For example, you can limit maintenance window executions to only those scheduled before or after a certain date and time.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsFilters :: Lens.Lens' DescribeMaintenanceWindowSchedule (Core.Maybe [Types.PatchOrchestratorFilter])
dmwsFilters = Lens.field @"filters"
{-# DEPRECATED dmwsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsMaxResults :: Lens.Lens' DescribeMaintenanceWindowSchedule (Core.Maybe Core.Natural)
dmwsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dmwsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsNextToken :: Lens.Lens' DescribeMaintenanceWindowSchedule (Core.Maybe Types.NextToken)
dmwsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of resource you want to retrieve information about. For example, "INSTANCE".
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsResourceType :: Lens.Lens' DescribeMaintenanceWindowSchedule (Core.Maybe Types.MaintenanceWindowResourceType)
dmwsResourceType = Lens.field @"resourceType"
{-# DEPRECATED dmwsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The instance ID or key/value pair to retrieve information about.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsTargets :: Lens.Lens' DescribeMaintenanceWindowSchedule (Core.Maybe [Types.Target])
dmwsTargets = Lens.field @"targets"
{-# DEPRECATED dmwsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The ID of the maintenance window to retrieve information about.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsWindowId :: Lens.Lens' DescribeMaintenanceWindowSchedule (Core.Maybe Types.WindowId)
dmwsWindowId = Lens.field @"windowId"
{-# DEPRECATED dmwsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Core.FromJSON DescribeMaintenanceWindowSchedule where
  toJSON DescribeMaintenanceWindowSchedule {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ResourceType" Core..=) Core.<$> resourceType,
            ("Targets" Core..=) Core.<$> targets,
            ("WindowId" Core..=) Core.<$> windowId
          ]
      )

instance Core.AWSRequest DescribeMaintenanceWindowSchedule where
  type
    Rs DescribeMaintenanceWindowSchedule =
      DescribeMaintenanceWindowScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DescribeMaintenanceWindowSchedule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowScheduleResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ScheduledWindowExecutions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMaintenanceWindowSchedule where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"scheduledWindowExecutions" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeMaintenanceWindowScheduleResponse' smart constructor.
data DescribeMaintenanceWindowScheduleResponse = DescribeMaintenanceWindowScheduleResponse'
  { -- | The token for the next set of items to return. (You use this token in the next call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about maintenance window executions scheduled for the specified time range.
    scheduledWindowExecutions :: Core.Maybe [Types.ScheduledWindowExecution],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowScheduleResponse' value with any optional fields omitted.
mkDescribeMaintenanceWindowScheduleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMaintenanceWindowScheduleResponse
mkDescribeMaintenanceWindowScheduleResponse responseStatus =
  DescribeMaintenanceWindowScheduleResponse'
    { nextToken =
        Core.Nothing,
      scheduledWindowExecutions = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of items to return. (You use this token in the next call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsrrsNextToken :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse (Core.Maybe Types.NextToken)
dmwsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about maintenance window executions scheduled for the specified time range.
--
-- /Note:/ Consider using 'scheduledWindowExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsrrsScheduledWindowExecutions :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse (Core.Maybe [Types.ScheduledWindowExecution])
dmwsrrsScheduledWindowExecutions = Lens.field @"scheduledWindowExecutions"
{-# DEPRECATED dmwsrrsScheduledWindowExecutions "Use generic-lens or generic-optics with 'scheduledWindowExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwsrrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowScheduleResponse Core.Int
dmwsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmwsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
