{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListMonitoringSchedules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of all monitoring schedules.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListMonitoringSchedules
  ( -- * Creating a request
    ListMonitoringSchedules (..),
    mkListMonitoringSchedules,

    -- ** Request lenses
    lmsCreationTimeAfter,
    lmsCreationTimeBefore,
    lmsEndpointName,
    lmsLastModifiedTimeAfter,
    lmsLastModifiedTimeBefore,
    lmsMaxResults,
    lmsNameContains,
    lmsNextToken,
    lmsSortBy,
    lmsSortOrder,
    lmsStatusEquals,

    -- * Destructuring the response
    ListMonitoringSchedulesResponse (..),
    mkListMonitoringSchedulesResponse,

    -- ** Response lenses
    lmsrrsMonitoringScheduleSummaries,
    lmsrrsNextToken,
    lmsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListMonitoringSchedules' smart constructor.
data ListMonitoringSchedules = ListMonitoringSchedules'
  { -- | A filter that returns only monitoring schedules created after a specified time.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only monitoring schedules created before a specified time.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | Name of a specific endpoint to fetch schedules for.
    endpointName :: Core.Maybe Types.EndpointName,
    -- | A filter that returns only monitoring schedules modified after a specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only monitoring schedules modified before a specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of jobs to return in the response. The default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filter for monitoring schedules whose name contains a specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.MonitoringScheduleSortKey,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A filter that returns only monitoring schedules modified before a specified time.
    statusEquals :: Core.Maybe Types.ScheduleStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListMonitoringSchedules' value with any optional fields omitted.
mkListMonitoringSchedules ::
  ListMonitoringSchedules
mkListMonitoringSchedules =
  ListMonitoringSchedules'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      endpointName = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      lastModifiedTimeBefore = Core.Nothing,
      maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing,
      statusEquals = Core.Nothing
    }

-- | A filter that returns only monitoring schedules created after a specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsCreationTimeAfter :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.NominalDiffTime)
lmsCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lmsCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only monitoring schedules created before a specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsCreationTimeBefore :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.NominalDiffTime)
lmsCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lmsCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | Name of a specific endpoint to fetch schedules for.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsEndpointName :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Types.EndpointName)
lmsEndpointName = Lens.field @"endpointName"
{-# DEPRECATED lmsEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | A filter that returns only monitoring schedules modified after a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsLastModifiedTimeAfter :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.NominalDiffTime)
lmsLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED lmsLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only monitoring schedules modified before a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsLastModifiedTimeBefore :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.NominalDiffTime)
lmsLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED lmsLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | The maximum number of jobs to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsMaxResults :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.Natural)
lmsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lmsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Filter for monitoring schedules whose name contains a specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsNameContains :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Types.NameContains)
lmsNameContains = Lens.field @"nameContains"
{-# DEPRECATED lmsNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsNextToken :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Types.NextToken)
lmsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsSortBy :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Types.MonitoringScheduleSortKey)
lmsSortBy = Lens.field @"sortBy"
{-# DEPRECATED lmsSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsSortOrder :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Types.SortOrder)
lmsSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lmsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only monitoring schedules modified before a specified time.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsStatusEquals :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Types.ScheduleStatus)
lmsStatusEquals = Lens.field @"statusEquals"
{-# DEPRECATED lmsStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

instance Core.FromJSON ListMonitoringSchedules where
  toJSON ListMonitoringSchedules {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("EndpointName" Core..=) Core.<$> endpointName,
            ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("StatusEquals" Core..=) Core.<$> statusEquals
          ]
      )

instance Core.AWSRequest ListMonitoringSchedules where
  type Rs ListMonitoringSchedules = ListMonitoringSchedulesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListMonitoringSchedules")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitoringSchedulesResponse'
            Core.<$> (x Core..:? "MonitoringScheduleSummaries" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListMonitoringSchedules where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"monitoringScheduleSummaries") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListMonitoringSchedulesResponse' smart constructor.
data ListMonitoringSchedulesResponse = ListMonitoringSchedulesResponse'
  { -- | A JSON array in which each element is a summary for a monitoring schedule.
    monitoringScheduleSummaries :: [Types.MonitoringScheduleSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListMonitoringSchedulesResponse' value with any optional fields omitted.
mkListMonitoringSchedulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListMonitoringSchedulesResponse
mkListMonitoringSchedulesResponse responseStatus =
  ListMonitoringSchedulesResponse'
    { monitoringScheduleSummaries =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A JSON array in which each element is a summary for a monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsrrsMonitoringScheduleSummaries :: Lens.Lens' ListMonitoringSchedulesResponse [Types.MonitoringScheduleSummary]
lmsrrsMonitoringScheduleSummaries = Lens.field @"monitoringScheduleSummaries"
{-# DEPRECATED lmsrrsMonitoringScheduleSummaries "Use generic-lens or generic-optics with 'monitoringScheduleSummaries' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsrrsNextToken :: Lens.Lens' ListMonitoringSchedulesResponse (Core.Maybe Types.NextToken)
lmsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmsrrsResponseStatus :: Lens.Lens' ListMonitoringSchedulesResponse Core.Int
lmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
