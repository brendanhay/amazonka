{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists jobs.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljMaxResults,
    ljNamespaceId,
    ljNextToken,
    ljStatus,
    ljTargetSelection,
    ljThingGroupId,
    ljThingGroupName,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrrsJobs,
    ljrrsNextToken,
    ljrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | The maximum number of results to return per request.
    maxResults :: Core.Maybe Core.Natural,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
    -- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
    namespaceId :: Core.Maybe Types.NamespaceId,
    -- | The token to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | An optional filter that lets you search for jobs that have the specified status.
    status :: Core.Maybe Types.JobStatus,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
    targetSelection :: Core.Maybe Types.TargetSelection,
    -- | A filter that limits the returned jobs to those for the specified group.
    thingGroupId :: Core.Maybe Types.ThingGroupId,
    -- | A filter that limits the returned jobs to those for the specified group.
    thingGroupName :: Core.Maybe Types.ThingGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobs' value with any optional fields omitted.
mkListJobs ::
  ListJobs
mkListJobs =
  ListJobs'
    { maxResults = Core.Nothing,
      namespaceId = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing,
      targetSelection = Core.Nothing,
      thingGroupId = Core.Nothing,
      thingGroupName = Core.Nothing
    }

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Core.Maybe Core.Natural)
ljMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNamespaceId :: Lens.Lens' ListJobs (Core.Maybe Types.NamespaceId)
ljNamespaceId = Lens.field @"namespaceId"
{-# DEPRECATED ljNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Core.Maybe Types.NextToken)
ljNextToken = Lens.field @"nextToken"
{-# DEPRECATED ljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An optional filter that lets you search for jobs that have the specified status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljStatus :: Lens.Lens' ListJobs (Core.Maybe Types.JobStatus)
ljStatus = Lens.field @"status"
{-# DEPRECATED ljStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljTargetSelection :: Lens.Lens' ListJobs (Core.Maybe Types.TargetSelection)
ljTargetSelection = Lens.field @"targetSelection"
{-# DEPRECATED ljTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

-- | A filter that limits the returned jobs to those for the specified group.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljThingGroupId :: Lens.Lens' ListJobs (Core.Maybe Types.ThingGroupId)
ljThingGroupId = Lens.field @"thingGroupId"
{-# DEPRECATED ljThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | A filter that limits the returned jobs to those for the specified group.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljThingGroupName :: Lens.Lens' ListJobs (Core.Maybe Types.ThingGroupName)
ljThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED ljThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

instance Core.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/jobs",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "namespaceId" Core.<$> namespaceId)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "status" Core.<$> status)
            Core.<> (Core.toQueryValue "targetSelection" Core.<$> targetSelection)
            Core.<> (Core.toQueryValue "thingGroupId" Core.<$> thingGroupId)
            Core.<> (Core.toQueryValue "thingGroupName" Core.<$> thingGroupName),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Core.<$> (x Core..:? "jobs")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"jobs" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | A list of jobs.
    jobs :: Core.Maybe [Types.JobSummary],
    -- | The token for the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListJobsResponse' value with any optional fields omitted.
mkListJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListJobsResponse
mkListJobsResponse responseStatus =
  ListJobsResponse'
    { jobs = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of jobs.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsJobs :: Lens.Lens' ListJobsResponse (Core.Maybe [Types.JobSummary])
ljrrsJobs = Lens.field @"jobs"
{-# DEPRECATED ljrrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The token for the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsNextToken :: Lens.Lens' ListJobsResponse (Core.Maybe Types.NextToken)
ljrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ljrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsResponseStatus :: Lens.Lens' ListJobsResponse Core.Int
ljrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ljrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
