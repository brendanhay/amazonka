{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListJobs (..)
    , mkListJobs
    -- ** Request lenses
    , ljMaxResults
    , ljNamespaceId
    , ljNextToken
    , ljStatus
    , ljTargetSelection
    , ljThingGroupId
    , ljThingGroupName

    -- * Destructuring the response
    , ListJobsResponse (..)
    , mkListJobsResponse
    -- ** Response lenses
    , ljrrsJobs
    , ljrrsNextToken
    , ljrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return per request.
  , namespaceId :: Core.Maybe Types.NamespaceId
    -- ^ The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@ 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next set of results.
  , status :: Core.Maybe Types.JobStatus
    -- ^ An optional filter that lets you search for jobs that have the specified status.
  , targetSelection :: Core.Maybe Types.TargetSelection
    -- ^ Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group. 
  , thingGroupId :: Core.Maybe Types.ThingGroupId
    -- ^ A filter that limits the returned jobs to those for the specified group.
  , thingGroupName :: Core.Maybe Types.ThingGroupName
    -- ^ A filter that limits the returned jobs to those for the specified group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobs' value with any optional fields omitted.
mkListJobs
    :: ListJobs
mkListJobs
  = ListJobs'{maxResults = Core.Nothing, namespaceId = Core.Nothing,
              nextToken = Core.Nothing, status = Core.Nothing,
              targetSelection = Core.Nothing, thingGroupId = Core.Nothing,
              thingGroupName = Core.Nothing}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Core.Maybe Core.Natural)
ljMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ljMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs notifications to MQTT topics that contain the value in the following format.
-- @> aws/things//THING_NAME/ /jobs//JOB_ID/ /notify-namespace-/NAMESPACE_ID/ /@ 
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNamespaceId :: Lens.Lens' ListJobs (Core.Maybe Types.NamespaceId)
ljNamespaceId = Lens.field @"namespaceId"
{-# INLINEABLE ljNamespaceId #-}
{-# DEPRECATED namespaceId "Use generic-lens or generic-optics with 'namespaceId' instead"  #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Core.Maybe Types.NextToken)
ljNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An optional filter that lets you search for jobs that have the specified status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljStatus :: Lens.Lens' ListJobs (Core.Maybe Types.JobStatus)
ljStatus = Lens.field @"status"
{-# INLINEABLE ljStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group. 
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljTargetSelection :: Lens.Lens' ListJobs (Core.Maybe Types.TargetSelection)
ljTargetSelection = Lens.field @"targetSelection"
{-# INLINEABLE ljTargetSelection #-}
{-# DEPRECATED targetSelection "Use generic-lens or generic-optics with 'targetSelection' instead"  #-}

-- | A filter that limits the returned jobs to those for the specified group.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljThingGroupId :: Lens.Lens' ListJobs (Core.Maybe Types.ThingGroupId)
ljThingGroupId = Lens.field @"thingGroupId"
{-# INLINEABLE ljThingGroupId #-}
{-# DEPRECATED thingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead"  #-}

-- | A filter that limits the returned jobs to those for the specified group.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljThingGroupName :: Lens.Lens' ListJobs (Core.Maybe Types.ThingGroupName)
ljThingGroupName = Lens.field @"thingGroupName"
{-# INLINEABLE ljThingGroupName #-}
{-# DEPRECATED thingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead"  #-}

instance Core.ToQuery ListJobs where
        toQuery ListJobs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "namespaceId") namespaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "status") status
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "targetSelection")
                targetSelection
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "thingGroupId")
                thingGroupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "thingGroupName")
                thingGroupName

instance Core.ToHeaders ListJobs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/jobs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListJobsResponse' Core.<$>
                   (x Core..:? "jobs") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"jobs" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { jobs :: Core.Maybe [Types.JobSummary]
    -- ^ A list of jobs.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results, or __null__ if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListJobsResponse' value with any optional fields omitted.
mkListJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListJobsResponse
mkListJobsResponse responseStatus
  = ListJobsResponse'{jobs = Core.Nothing, nextToken = Core.Nothing,
                      responseStatus}

-- | A list of jobs.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsJobs :: Lens.Lens' ListJobsResponse (Core.Maybe [Types.JobSummary])
ljrrsJobs = Lens.field @"jobs"
{-# INLINEABLE ljrrsJobs #-}
{-# DEPRECATED jobs "Use generic-lens or generic-optics with 'jobs' instead"  #-}

-- | The token for the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsNextToken :: Lens.Lens' ListJobsResponse (Core.Maybe Types.NextToken)
ljrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsResponseStatus :: Lens.Lens' ListJobsResponse Core.Int
ljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
