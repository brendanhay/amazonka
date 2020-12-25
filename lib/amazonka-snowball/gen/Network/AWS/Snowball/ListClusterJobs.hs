{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.ListClusterJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @JobListEntry@ objects of the specified length. Each @JobListEntry@ object is for a job in the specified cluster and contains a job's state, a job's ID, and other information.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListClusterJobs
  ( -- * Creating a request
    ListClusterJobs (..),
    mkListClusterJobs,

    -- ** Request lenses
    lcjClusterId,
    lcjMaxResults,
    lcjNextToken,

    -- * Destructuring the response
    ListClusterJobsResponse (..),
    mkListClusterJobsResponse,

    -- ** Response lenses
    lcjrrsJobListEntries,
    lcjrrsNextToken,
    lcjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkListClusterJobs' smart constructor.
data ListClusterJobs = ListClusterJobs'
  { -- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
    clusterId :: Types.ClusterId,
    -- | The number of @JobListEntry@ objects to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListClusterJobs' value with any optional fields omitted.
mkListClusterJobs ::
  -- | 'clusterId'
  Types.ClusterId ->
  ListClusterJobs
mkListClusterJobs clusterId =
  ListClusterJobs'
    { clusterId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjClusterId :: Lens.Lens' ListClusterJobs Types.ClusterId
lcjClusterId = Lens.field @"clusterId"
{-# DEPRECATED lcjClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The number of @JobListEntry@ objects to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjMaxResults :: Lens.Lens' ListClusterJobs (Core.Maybe Core.Natural)
lcjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjNextToken :: Lens.Lens' ListClusterJobs (Core.Maybe Types.NextToken)
lcjNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListClusterJobs where
  toJSON ListClusterJobs {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterId" Core..= clusterId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListClusterJobs where
  type Rs ListClusterJobs = ListClusterJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSIESnowballJobManagementService.ListClusterJobs"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClusterJobsResponse'
            Core.<$> (x Core..:? "JobListEntries")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListClusterJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"jobListEntries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListClusterJobsResponse' smart constructor.
data ListClusterJobsResponse = ListClusterJobsResponse'
  { -- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs.
    jobListEntries :: Core.Maybe [Types.JobListEntry],
    -- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ListClusterJobsResult@ call, your list of returned jobs will start from this point in the array.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListClusterJobsResponse' value with any optional fields omitted.
mkListClusterJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListClusterJobsResponse
mkListClusterJobsResponse responseStatus =
  ListClusterJobsResponse'
    { jobListEntries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs.
--
-- /Note:/ Consider using 'jobListEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsJobListEntries :: Lens.Lens' ListClusterJobsResponse (Core.Maybe [Types.JobListEntry])
lcjrrsJobListEntries = Lens.field @"jobListEntries"
{-# DEPRECATED lcjrrsJobListEntries "Use generic-lens or generic-optics with 'jobListEntries' instead." #-}

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ListClusterJobsResult@ call, your list of returned jobs will start from this point in the array.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsNextToken :: Lens.Lens' ListClusterJobsResponse (Core.Maybe Types.String)
lcjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsResponseStatus :: Lens.Lens' ListClusterJobsResponse Core.Int
lcjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
