{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListClusterJobs (..)
    , mkListClusterJobs
    -- ** Request lenses
    , lcjClusterId
    , lcjMaxResults
    , lcjNextToken

    -- * Destructuring the response
    , ListClusterJobsResponse (..)
    , mkListClusterJobsResponse
    -- ** Response lenses
    , lcjrrsJobListEntries
    , lcjrrsNextToken
    , lcjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkListClusterJobs' smart constructor.
data ListClusterJobs = ListClusterJobs'
  { clusterId :: Types.ClusterId
    -- ^ The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The number of @JobListEntry@ objects to return.
  , nextToken :: Core.Maybe Core.Text
    -- ^ HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListClusterJobs' value with any optional fields omitted.
mkListClusterJobs
    :: Types.ClusterId -- ^ 'clusterId'
    -> ListClusterJobs
mkListClusterJobs clusterId
  = ListClusterJobs'{clusterId, maxResults = Core.Nothing,
                     nextToken = Core.Nothing}

-- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjClusterId :: Lens.Lens' ListClusterJobs Types.ClusterId
lcjClusterId = Lens.field @"clusterId"
{-# INLINEABLE lcjClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The number of @JobListEntry@ objects to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjMaxResults :: Lens.Lens' ListClusterJobs (Core.Maybe Core.Natural)
lcjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | HTTP requests are stateless. To identify what object comes "next" in the list of @JobListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjNextToken :: Lens.Lens' ListClusterJobs (Core.Maybe Core.Text)
lcjNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListClusterJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListClusterJobs where
        toHeaders ListClusterJobs{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSIESnowballJobManagementService.ListClusterJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListClusterJobs where
        toJSON ListClusterJobs{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClusterId" Core..= clusterId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListClusterJobs where
        type Rs ListClusterJobs = ListClusterJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListClusterJobsResponse' Core.<$>
                   (x Core..:? "JobListEntries") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListClusterJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"jobListEntries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListClusterJobsResponse' smart constructor.
data ListClusterJobsResponse = ListClusterJobsResponse'
  { jobListEntries :: Core.Maybe [Types.JobListEntry]
    -- ^ Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs. 
  , nextToken :: Core.Maybe Core.Text
    -- ^ HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ListClusterJobsResult@ call, your list of returned jobs will start from this point in the array.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListClusterJobsResponse' value with any optional fields omitted.
mkListClusterJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListClusterJobsResponse
mkListClusterJobsResponse responseStatus
  = ListClusterJobsResponse'{jobListEntries = Core.Nothing,
                             nextToken = Core.Nothing, responseStatus}

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of export jobs. 
--
-- /Note:/ Consider using 'jobListEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsJobListEntries :: Lens.Lens' ListClusterJobsResponse (Core.Maybe [Types.JobListEntry])
lcjrrsJobListEntries = Lens.field @"jobListEntries"
{-# INLINEABLE lcjrrsJobListEntries #-}
{-# DEPRECATED jobListEntries "Use generic-lens or generic-optics with 'jobListEntries' instead"  #-}

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ListClusterJobsResult@ call, your list of returned jobs will start from this point in the array.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsNextToken :: Lens.Lens' ListClusterJobsResponse (Core.Maybe Core.Text)
lcjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsResponseStatus :: Lens.Lens' ListClusterJobsResponse Core.Int
lcjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
