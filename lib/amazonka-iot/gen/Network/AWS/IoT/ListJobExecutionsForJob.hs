{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListJobExecutionsForJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for a job.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobExecutionsForJob
    (
    -- * Creating a request
      ListJobExecutionsForJob (..)
    , mkListJobExecutionsForJob
    -- ** Request lenses
    , ljefjJobId
    , ljefjMaxResults
    , ljefjNextToken
    , ljefjStatus

    -- * Destructuring the response
    , ListJobExecutionsForJobResponse (..)
    , mkListJobExecutionsForJobResponse
    -- ** Response lenses
    , ljefjrrsExecutionSummaries
    , ljefjrrsNextToken
    , ljefjrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJobExecutionsForJob' smart constructor.
data ListJobExecutionsForJob = ListJobExecutionsForJob'
  { jobId :: Types.JobId
    -- ^ The unique identifier you assigned to this job when it was created.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next set of results.
  , status :: Core.Maybe Types.JobExecutionStatus
    -- ^ The status of the job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobExecutionsForJob' value with any optional fields omitted.
mkListJobExecutionsForJob
    :: Types.JobId -- ^ 'jobId'
    -> ListJobExecutionsForJob
mkListJobExecutionsForJob jobId
  = ListJobExecutionsForJob'{jobId, maxResults = Core.Nothing,
                             nextToken = Core.Nothing, status = Core.Nothing}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjJobId :: Lens.Lens' ListJobExecutionsForJob Types.JobId
ljefjJobId = Lens.field @"jobId"
{-# INLINEABLE ljefjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjMaxResults :: Lens.Lens' ListJobExecutionsForJob (Core.Maybe Core.Natural)
ljefjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ljefjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjNextToken :: Lens.Lens' ListJobExecutionsForJob (Core.Maybe Types.NextToken)
ljefjNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljefjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The status of the job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjStatus :: Lens.Lens' ListJobExecutionsForJob (Core.Maybe Types.JobExecutionStatus)
ljefjStatus = Lens.field @"status"
{-# INLINEABLE ljefjStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListJobExecutionsForJob where
        toQuery ListJobExecutionsForJob{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "status") status

instance Core.ToHeaders ListJobExecutionsForJob where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListJobExecutionsForJob where
        type Rs ListJobExecutionsForJob = ListJobExecutionsForJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/jobs/" Core.<> Core.toText jobId Core.<> "/things",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListJobExecutionsForJobResponse' Core.<$>
                   (x Core..:? "executionSummaries") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListJobExecutionsForJob where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"executionSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListJobExecutionsForJobResponse' smart constructor.
data ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse'
  { executionSummaries :: Core.Maybe [Types.JobExecutionSummaryForJob]
    -- ^ A list of job execution summaries.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results, or __null__ if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListJobExecutionsForJobResponse' value with any optional fields omitted.
mkListJobExecutionsForJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListJobExecutionsForJobResponse
mkListJobExecutionsForJobResponse responseStatus
  = ListJobExecutionsForJobResponse'{executionSummaries =
                                       Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | A list of job execution summaries.
--
-- /Note:/ Consider using 'executionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjrrsExecutionSummaries :: Lens.Lens' ListJobExecutionsForJobResponse (Core.Maybe [Types.JobExecutionSummaryForJob])
ljefjrrsExecutionSummaries = Lens.field @"executionSummaries"
{-# INLINEABLE ljefjrrsExecutionSummaries #-}
{-# DEPRECATED executionSummaries "Use generic-lens or generic-optics with 'executionSummaries' instead"  #-}

-- | The token for the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjrrsNextToken :: Lens.Lens' ListJobExecutionsForJobResponse (Core.Maybe Types.NextToken)
ljefjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljefjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljefjrrsResponseStatus :: Lens.Lens' ListJobExecutionsForJobResponse Core.Int
ljefjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ljefjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
