{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your most recently created jobs. This array includes in-process, completed, and errored jobs. This will return the jobs themselves, not just a list of the jobs. To retrieve the twenty next most recent jobs, use the nextToken string returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListJobs
    (
    -- * Creating a request
      ListJobs (..)
    , mkListJobs
    -- ** Request lenses
    , ljMaxResults
    , ljNextToken
    , ljOrder
    , ljQueue
    , ljStatus

    -- * Destructuring the response
    , ListJobsResponse (..)
    , mkListJobsResponse
    -- ** Response lenses
    , ljrrsJobs
    , ljrrsNextToken
    , ljrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Optional. Number of jobs, up to twenty, that will be returned at one time.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Optional. Use this string, provided with the response to a previous request, to request the next batch of jobs.
  , order :: Core.Maybe Types.Order
    -- ^ Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
  , queue :: Core.Maybe Core.Text
    -- ^ Optional. Provide a queue name to get back only jobs from that queue.
  , status :: Core.Maybe Types.JobStatus
    -- ^ Optional. A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJobs' value with any optional fields omitted.
mkListJobs
    :: ListJobs
mkListJobs
  = ListJobs'{maxResults = Core.Nothing, nextToken = Core.Nothing,
              order = Core.Nothing, queue = Core.Nothing, status = Core.Nothing}

-- | Optional. Number of jobs, up to twenty, that will be returned at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxResults :: Lens.Lens' ListJobs (Core.Maybe Core.Natural)
ljMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ljMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Optional. Use this string, provided with the response to a previous request, to request the next batch of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljNextToken :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
ljNextToken = Lens.field @"nextToken"
{-# INLINEABLE ljNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljOrder :: Lens.Lens' ListJobs (Core.Maybe Types.Order)
ljOrder = Lens.field @"order"
{-# INLINEABLE ljOrder #-}
{-# DEPRECATED order "Use generic-lens or generic-optics with 'order' instead"  #-}

-- | Optional. Provide a queue name to get back only jobs from that queue.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljQueue :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
ljQueue = Lens.field @"queue"
{-# INLINEABLE ljQueue #-}
{-# DEPRECATED queue "Use generic-lens or generic-optics with 'queue' instead"  #-}

-- | Optional. A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljStatus :: Lens.Lens' ListJobs (Core.Maybe Types.JobStatus)
ljStatus = Lens.field @"status"
{-# INLINEABLE ljStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListJobs where
        toQuery ListJobs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "order") order
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "queue") queue
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "status") status

instance Core.ToHeaders ListJobs where
        toHeaders ListJobs{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/2017-08-29/jobs",
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
  { jobs :: Core.Maybe [Types.Job]
    -- ^ List of jobs
  , nextToken :: Core.Maybe Core.Text
    -- ^ Use this string to request the next batch of jobs.
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

-- | List of jobs
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsJobs :: Lens.Lens' ListJobsResponse (Core.Maybe [Types.Job])
ljrrsJobs = Lens.field @"jobs"
{-# INLINEABLE ljrrsJobs #-}
{-# DEPRECATED jobs "Use generic-lens or generic-optics with 'jobs' instead"  #-}

-- | Use this string to request the next batch of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsNextToken :: Lens.Lens' ListJobsResponse (Core.Maybe Core.Text)
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
