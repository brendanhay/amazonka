{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListTranscriptionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transcription jobs with the specified status.
module Network.AWS.Transcribe.ListTranscriptionJobs
    (
    -- * Creating a request
      ListTranscriptionJobs (..)
    , mkListTranscriptionJobs
    -- ** Request lenses
    , ltjJobNameContains
    , ltjMaxResults
    , ltjNextToken
    , ltjStatus

    -- * Destructuring the response
    , ListTranscriptionJobsResponse (..)
    , mkListTranscriptionJobsResponse
    -- ** Response lenses
    , ltjrrsNextToken
    , ltjrrsStatus
    , ltjrrsTranscriptionJobSummaries
    , ltjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkListTranscriptionJobs' smart constructor.
data ListTranscriptionJobs = ListTranscriptionJobs'
  { jobNameContains :: Core.Maybe Types.JobNameContains
    -- ^ When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of jobs to return in the response. If there are fewer results in the list, this response contains only the actual results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous request to @ListTranscriptionJobs@ was truncated, include the @NextToken@ to fetch the next set of jobs.
  , status :: Core.Maybe Types.TranscriptionJobStatus
    -- ^ When specified, returns only transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don’t specify a status, Amazon Transcribe returns all transcription jobs ordered by creation date. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTranscriptionJobs' value with any optional fields omitted.
mkListTranscriptionJobs
    :: ListTranscriptionJobs
mkListTranscriptionJobs
  = ListTranscriptionJobs'{jobNameContains = Core.Nothing,
                           maxResults = Core.Nothing, nextToken = Core.Nothing,
                           status = Core.Nothing}

-- | When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'jobNameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjJobNameContains :: Lens.Lens' ListTranscriptionJobs (Core.Maybe Types.JobNameContains)
ltjJobNameContains = Lens.field @"jobNameContains"
{-# INLINEABLE ltjJobNameContains #-}
{-# DEPRECATED jobNameContains "Use generic-lens or generic-optics with 'jobNameContains' instead"  #-}

-- | The maximum number of jobs to return in the response. If there are fewer results in the list, this response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjMaxResults :: Lens.Lens' ListTranscriptionJobs (Core.Maybe Core.Natural)
ltjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the result of the previous request to @ListTranscriptionJobs@ was truncated, include the @NextToken@ to fetch the next set of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjNextToken :: Lens.Lens' ListTranscriptionJobs (Core.Maybe Types.NextToken)
ltjNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | When specified, returns only transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don’t specify a status, Amazon Transcribe returns all transcription jobs ordered by creation date. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjStatus :: Lens.Lens' ListTranscriptionJobs (Core.Maybe Types.TranscriptionJobStatus)
ltjStatus = Lens.field @"status"
{-# INLINEABLE ltjStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListTranscriptionJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTranscriptionJobs where
        toHeaders ListTranscriptionJobs{..}
          = Core.pure ("X-Amz-Target", "Transcribe.ListTranscriptionJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTranscriptionJobs where
        toJSON ListTranscriptionJobs{..}
          = Core.object
              (Core.catMaybes
                 [("JobNameContains" Core..=) Core.<$> jobNameContains,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Status" Core..=) Core.<$> status])

instance Core.AWSRequest ListTranscriptionJobs where
        type Rs ListTranscriptionJobs = ListTranscriptionJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTranscriptionJobsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Status" Core.<*>
                     x Core..:? "TranscriptionJobSummaries"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTranscriptionJobsResponse' smart constructor.
data ListTranscriptionJobsResponse = ListTranscriptionJobsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The @ListTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListTranscriptionJobs@ operation to return in the next page of jobs.
  , status :: Core.Maybe Types.TranscriptionJobStatus
    -- ^ The requested status of the jobs returned.
  , transcriptionJobSummaries :: Core.Maybe [Types.TranscriptionJobSummary]
    -- ^ A list of objects containing summary information for a transcription job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTranscriptionJobsResponse' value with any optional fields omitted.
mkListTranscriptionJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTranscriptionJobsResponse
mkListTranscriptionJobsResponse responseStatus
  = ListTranscriptionJobsResponse'{nextToken = Core.Nothing,
                                   status = Core.Nothing, transcriptionJobSummaries = Core.Nothing,
                                   responseStatus}

-- | The @ListTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListTranscriptionJobs@ operation to return in the next page of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrrsNextToken :: Lens.Lens' ListTranscriptionJobsResponse (Core.Maybe Types.NextToken)
ltjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The requested status of the jobs returned.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrrsStatus :: Lens.Lens' ListTranscriptionJobsResponse (Core.Maybe Types.TranscriptionJobStatus)
ltjrrsStatus = Lens.field @"status"
{-# INLINEABLE ltjrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A list of objects containing summary information for a transcription job.
--
-- /Note:/ Consider using 'transcriptionJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrrsTranscriptionJobSummaries :: Lens.Lens' ListTranscriptionJobsResponse (Core.Maybe [Types.TranscriptionJobSummary])
ltjrrsTranscriptionJobSummaries = Lens.field @"transcriptionJobSummaries"
{-# INLINEABLE ltjrrsTranscriptionJobSummaries #-}
{-# DEPRECATED transcriptionJobSummaries "Use generic-lens or generic-optics with 'transcriptionJobSummaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrrsResponseStatus :: Lens.Lens' ListTranscriptionJobsResponse Core.Int
ltjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
