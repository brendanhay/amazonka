{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListProcessingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists processing jobs that satisfy various filters.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListProcessingJobs
    (
    -- * Creating a request
      ListProcessingJobs (..)
    , mkListProcessingJobs
    -- ** Request lenses
    , lpjCreationTimeAfter
    , lpjCreationTimeBefore
    , lpjLastModifiedTimeAfter
    , lpjLastModifiedTimeBefore
    , lpjMaxResults
    , lpjNameContains
    , lpjNextToken
    , lpjSortBy
    , lpjSortOrder
    , lpjStatusEquals

    -- * Destructuring the response
    , ListProcessingJobsResponse (..)
    , mkListProcessingJobsResponse
    -- ** Response lenses
    , lpjrrsProcessingJobSummaries
    , lpjrrsNextToken
    , lpjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListProcessingJobs' smart constructor.
data ListProcessingJobs = ListProcessingJobs'
  { creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only processing jobs created after the specified time.
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only processing jobs created after the specified time.
  , lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only processing jobs modified after the specified time.
  , lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only processing jobs modified before the specified time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of processing jobs to return in the response.
  , nameContains :: Core.Maybe Core.Text
    -- ^ A string in the processing job name. This filter returns only processing jobs whose name contains the specified string.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous @ListProcessingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of processing jobs, use the token in the next request.
  , sortBy :: Core.Maybe Types.SortBy
    -- ^ The field to sort results by. The default is @CreationTime@ .
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order for results. The default is @Ascending@ .
  , statusEquals :: Core.Maybe Types.ProcessingJobStatus
    -- ^ A filter that retrieves only processing jobs with a specific status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListProcessingJobs' value with any optional fields omitted.
mkListProcessingJobs
    :: ListProcessingJobs
mkListProcessingJobs
  = ListProcessingJobs'{creationTimeAfter = Core.Nothing,
                        creationTimeBefore = Core.Nothing,
                        lastModifiedTimeAfter = Core.Nothing,
                        lastModifiedTimeBefore = Core.Nothing, maxResults = Core.Nothing,
                        nameContains = Core.Nothing, nextToken = Core.Nothing,
                        sortBy = Core.Nothing, sortOrder = Core.Nothing,
                        statusEquals = Core.Nothing}

-- | A filter that returns only processing jobs created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjCreationTimeAfter :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.NominalDiffTime)
lpjCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lpjCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only processing jobs created after the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjCreationTimeBefore :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.NominalDiffTime)
lpjCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lpjCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | A filter that returns only processing jobs modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjLastModifiedTimeAfter :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.NominalDiffTime)
lpjLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# INLINEABLE lpjLastModifiedTimeAfter #-}
{-# DEPRECATED lastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead"  #-}

-- | A filter that returns only processing jobs modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjLastModifiedTimeBefore :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.NominalDiffTime)
lpjLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# INLINEABLE lpjLastModifiedTimeBefore #-}
{-# DEPRECATED lastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead"  #-}

-- | The maximum number of processing jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjMaxResults :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.Natural)
lpjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A string in the processing job name. This filter returns only processing jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjNameContains :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.Text)
lpjNameContains = Lens.field @"nameContains"
{-# INLINEABLE lpjNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the result of the previous @ListProcessingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of processing jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjNextToken :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.NextToken)
lpjNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjSortBy :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.SortBy)
lpjSortBy = Lens.field @"sortBy"
{-# INLINEABLE lpjSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjSortOrder :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.SortOrder)
lpjSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lpjSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | A filter that retrieves only processing jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjStatusEquals :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.ProcessingJobStatus)
lpjStatusEquals = Lens.field @"statusEquals"
{-# INLINEABLE lpjStatusEquals #-}
{-# DEPRECATED statusEquals "Use generic-lens or generic-optics with 'statusEquals' instead"  #-}

instance Core.ToQuery ListProcessingJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListProcessingJobs where
        toHeaders ListProcessingJobs{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListProcessingJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListProcessingJobs where
        toJSON ListProcessingJobs{..}
          = Core.object
              (Core.catMaybes
                 [("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
                  ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NameContains" Core..=) Core.<$> nameContains,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder,
                  ("StatusEquals" Core..=) Core.<$> statusEquals])

instance Core.AWSRequest ListProcessingJobs where
        type Rs ListProcessingJobs = ListProcessingJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListProcessingJobsResponse' Core.<$>
                   (x Core..:? "ProcessingJobSummaries" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListProcessingJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"processingJobSummaries") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListProcessingJobsResponse' smart constructor.
data ListProcessingJobsResponse = ListProcessingJobsResponse'
  { processingJobSummaries :: [Types.ProcessingJobSummary]
    -- ^ An array of @ProcessingJobSummary@ objects, each listing a processing job.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of processing jobs, use it in the subsequent request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListProcessingJobsResponse' value with any optional fields omitted.
mkListProcessingJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListProcessingJobsResponse
mkListProcessingJobsResponse responseStatus
  = ListProcessingJobsResponse'{processingJobSummaries = Core.mempty,
                                nextToken = Core.Nothing, responseStatus}

-- | An array of @ProcessingJobSummary@ objects, each listing a processing job.
--
-- /Note:/ Consider using 'processingJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrrsProcessingJobSummaries :: Lens.Lens' ListProcessingJobsResponse [Types.ProcessingJobSummary]
lpjrrsProcessingJobSummaries = Lens.field @"processingJobSummaries"
{-# INLINEABLE lpjrrsProcessingJobSummaries #-}
{-# DEPRECATED processingJobSummaries "Use generic-lens or generic-optics with 'processingJobSummaries' instead"  #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of processing jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrrsNextToken :: Lens.Lens' ListProcessingJobsResponse (Core.Maybe Types.NextToken)
lpjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrrsResponseStatus :: Lens.Lens' ListProcessingJobsResponse Core.Int
lpjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
