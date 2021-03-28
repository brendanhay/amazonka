{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListLabelingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of labeling jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListLabelingJobs
    (
    -- * Creating a request
      ListLabelingJobs (..)
    , mkListLabelingJobs
    -- ** Request lenses
    , lljCreationTimeAfter
    , lljCreationTimeBefore
    , lljLastModifiedTimeAfter
    , lljLastModifiedTimeBefore
    , lljMaxResults
    , lljNameContains
    , lljNextToken
    , lljSortBy
    , lljSortOrder
    , lljStatusEquals

    -- * Destructuring the response
    , ListLabelingJobsResponse (..)
    , mkListLabelingJobsResponse
    -- ** Response lenses
    , lljrrsLabelingJobSummaryList
    , lljrrsNextToken
    , lljrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListLabelingJobs' smart constructor.
data ListLabelingJobs = ListLabelingJobs'
  { creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only labeling jobs created after the specified time (timestamp).
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only labeling jobs created before the specified time (timestamp).
  , lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only labeling jobs modified after the specified time (timestamp).
  , lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only labeling jobs modified before the specified time (timestamp).
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of labeling jobs to return in each page of the response.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
  , sortBy :: Core.Maybe Types.SortBy
    -- ^ The field to sort results by. The default is @CreationTime@ .
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order for results. The default is @Ascending@ .
  , statusEquals :: Core.Maybe Types.LabelingJobStatus
    -- ^ A filter that retrieves only labeling jobs with a specific status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListLabelingJobs' value with any optional fields omitted.
mkListLabelingJobs
    :: ListLabelingJobs
mkListLabelingJobs
  = ListLabelingJobs'{creationTimeAfter = Core.Nothing,
                      creationTimeBefore = Core.Nothing,
                      lastModifiedTimeAfter = Core.Nothing,
                      lastModifiedTimeBefore = Core.Nothing, maxResults = Core.Nothing,
                      nameContains = Core.Nothing, nextToken = Core.Nothing,
                      sortBy = Core.Nothing, sortOrder = Core.Nothing,
                      statusEquals = Core.Nothing}

-- | A filter that returns only labeling jobs created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljCreationTimeAfter :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.NominalDiffTime)
lljCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lljCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only labeling jobs created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljCreationTimeBefore :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.NominalDiffTime)
lljCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lljCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | A filter that returns only labeling jobs modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljLastModifiedTimeAfter :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.NominalDiffTime)
lljLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# INLINEABLE lljLastModifiedTimeAfter #-}
{-# DEPRECATED lastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead"  #-}

-- | A filter that returns only labeling jobs modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljLastModifiedTimeBefore :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.NominalDiffTime)
lljLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# INLINEABLE lljLastModifiedTimeBefore #-}
{-# DEPRECATED lastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead"  #-}

-- | The maximum number of labeling jobs to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljMaxResults :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.Natural)
lljMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lljMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljNameContains :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.NameContains)
lljNameContains = Lens.field @"nameContains"
{-# INLINEABLE lljNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljNextToken :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.NextToken)
lljNextToken = Lens.field @"nextToken"
{-# INLINEABLE lljNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljSortBy :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.SortBy)
lljSortBy = Lens.field @"sortBy"
{-# INLINEABLE lljSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljSortOrder :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.SortOrder)
lljSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lljSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | A filter that retrieves only labeling jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljStatusEquals :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.LabelingJobStatus)
lljStatusEquals = Lens.field @"statusEquals"
{-# INLINEABLE lljStatusEquals #-}
{-# DEPRECATED statusEquals "Use generic-lens or generic-optics with 'statusEquals' instead"  #-}

instance Core.ToQuery ListLabelingJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListLabelingJobs where
        toHeaders ListLabelingJobs{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListLabelingJobs") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListLabelingJobs where
        toJSON ListLabelingJobs{..}
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

instance Core.AWSRequest ListLabelingJobs where
        type Rs ListLabelingJobs = ListLabelingJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListLabelingJobsResponse' Core.<$>
                   (x Core..:? "LabelingJobSummaryList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListLabelingJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"labelingJobSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListLabelingJobsResponse' smart constructor.
data ListLabelingJobsResponse = ListLabelingJobsResponse'
  { labelingJobSummaryList :: Core.Maybe [Types.LabelingJobSummary]
    -- ^ An array of @LabelingJobSummary@ objects, each describing a labeling job.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListLabelingJobsResponse' value with any optional fields omitted.
mkListLabelingJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListLabelingJobsResponse
mkListLabelingJobsResponse responseStatus
  = ListLabelingJobsResponse'{labelingJobSummaryList = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
--
-- /Note:/ Consider using 'labelingJobSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrrsLabelingJobSummaryList :: Lens.Lens' ListLabelingJobsResponse (Core.Maybe [Types.LabelingJobSummary])
lljrrsLabelingJobSummaryList = Lens.field @"labelingJobSummaryList"
{-# INLINEABLE lljrrsLabelingJobSummaryList #-}
{-# DEPRECATED labelingJobSummaryList "Use generic-lens or generic-optics with 'labelingJobSummaryList' instead"  #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrrsNextToken :: Lens.Lens' ListLabelingJobsResponse (Core.Maybe Types.NextToken)
lljrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lljrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrrsResponseStatus :: Lens.Lens' ListLabelingJobsResponse Core.Int
lljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
