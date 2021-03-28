{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListLabelingJobsForWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of labeling jobs assigned to a specified work team.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListLabelingJobsForWorkteam
    (
    -- * Creating a request
      ListLabelingJobsForWorkteam (..)
    , mkListLabelingJobsForWorkteam
    -- ** Request lenses
    , lljfwWorkteamArn
    , lljfwCreationTimeAfter
    , lljfwCreationTimeBefore
    , lljfwJobReferenceCodeContains
    , lljfwMaxResults
    , lljfwNextToken
    , lljfwSortBy
    , lljfwSortOrder

    -- * Destructuring the response
    , ListLabelingJobsForWorkteamResponse (..)
    , mkListLabelingJobsForWorkteamResponse
    -- ** Response lenses
    , lljfwrrsLabelingJobSummaryList
    , lljfwrrsNextToken
    , lljfwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListLabelingJobsForWorkteam' smart constructor.
data ListLabelingJobsForWorkteam = ListLabelingJobsForWorkteam'
  { workteamArn :: Types.WorkteamArn
    -- ^ The Amazon Resource Name (ARN) of the work team for which you want to see labeling jobs for.
  , creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only labeling jobs created after the specified time (timestamp).
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only labeling jobs created before the specified time (timestamp).
  , jobReferenceCodeContains :: Core.Maybe Types.JobReferenceCodeContains
    -- ^ A filter the limits jobs to only the ones whose job reference code contains the specified string.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of labeling jobs to return in each page of the response.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous @ListLabelingJobsForWorkteam@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
  , sortBy :: Core.Maybe Types.ListLabelingJobsForWorkteamSortByOptions
    -- ^ The field to sort results by. The default is @CreationTime@ .
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order for results. The default is @Ascending@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListLabelingJobsForWorkteam' value with any optional fields omitted.
mkListLabelingJobsForWorkteam
    :: Types.WorkteamArn -- ^ 'workteamArn'
    -> ListLabelingJobsForWorkteam
mkListLabelingJobsForWorkteam workteamArn
  = ListLabelingJobsForWorkteam'{workteamArn,
                                 creationTimeAfter = Core.Nothing,
                                 creationTimeBefore = Core.Nothing,
                                 jobReferenceCodeContains = Core.Nothing, maxResults = Core.Nothing,
                                 nextToken = Core.Nothing, sortBy = Core.Nothing,
                                 sortOrder = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the work team for which you want to see labeling jobs for.
--
-- /Note:/ Consider using 'workteamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwWorkteamArn :: Lens.Lens' ListLabelingJobsForWorkteam Types.WorkteamArn
lljfwWorkteamArn = Lens.field @"workteamArn"
{-# INLINEABLE lljfwWorkteamArn #-}
{-# DEPRECATED workteamArn "Use generic-lens or generic-optics with 'workteamArn' instead"  #-}

-- | A filter that returns only labeling jobs created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwCreationTimeAfter :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Core.NominalDiffTime)
lljfwCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lljfwCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only labeling jobs created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwCreationTimeBefore :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Core.NominalDiffTime)
lljfwCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lljfwCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | A filter the limits jobs to only the ones whose job reference code contains the specified string.
--
-- /Note:/ Consider using 'jobReferenceCodeContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwJobReferenceCodeContains :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Types.JobReferenceCodeContains)
lljfwJobReferenceCodeContains = Lens.field @"jobReferenceCodeContains"
{-# INLINEABLE lljfwJobReferenceCodeContains #-}
{-# DEPRECATED jobReferenceCodeContains "Use generic-lens or generic-optics with 'jobReferenceCodeContains' instead"  #-}

-- | The maximum number of labeling jobs to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwMaxResults :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Core.Natural)
lljfwMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lljfwMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the result of the previous @ListLabelingJobsForWorkteam@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwNextToken :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Types.NextToken)
lljfwNextToken = Lens.field @"nextToken"
{-# INLINEABLE lljfwNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwSortBy :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Types.ListLabelingJobsForWorkteamSortByOptions)
lljfwSortBy = Lens.field @"sortBy"
{-# INLINEABLE lljfwSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwSortOrder :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Types.SortOrder)
lljfwSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lljfwSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListLabelingJobsForWorkteam where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListLabelingJobsForWorkteam where
        toHeaders ListLabelingJobsForWorkteam{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.ListLabelingJobsForWorkteam")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListLabelingJobsForWorkteam where
        toJSON ListLabelingJobsForWorkteam{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkteamArn" Core..= workteamArn),
                  ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("JobReferenceCodeContains" Core..=) Core.<$>
                    jobReferenceCodeContains,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListLabelingJobsForWorkteam where
        type Rs ListLabelingJobsForWorkteam =
             ListLabelingJobsForWorkteamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListLabelingJobsForWorkteamResponse' Core.<$>
                   (x Core..:? "LabelingJobSummaryList" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListLabelingJobsForWorkteam where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"labelingJobSummaryList") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListLabelingJobsForWorkteamResponse' smart constructor.
data ListLabelingJobsForWorkteamResponse = ListLabelingJobsForWorkteamResponse'
  { labelingJobSummaryList :: [Types.LabelingJobForWorkteamSummary]
    -- ^ An array of @LabelingJobSummary@ objects, each describing a labeling job.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListLabelingJobsForWorkteamResponse' value with any optional fields omitted.
mkListLabelingJobsForWorkteamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListLabelingJobsForWorkteamResponse
mkListLabelingJobsForWorkteamResponse responseStatus
  = ListLabelingJobsForWorkteamResponse'{labelingJobSummaryList =
                                           Core.mempty,
                                         nextToken = Core.Nothing, responseStatus}

-- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
--
-- /Note:/ Consider using 'labelingJobSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrrsLabelingJobSummaryList :: Lens.Lens' ListLabelingJobsForWorkteamResponse [Types.LabelingJobForWorkteamSummary]
lljfwrrsLabelingJobSummaryList = Lens.field @"labelingJobSummaryList"
{-# INLINEABLE lljfwrrsLabelingJobSummaryList #-}
{-# DEPRECATED labelingJobSummaryList "Use generic-lens or generic-optics with 'labelingJobSummaryList' instead"  #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrrsNextToken :: Lens.Lens' ListLabelingJobsForWorkteamResponse (Core.Maybe Types.NextToken)
lljfwrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lljfwrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrrsResponseStatus :: Lens.Lens' ListLabelingJobsForWorkteamResponse Core.Int
lljfwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lljfwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
