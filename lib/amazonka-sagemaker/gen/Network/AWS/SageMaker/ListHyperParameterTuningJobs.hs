{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListHyperParameterTuningJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of 'HyperParameterTuningJobSummary' objects that describe the hyperparameter tuning jobs launched in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListHyperParameterTuningJobs
    (
    -- * Creating a request
      ListHyperParameterTuningJobs (..)
    , mkListHyperParameterTuningJobs
    -- ** Request lenses
    , lhptjCreationTimeAfter
    , lhptjCreationTimeBefore
    , lhptjLastModifiedTimeAfter
    , lhptjLastModifiedTimeBefore
    , lhptjMaxResults
    , lhptjNameContains
    , lhptjNextToken
    , lhptjSortBy
    , lhptjSortOrder
    , lhptjStatusEquals

    -- * Destructuring the response
    , ListHyperParameterTuningJobsResponse (..)
    , mkListHyperParameterTuningJobsResponse
    -- ** Response lenses
    , lhptjrrsHyperParameterTuningJobSummaries
    , lhptjrrsNextToken
    , lhptjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListHyperParameterTuningJobs' smart constructor.
data ListHyperParameterTuningJobs = ListHyperParameterTuningJobs'
  { creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only tuning jobs that were created after the specified time.
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only tuning jobs that were created before the specified time.
  , lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only tuning jobs that were modified after the specified time.
  , lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only tuning jobs that were modified before the specified time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of tuning jobs to return. The default value is 10.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ A string in the tuning job name. This filter returns only tuning jobs whose name contains the specified string.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
  , sortBy :: Core.Maybe Types.HyperParameterTuningJobSortByOptions
    -- ^ The field to sort results by. The default is @Name@ .
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order for results. The default is @Ascending@ .
  , statusEquals :: Core.Maybe Types.HyperParameterTuningJobStatus
    -- ^ A filter that returns only tuning jobs with the specified status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListHyperParameterTuningJobs' value with any optional fields omitted.
mkListHyperParameterTuningJobs
    :: ListHyperParameterTuningJobs
mkListHyperParameterTuningJobs
  = ListHyperParameterTuningJobs'{creationTimeAfter = Core.Nothing,
                                  creationTimeBefore = Core.Nothing,
                                  lastModifiedTimeAfter = Core.Nothing,
                                  lastModifiedTimeBefore = Core.Nothing, maxResults = Core.Nothing,
                                  nameContains = Core.Nothing, nextToken = Core.Nothing,
                                  sortBy = Core.Nothing, sortOrder = Core.Nothing,
                                  statusEquals = Core.Nothing}

-- | A filter that returns only tuning jobs that were created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjCreationTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.NominalDiffTime)
lhptjCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lhptjCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only tuning jobs that were created before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjCreationTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.NominalDiffTime)
lhptjCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lhptjCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | A filter that returns only tuning jobs that were modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjLastModifiedTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.NominalDiffTime)
lhptjLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# INLINEABLE lhptjLastModifiedTimeAfter #-}
{-# DEPRECATED lastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead"  #-}

-- | A filter that returns only tuning jobs that were modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjLastModifiedTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.NominalDiffTime)
lhptjLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# INLINEABLE lhptjLastModifiedTimeBefore #-}
{-# DEPRECATED lastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead"  #-}

-- | The maximum number of tuning jobs to return. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjMaxResults :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.Natural)
lhptjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lhptjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A string in the tuning job name. This filter returns only tuning jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjNameContains :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.NameContains)
lhptjNameContains = Lens.field @"nameContains"
{-# INLINEABLE lhptjNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the result of the previous @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjNextToken :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.NextToken)
lhptjNextToken = Lens.field @"nextToken"
{-# INLINEABLE lhptjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The field to sort results by. The default is @Name@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjSortBy :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.HyperParameterTuningJobSortByOptions)
lhptjSortBy = Lens.field @"sortBy"
{-# INLINEABLE lhptjSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjSortOrder :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.SortOrder)
lhptjSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lhptjSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | A filter that returns only tuning jobs with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjStatusEquals :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.HyperParameterTuningJobStatus)
lhptjStatusEquals = Lens.field @"statusEquals"
{-# INLINEABLE lhptjStatusEquals #-}
{-# DEPRECATED statusEquals "Use generic-lens or generic-optics with 'statusEquals' instead"  #-}

instance Core.ToQuery ListHyperParameterTuningJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListHyperParameterTuningJobs where
        toHeaders ListHyperParameterTuningJobs{..}
          = Core.pure
              ("X-Amz-Target", "SageMaker.ListHyperParameterTuningJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListHyperParameterTuningJobs where
        toJSON ListHyperParameterTuningJobs{..}
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

instance Core.AWSRequest ListHyperParameterTuningJobs where
        type Rs ListHyperParameterTuningJobs =
             ListHyperParameterTuningJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListHyperParameterTuningJobsResponse' Core.<$>
                   (x Core..:? "HyperParameterTuningJobSummaries" Core..!=
                      Core.mempty)
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListHyperParameterTuningJobs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^. Lens.field @"hyperParameterTuningJobSummaries")
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListHyperParameterTuningJobsResponse' smart constructor.
data ListHyperParameterTuningJobsResponse = ListHyperParameterTuningJobsResponse'
  { hyperParameterTuningJobSummaries :: [Types.HyperParameterTuningJobSummary]
    -- ^ A list of 'HyperParameterTuningJobSummary' objects that describe the tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of this @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListHyperParameterTuningJobsResponse' value with any optional fields omitted.
mkListHyperParameterTuningJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListHyperParameterTuningJobsResponse
mkListHyperParameterTuningJobsResponse responseStatus
  = ListHyperParameterTuningJobsResponse'{hyperParameterTuningJobSummaries
                                            = Core.mempty,
                                          nextToken = Core.Nothing, responseStatus}

-- | A list of 'HyperParameterTuningJobSummary' objects that describe the tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
--
-- /Note:/ Consider using 'hyperParameterTuningJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrrsHyperParameterTuningJobSummaries :: Lens.Lens' ListHyperParameterTuningJobsResponse [Types.HyperParameterTuningJobSummary]
lhptjrrsHyperParameterTuningJobSummaries = Lens.field @"hyperParameterTuningJobSummaries"
{-# INLINEABLE lhptjrrsHyperParameterTuningJobSummaries #-}
{-# DEPRECATED hyperParameterTuningJobSummaries "Use generic-lens or generic-optics with 'hyperParameterTuningJobSummaries' instead"  #-}

-- | If the result of this @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrrsNextToken :: Lens.Lens' ListHyperParameterTuningJobsResponse (Core.Maybe Types.NextToken)
lhptjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lhptjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrrsResponseStatus :: Lens.Lens' ListHyperParameterTuningJobsResponse Core.Int
lhptjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lhptjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
