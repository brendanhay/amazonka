{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of 'TrainingJobSummary' objects that describe the training jobs that a hyperparameter tuning job launched.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
    (
    -- * Creating a request
      ListTrainingJobsForHyperParameterTuningJob (..)
    , mkListTrainingJobsForHyperParameterTuningJob
    -- ** Request lenses
    , ltjfhptjHyperParameterTuningJobName
    , ltjfhptjMaxResults
    , ltjfhptjNextToken
    , ltjfhptjSortBy
    , ltjfhptjSortOrder
    , ltjfhptjStatusEquals

    -- * Destructuring the response
    , ListTrainingJobsForHyperParameterTuningJobResponse (..)
    , mkListTrainingJobsForHyperParameterTuningJobResponse
    -- ** Response lenses
    , ltjfhptjrrsTrainingJobSummaries
    , ltjfhptjrrsNextToken
    , ltjfhptjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListTrainingJobsForHyperParameterTuningJob' smart constructor.
data ListTrainingJobsForHyperParameterTuningJob = ListTrainingJobsForHyperParameterTuningJob'
  { hyperParameterTuningJobName :: Types.HyperParameterTuningJobName
    -- ^ The name of the tuning job whose training jobs you want to list.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of training jobs to return. The default value is 10.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
  , sortBy :: Core.Maybe Types.TrainingJobSortByOptions
    -- ^ The field to sort results by. The default is @Name@ .
--
-- If the value of this field is @FinalObjectiveMetricValue@ , any training jobs that did not return an objective metric are not listed.
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order for results. The default is @Ascending@ .
  , statusEquals :: Core.Maybe Types.TrainingJobStatus
    -- ^ A filter that returns only training jobs with the specified status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrainingJobsForHyperParameterTuningJob' value with any optional fields omitted.
mkListTrainingJobsForHyperParameterTuningJob
    :: Types.HyperParameterTuningJobName -- ^ 'hyperParameterTuningJobName'
    -> ListTrainingJobsForHyperParameterTuningJob
mkListTrainingJobsForHyperParameterTuningJob
  hyperParameterTuningJobName
  = ListTrainingJobsForHyperParameterTuningJob'{hyperParameterTuningJobName,
                                                maxResults = Core.Nothing, nextToken = Core.Nothing,
                                                sortBy = Core.Nothing, sortOrder = Core.Nothing,
                                                statusEquals = Core.Nothing}

-- | The name of the tuning job whose training jobs you want to list.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjHyperParameterTuningJobName :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob Types.HyperParameterTuningJobName
ltjfhptjHyperParameterTuningJobName = Lens.field @"hyperParameterTuningJobName"
{-# INLINEABLE ltjfhptjHyperParameterTuningJobName #-}
{-# DEPRECATED hyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead"  #-}

-- | The maximum number of training jobs to return. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjMaxResults :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Core.Maybe Core.Natural)
ltjfhptjMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltjfhptjMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the result of the previous @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjNextToken :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Core.Maybe Types.NextToken)
ltjfhptjNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltjfhptjNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The field to sort results by. The default is @Name@ .
--
-- If the value of this field is @FinalObjectiveMetricValue@ , any training jobs that did not return an objective metric are not listed.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjSortBy :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Core.Maybe Types.TrainingJobSortByOptions)
ltjfhptjSortBy = Lens.field @"sortBy"
{-# INLINEABLE ltjfhptjSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjSortOrder :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Core.Maybe Types.SortOrder)
ltjfhptjSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE ltjfhptjSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | A filter that returns only training jobs with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjStatusEquals :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Core.Maybe Types.TrainingJobStatus)
ltjfhptjStatusEquals = Lens.field @"statusEquals"
{-# INLINEABLE ltjfhptjStatusEquals #-}
{-# DEPRECATED statusEquals "Use generic-lens or generic-optics with 'statusEquals' instead"  #-}

instance Core.ToQuery ListTrainingJobsForHyperParameterTuningJob
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTrainingJobsForHyperParameterTuningJob
         where
        toHeaders ListTrainingJobsForHyperParameterTuningJob{..}
          = Core.pure
              ("X-Amz-Target",
               "SageMaker.ListTrainingJobsForHyperParameterTuningJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTrainingJobsForHyperParameterTuningJob
         where
        toJSON ListTrainingJobsForHyperParameterTuningJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("HyperParameterTuningJobName" Core..=
                       hyperParameterTuningJobName),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder,
                  ("StatusEquals" Core..=) Core.<$> statusEquals])

instance Core.AWSRequest ListTrainingJobsForHyperParameterTuningJob
         where
        type Rs ListTrainingJobsForHyperParameterTuningJob =
             ListTrainingJobsForHyperParameterTuningJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTrainingJobsForHyperParameterTuningJobResponse' Core.<$>
                   (x Core..:? "TrainingJobSummaries" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTrainingJobsForHyperParameterTuningJob
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"trainingJobSummaries") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTrainingJobsForHyperParameterTuningJobResponse' smart constructor.
data ListTrainingJobsForHyperParameterTuningJobResponse = ListTrainingJobsForHyperParameterTuningJobResponse'
  { trainingJobSummaries :: [Types.HyperParameterTrainingJobSummary]
    -- ^ A list of 'TrainingJobSummary' objects that describe the training jobs that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of this @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTrainingJobsForHyperParameterTuningJobResponse' value with any optional fields omitted.
mkListTrainingJobsForHyperParameterTuningJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTrainingJobsForHyperParameterTuningJobResponse
mkListTrainingJobsForHyperParameterTuningJobResponse responseStatus
  = ListTrainingJobsForHyperParameterTuningJobResponse'{trainingJobSummaries
                                                          = Core.mempty,
                                                        nextToken = Core.Nothing, responseStatus}

-- | A list of 'TrainingJobSummary' objects that describe the training jobs that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
--
-- /Note:/ Consider using 'trainingJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjrrsTrainingJobSummaries :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse [Types.HyperParameterTrainingJobSummary]
ltjfhptjrrsTrainingJobSummaries = Lens.field @"trainingJobSummaries"
{-# INLINEABLE ltjfhptjrrsTrainingJobSummaries #-}
{-# DEPRECATED trainingJobSummaries "Use generic-lens or generic-optics with 'trainingJobSummaries' instead"  #-}

-- | If the result of this @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjrrsNextToken :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse (Core.Maybe Types.NextToken)
ltjfhptjrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltjfhptjrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjrrsResponseStatus :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse Core.Int
ltjfhptjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltjfhptjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
