{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrainingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists training jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrainingJobs
  ( -- * Creating a request
    ListTrainingJobs (..),
    mkListTrainingJobs,

    -- ** Request lenses
    ltjsCreationTimeAfter,
    ltjsCreationTimeBefore,
    ltjsLastModifiedTimeAfter,
    ltjsLastModifiedTimeBefore,
    ltjsMaxResults,
    ltjsNameContains,
    ltjsNextToken,
    ltjsSortBy,
    ltjsSortOrder,
    ltjsStatusEquals,

    -- * Destructuring the response
    ListTrainingJobsResponse (..),
    mkListTrainingJobsResponse,

    -- ** Response lenses
    ltjrrsTrainingJobSummaries,
    ltjrrsNextToken,
    ltjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListTrainingJobs' smart constructor.
data ListTrainingJobs = ListTrainingJobs'
  { -- | A filter that returns only training jobs created after the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only training jobs created before the specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only training jobs modified after the specified time (timestamp).
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only training jobs modified before the specified time (timestamp).
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of training jobs to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the training job name. This filter returns only training jobs whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.SortBy,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A filter that retrieves only training jobs with a specific status.
    statusEquals :: Core.Maybe Types.TrainingJobStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTrainingJobs' value with any optional fields omitted.
mkListTrainingJobs ::
  ListTrainingJobs
mkListTrainingJobs =
  ListTrainingJobs'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      lastModifiedTimeBefore = Core.Nothing,
      maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing,
      statusEquals = Core.Nothing
    }

-- | A filter that returns only training jobs created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsCreationTimeAfter :: Lens.Lens' ListTrainingJobs (Core.Maybe Core.NominalDiffTime)
ltjsCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED ltjsCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only training jobs created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsCreationTimeBefore :: Lens.Lens' ListTrainingJobs (Core.Maybe Core.NominalDiffTime)
ltjsCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED ltjsCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only training jobs modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsLastModifiedTimeAfter :: Lens.Lens' ListTrainingJobs (Core.Maybe Core.NominalDiffTime)
ltjsLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED ltjsLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only training jobs modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsLastModifiedTimeBefore :: Lens.Lens' ListTrainingJobs (Core.Maybe Core.NominalDiffTime)
ltjsLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED ltjsLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | The maximum number of training jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsMaxResults :: Lens.Lens' ListTrainingJobs (Core.Maybe Core.Natural)
ltjsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltjsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the training job name. This filter returns only training jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsNameContains :: Lens.Lens' ListTrainingJobs (Core.Maybe Types.NameContains)
ltjsNameContains = Lens.field @"nameContains"
{-# DEPRECATED ltjsNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsNextToken :: Lens.Lens' ListTrainingJobs (Core.Maybe Types.NextToken)
ltjsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltjsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsSortBy :: Lens.Lens' ListTrainingJobs (Core.Maybe Types.SortBy)
ltjsSortBy = Lens.field @"sortBy"
{-# DEPRECATED ltjsSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsSortOrder :: Lens.Lens' ListTrainingJobs (Core.Maybe Types.SortOrder)
ltjsSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED ltjsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that retrieves only training jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsStatusEquals :: Lens.Lens' ListTrainingJobs (Core.Maybe Types.TrainingJobStatus)
ltjsStatusEquals = Lens.field @"statusEquals"
{-# DEPRECATED ltjsStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

instance Core.FromJSON ListTrainingJobs where
  toJSON ListTrainingJobs {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("StatusEquals" Core..=) Core.<$> statusEquals
          ]
      )

instance Core.AWSRequest ListTrainingJobs where
  type Rs ListTrainingJobs = ListTrainingJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListTrainingJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrainingJobsResponse'
            Core.<$> (x Core..:? "TrainingJobSummaries" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTrainingJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"trainingJobSummaries") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTrainingJobsResponse' smart constructor.
data ListTrainingJobsResponse = ListTrainingJobsResponse'
  { -- | An array of @TrainingJobSummary@ objects, each listing a training job.
    trainingJobSummaries :: [Types.TrainingJobSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTrainingJobsResponse' value with any optional fields omitted.
mkListTrainingJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTrainingJobsResponse
mkListTrainingJobsResponse responseStatus =
  ListTrainingJobsResponse'
    { trainingJobSummaries = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @TrainingJobSummary@ objects, each listing a training job.
--
-- /Note:/ Consider using 'trainingJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrrsTrainingJobSummaries :: Lens.Lens' ListTrainingJobsResponse [Types.TrainingJobSummary]
ltjrrsTrainingJobSummaries = Lens.field @"trainingJobSummaries"
{-# DEPRECATED ltjrrsTrainingJobSummaries "Use generic-lens or generic-optics with 'trainingJobSummaries' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrrsNextToken :: Lens.Lens' ListTrainingJobsResponse (Core.Maybe Types.NextToken)
ltjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrrsResponseStatus :: Lens.Lens' ListTrainingJobsResponse Core.Int
ltjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
