{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListLabelingJobs (..),
    mkListLabelingJobs,

    -- ** Request lenses
    lljCreationTimeAfter,
    lljCreationTimeBefore,
    lljLastModifiedTimeAfter,
    lljLastModifiedTimeBefore,
    lljMaxResults,
    lljNameContains,
    lljNextToken,
    lljSortBy,
    lljSortOrder,
    lljStatusEquals,

    -- * Destructuring the response
    ListLabelingJobsResponse (..),
    mkListLabelingJobsResponse,

    -- ** Response lenses
    lljrrsLabelingJobSummaryList,
    lljrrsNextToken,
    lljrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListLabelingJobs' smart constructor.
data ListLabelingJobs = ListLabelingJobs'
  { -- | A filter that returns only labeling jobs created after the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only labeling jobs created before the specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only labeling jobs modified after the specified time (timestamp).
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only labeling jobs modified before the specified time (timestamp).
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of labeling jobs to return in each page of the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.SortBy,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A filter that retrieves only labeling jobs with a specific status.
    statusEquals :: Core.Maybe Types.LabelingJobStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListLabelingJobs' value with any optional fields omitted.
mkListLabelingJobs ::
  ListLabelingJobs
mkListLabelingJobs =
  ListLabelingJobs'
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

-- | A filter that returns only labeling jobs created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljCreationTimeAfter :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.NominalDiffTime)
lljCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lljCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only labeling jobs created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljCreationTimeBefore :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.NominalDiffTime)
lljCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lljCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only labeling jobs modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljLastModifiedTimeAfter :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.NominalDiffTime)
lljLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED lljLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only labeling jobs modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljLastModifiedTimeBefore :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.NominalDiffTime)
lljLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED lljLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | The maximum number of labeling jobs to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljMaxResults :: Lens.Lens' ListLabelingJobs (Core.Maybe Core.Natural)
lljMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljNameContains :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.NameContains)
lljNameContains = Lens.field @"nameContains"
{-# DEPRECATED lljNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljNextToken :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.NextToken)
lljNextToken = Lens.field @"nextToken"
{-# DEPRECATED lljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljSortBy :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.SortBy)
lljSortBy = Lens.field @"sortBy"
{-# DEPRECATED lljSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljSortOrder :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.SortOrder)
lljSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lljSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that retrieves only labeling jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljStatusEquals :: Lens.Lens' ListLabelingJobs (Core.Maybe Types.LabelingJobStatus)
lljStatusEquals = Lens.field @"statusEquals"
{-# DEPRECATED lljStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

instance Core.FromJSON ListLabelingJobs where
  toJSON ListLabelingJobs {..} =
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

instance Core.AWSRequest ListLabelingJobs where
  type Rs ListLabelingJobs = ListLabelingJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListLabelingJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLabelingJobsResponse'
            Core.<$> (x Core..:? "LabelingJobSummaryList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLabelingJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"labelingJobSummaryList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListLabelingJobsResponse' smart constructor.
data ListLabelingJobsResponse = ListLabelingJobsResponse'
  { -- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
    labelingJobSummaryList :: Core.Maybe [Types.LabelingJobSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListLabelingJobsResponse' value with any optional fields omitted.
mkListLabelingJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLabelingJobsResponse
mkListLabelingJobsResponse responseStatus =
  ListLabelingJobsResponse'
    { labelingJobSummaryList = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
--
-- /Note:/ Consider using 'labelingJobSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrrsLabelingJobSummaryList :: Lens.Lens' ListLabelingJobsResponse (Core.Maybe [Types.LabelingJobSummary])
lljrrsLabelingJobSummaryList = Lens.field @"labelingJobSummaryList"
{-# DEPRECATED lljrrsLabelingJobSummaryList "Use generic-lens or generic-optics with 'labelingJobSummaryList' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrrsNextToken :: Lens.Lens' ListLabelingJobsResponse (Core.Maybe Types.NextToken)
lljrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lljrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrrsResponseStatus :: Lens.Lens' ListLabelingJobsResponse Core.Int
lljrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lljrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
