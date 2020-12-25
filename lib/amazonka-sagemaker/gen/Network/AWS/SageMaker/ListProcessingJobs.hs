{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListProcessingJobs (..),
    mkListProcessingJobs,

    -- ** Request lenses
    lpjCreationTimeAfter,
    lpjCreationTimeBefore,
    lpjLastModifiedTimeAfter,
    lpjLastModifiedTimeBefore,
    lpjMaxResults,
    lpjNameContains,
    lpjNextToken,
    lpjSortBy,
    lpjSortOrder,
    lpjStatusEquals,

    -- * Destructuring the response
    ListProcessingJobsResponse (..),
    mkListProcessingJobsResponse,

    -- ** Response lenses
    lpjrrsProcessingJobSummaries,
    lpjrrsNextToken,
    lpjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListProcessingJobs' smart constructor.
data ListProcessingJobs = ListProcessingJobs'
  { -- | A filter that returns only processing jobs created after the specified time.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only processing jobs created after the specified time.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only processing jobs modified after the specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only processing jobs modified before the specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of processing jobs to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the processing job name. This filter returns only processing jobs whose name contains the specified string.
    nameContains :: Core.Maybe Types.String,
    -- | If the result of the previous @ListProcessingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of processing jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.SortBy,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A filter that retrieves only processing jobs with a specific status.
    statusEquals :: Core.Maybe Types.ProcessingJobStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListProcessingJobs' value with any optional fields omitted.
mkListProcessingJobs ::
  ListProcessingJobs
mkListProcessingJobs =
  ListProcessingJobs'
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

-- | A filter that returns only processing jobs created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjCreationTimeAfter :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.NominalDiffTime)
lpjCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lpjCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only processing jobs created after the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjCreationTimeBefore :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.NominalDiffTime)
lpjCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lpjCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only processing jobs modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjLastModifiedTimeAfter :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.NominalDiffTime)
lpjLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED lpjLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only processing jobs modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjLastModifiedTimeBefore :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.NominalDiffTime)
lpjLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED lpjLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | The maximum number of processing jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjMaxResults :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.Natural)
lpjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the processing job name. This filter returns only processing jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjNameContains :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.String)
lpjNameContains = Lens.field @"nameContains"
{-# DEPRECATED lpjNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListProcessingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of processing jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjNextToken :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.NextToken)
lpjNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjSortBy :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.SortBy)
lpjSortBy = Lens.field @"sortBy"
{-# DEPRECATED lpjSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjSortOrder :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.SortOrder)
lpjSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lpjSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that retrieves only processing jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjStatusEquals :: Lens.Lens' ListProcessingJobs (Core.Maybe Types.ProcessingJobStatus)
lpjStatusEquals = Lens.field @"statusEquals"
{-# DEPRECATED lpjStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

instance Core.FromJSON ListProcessingJobs where
  toJSON ListProcessingJobs {..} =
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

instance Core.AWSRequest ListProcessingJobs where
  type Rs ListProcessingJobs = ListProcessingJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListProcessingJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProcessingJobsResponse'
            Core.<$> (x Core..:? "ProcessingJobSummaries" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListProcessingJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"processingJobSummaries") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListProcessingJobsResponse' smart constructor.
data ListProcessingJobsResponse = ListProcessingJobsResponse'
  { -- | An array of @ProcessingJobSummary@ objects, each listing a processing job.
    processingJobSummaries :: [Types.ProcessingJobSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of processing jobs, use it in the subsequent request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListProcessingJobsResponse' value with any optional fields omitted.
mkListProcessingJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProcessingJobsResponse
mkListProcessingJobsResponse responseStatus =
  ListProcessingJobsResponse'
    { processingJobSummaries = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @ProcessingJobSummary@ objects, each listing a processing job.
--
-- /Note:/ Consider using 'processingJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrrsProcessingJobSummaries :: Lens.Lens' ListProcessingJobsResponse [Types.ProcessingJobSummary]
lpjrrsProcessingJobSummaries = Lens.field @"processingJobSummaries"
{-# DEPRECATED lpjrrsProcessingJobSummaries "Use generic-lens or generic-optics with 'processingJobSummaries' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of processing jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrrsNextToken :: Lens.Lens' ListProcessingJobsResponse (Core.Maybe Types.NextToken)
lpjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrrsResponseStatus :: Lens.Lens' ListProcessingJobsResponse Core.Int
lpjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
