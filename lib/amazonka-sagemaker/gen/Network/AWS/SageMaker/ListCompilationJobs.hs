{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListCompilationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists model compilation jobs that satisfy various filters.
--
-- To create a model compilation job, use 'CreateCompilationJob' . To get information about a particular model compilation job you have created, use 'DescribeCompilationJob' .
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCompilationJobs
  ( -- * Creating a request
    ListCompilationJobs (..),
    mkListCompilationJobs,

    -- ** Request lenses
    lcjCreationTimeAfter,
    lcjCreationTimeBefore,
    lcjLastModifiedTimeAfter,
    lcjLastModifiedTimeBefore,
    lcjMaxResults,
    lcjNameContains,
    lcjNextToken,
    lcjSortBy,
    lcjSortOrder,
    lcjStatusEquals,

    -- * Destructuring the response
    ListCompilationJobsResponse (..),
    mkListCompilationJobsResponse,

    -- ** Response lenses
    lcjrrsCompilationJobSummaries,
    lcjrrsNextToken,
    lcjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListCompilationJobs' smart constructor.
data ListCompilationJobs = ListCompilationJobs'
  { -- | A filter that returns the model compilation jobs that were created after a specified time.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns the model compilation jobs that were created before a specified time.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns the model compilation jobs that were modified after a specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns the model compilation jobs that were modified before a specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of model compilation jobs to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns the model compilation jobs whose name contains a specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the result of the previous @ListCompilationJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model compilation jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The field by which to sort results. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.ListCompilationJobsSortBy,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A filter that retrieves model compilation jobs with a specific 'DescribeCompilationJobResponse$CompilationJobStatus' status.
    statusEquals :: Core.Maybe Types.CompilationJobStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCompilationJobs' value with any optional fields omitted.
mkListCompilationJobs ::
  ListCompilationJobs
mkListCompilationJobs =
  ListCompilationJobs'
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

-- | A filter that returns the model compilation jobs that were created after a specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjCreationTimeAfter :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.NominalDiffTime)
lcjCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lcjCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns the model compilation jobs that were created before a specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjCreationTimeBefore :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.NominalDiffTime)
lcjCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lcjCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns the model compilation jobs that were modified after a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjLastModifiedTimeAfter :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.NominalDiffTime)
lcjLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED lcjLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns the model compilation jobs that were modified before a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjLastModifiedTimeBefore :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.NominalDiffTime)
lcjLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED lcjLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | The maximum number of model compilation jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjMaxResults :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.Natural)
lcjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter that returns the model compilation jobs whose name contains a specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjNameContains :: Lens.Lens' ListCompilationJobs (Core.Maybe Types.NameContains)
lcjNameContains = Lens.field @"nameContains"
{-# DEPRECATED lcjNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListCompilationJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model compilation jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjNextToken :: Lens.Lens' ListCompilationJobs (Core.Maybe Types.NextToken)
lcjNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field by which to sort results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjSortBy :: Lens.Lens' ListCompilationJobs (Core.Maybe Types.ListCompilationJobsSortBy)
lcjSortBy = Lens.field @"sortBy"
{-# DEPRECATED lcjSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjSortOrder :: Lens.Lens' ListCompilationJobs (Core.Maybe Types.SortOrder)
lcjSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lcjSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that retrieves model compilation jobs with a specific 'DescribeCompilationJobResponse$CompilationJobStatus' status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjStatusEquals :: Lens.Lens' ListCompilationJobs (Core.Maybe Types.CompilationJobStatus)
lcjStatusEquals = Lens.field @"statusEquals"
{-# DEPRECATED lcjStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

instance Core.FromJSON ListCompilationJobs where
  toJSON ListCompilationJobs {..} =
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

instance Core.AWSRequest ListCompilationJobs where
  type Rs ListCompilationJobs = ListCompilationJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListCompilationJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCompilationJobsResponse'
            Core.<$> (x Core..:? "CompilationJobSummaries" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCompilationJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"compilationJobSummaries") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListCompilationJobsResponse' smart constructor.
data ListCompilationJobsResponse = ListCompilationJobsResponse'
  { -- | An array of 'CompilationJobSummary' objects, each describing a model compilation job.
    compilationJobSummaries :: [Types.CompilationJobSummary],
    -- | If the response is truncated, Amazon SageMaker returns this @NextToken@ . To retrieve the next set of model compilation jobs, use this token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCompilationJobsResponse' value with any optional fields omitted.
mkListCompilationJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCompilationJobsResponse
mkListCompilationJobsResponse responseStatus =
  ListCompilationJobsResponse'
    { compilationJobSummaries =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of 'CompilationJobSummary' objects, each describing a model compilation job.
--
-- /Note:/ Consider using 'compilationJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsCompilationJobSummaries :: Lens.Lens' ListCompilationJobsResponse [Types.CompilationJobSummary]
lcjrrsCompilationJobSummaries = Lens.field @"compilationJobSummaries"
{-# DEPRECATED lcjrrsCompilationJobSummaries "Use generic-lens or generic-optics with 'compilationJobSummaries' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this @NextToken@ . To retrieve the next set of model compilation jobs, use this token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsNextToken :: Lens.Lens' ListCompilationJobsResponse (Core.Maybe Types.NextToken)
lcjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrrsResponseStatus :: Lens.Lens' ListCompilationJobsResponse Core.Int
lcjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
