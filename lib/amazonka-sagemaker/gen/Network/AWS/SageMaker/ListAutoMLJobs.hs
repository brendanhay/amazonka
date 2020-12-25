{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListAutoMLJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAutoMLJobs
  ( -- * Creating a request
    ListAutoMLJobs (..),
    mkListAutoMLJobs,

    -- ** Request lenses
    lamljCreationTimeAfter,
    lamljCreationTimeBefore,
    lamljLastModifiedTimeAfter,
    lamljLastModifiedTimeBefore,
    lamljMaxResults,
    lamljNameContains,
    lamljNextToken,
    lamljSortBy,
    lamljSortOrder,
    lamljStatusEquals,

    -- * Destructuring the response
    ListAutoMLJobsResponse (..),
    mkListAutoMLJobsResponse,

    -- ** Response lenses
    lamljrrsAutoMLJobSummaries,
    lamljrrsNextToken,
    lamljrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListAutoMLJobs' smart constructor.
data ListAutoMLJobs = ListAutoMLJobs'
  { -- | Request a list of jobs, using a filter for time.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | Request a list of jobs, using a filter for time.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | Request a list of jobs, using a filter for time.
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | Request a list of jobs, using a filter for time.
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | Request a list of jobs up to a specified limit.
    maxResults :: Core.Maybe Core.Natural,
    -- | Request a list of jobs, using a search filter for name.
    nameContains :: Core.Maybe Types.AutoMLNameContains,
    -- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The parameter by which to sort the results. The default is AutoMLJobName.
    sortBy :: Core.Maybe Types.AutoMLSortBy,
    -- | The sort order for the results. The default is Descending.
    sortOrder :: Core.Maybe Types.AutoMLSortOrder,
    -- | Request a list of jobs, using a filter for status.
    statusEquals :: Core.Maybe Types.AutoMLJobStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAutoMLJobs' value with any optional fields omitted.
mkListAutoMLJobs ::
  ListAutoMLJobs
mkListAutoMLJobs =
  ListAutoMLJobs'
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

-- | Request a list of jobs, using a filter for time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljCreationTimeAfter :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.NominalDiffTime)
lamljCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lamljCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | Request a list of jobs, using a filter for time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljCreationTimeBefore :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.NominalDiffTime)
lamljCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lamljCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | Request a list of jobs, using a filter for time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljLastModifiedTimeAfter :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.NominalDiffTime)
lamljLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED lamljLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | Request a list of jobs, using a filter for time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljLastModifiedTimeBefore :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.NominalDiffTime)
lamljLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED lamljLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | Request a list of jobs up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljMaxResults :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.Natural)
lamljMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lamljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Request a list of jobs, using a search filter for name.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljNameContains :: Lens.Lens' ListAutoMLJobs (Core.Maybe Types.AutoMLNameContains)
lamljNameContains = Lens.field @"nameContains"
{-# DEPRECATED lamljNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljNextToken :: Lens.Lens' ListAutoMLJobs (Core.Maybe Types.NextToken)
lamljNextToken = Lens.field @"nextToken"
{-# DEPRECATED lamljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parameter by which to sort the results. The default is AutoMLJobName.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljSortBy :: Lens.Lens' ListAutoMLJobs (Core.Maybe Types.AutoMLSortBy)
lamljSortBy = Lens.field @"sortBy"
{-# DEPRECATED lamljSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for the results. The default is Descending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljSortOrder :: Lens.Lens' ListAutoMLJobs (Core.Maybe Types.AutoMLSortOrder)
lamljSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lamljSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | Request a list of jobs, using a filter for status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljStatusEquals :: Lens.Lens' ListAutoMLJobs (Core.Maybe Types.AutoMLJobStatus)
lamljStatusEquals = Lens.field @"statusEquals"
{-# DEPRECATED lamljStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

instance Core.FromJSON ListAutoMLJobs where
  toJSON ListAutoMLJobs {..} =
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

instance Core.AWSRequest ListAutoMLJobs where
  type Rs ListAutoMLJobs = ListAutoMLJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListAutoMLJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAutoMLJobsResponse'
            Core.<$> (x Core..:? "AutoMLJobSummaries" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAutoMLJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"autoMLJobSummaries") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAutoMLJobsResponse' smart constructor.
data ListAutoMLJobsResponse = ListAutoMLJobsResponse'
  { -- | Returns a summary list of jobs.
    autoMLJobSummaries :: [Types.AutoMLJobSummary],
    -- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAutoMLJobsResponse' value with any optional fields omitted.
mkListAutoMLJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAutoMLJobsResponse
mkListAutoMLJobsResponse responseStatus =
  ListAutoMLJobsResponse'
    { autoMLJobSummaries = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Returns a summary list of jobs.
--
-- /Note:/ Consider using 'autoMLJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljrrsAutoMLJobSummaries :: Lens.Lens' ListAutoMLJobsResponse [Types.AutoMLJobSummary]
lamljrrsAutoMLJobSummaries = Lens.field @"autoMLJobSummaries"
{-# DEPRECATED lamljrrsAutoMLJobSummaries "Use generic-lens or generic-optics with 'autoMLJobSummaries' instead." #-}

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljrrsNextToken :: Lens.Lens' ListAutoMLJobsResponse (Core.Maybe Types.NextToken)
lamljrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lamljrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljrrsResponseStatus :: Lens.Lens' ListAutoMLJobsResponse Core.Int
lamljrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lamljrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
