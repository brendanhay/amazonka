{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListHyperParameterTuningJobs (..),
    mkListHyperParameterTuningJobs,

    -- ** Request lenses
    lhptjCreationTimeAfter,
    lhptjCreationTimeBefore,
    lhptjLastModifiedTimeAfter,
    lhptjLastModifiedTimeBefore,
    lhptjMaxResults,
    lhptjNameContains,
    lhptjNextToken,
    lhptjSortBy,
    lhptjSortOrder,
    lhptjStatusEquals,

    -- * Destructuring the response
    ListHyperParameterTuningJobsResponse (..),
    mkListHyperParameterTuningJobsResponse,

    -- ** Response lenses
    lhptjrrsHyperParameterTuningJobSummaries,
    lhptjrrsNextToken,
    lhptjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListHyperParameterTuningJobs' smart constructor.
data ListHyperParameterTuningJobs = ListHyperParameterTuningJobs'
  { -- | A filter that returns only tuning jobs that were created after the specified time.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only tuning jobs that were created before the specified time.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only tuning jobs that were modified after the specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only tuning jobs that were modified before the specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of tuning jobs to return. The default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the tuning job name. This filter returns only tuning jobs whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the result of the previous @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The field to sort results by. The default is @Name@ .
    sortBy :: Core.Maybe Types.HyperParameterTuningJobSortByOptions,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A filter that returns only tuning jobs with the specified status.
    statusEquals :: Core.Maybe Types.HyperParameterTuningJobStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListHyperParameterTuningJobs' value with any optional fields omitted.
mkListHyperParameterTuningJobs ::
  ListHyperParameterTuningJobs
mkListHyperParameterTuningJobs =
  ListHyperParameterTuningJobs'
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

-- | A filter that returns only tuning jobs that were created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjCreationTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.NominalDiffTime)
lhptjCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lhptjCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only tuning jobs that were created before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjCreationTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.NominalDiffTime)
lhptjCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lhptjCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only tuning jobs that were modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjLastModifiedTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.NominalDiffTime)
lhptjLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED lhptjLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only tuning jobs that were modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjLastModifiedTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.NominalDiffTime)
lhptjLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED lhptjLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | The maximum number of tuning jobs to return. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjMaxResults :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.Natural)
lhptjMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lhptjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the tuning job name. This filter returns only tuning jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjNameContains :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.NameContains)
lhptjNameContains = Lens.field @"nameContains"
{-# DEPRECATED lhptjNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjNextToken :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.NextToken)
lhptjNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhptjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field to sort results by. The default is @Name@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjSortBy :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.HyperParameterTuningJobSortByOptions)
lhptjSortBy = Lens.field @"sortBy"
{-# DEPRECATED lhptjSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjSortOrder :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.SortOrder)
lhptjSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lhptjSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only tuning jobs with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjStatusEquals :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Types.HyperParameterTuningJobStatus)
lhptjStatusEquals = Lens.field @"statusEquals"
{-# DEPRECATED lhptjStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

instance Core.FromJSON ListHyperParameterTuningJobs where
  toJSON ListHyperParameterTuningJobs {..} =
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

instance Core.AWSRequest ListHyperParameterTuningJobs where
  type
    Rs ListHyperParameterTuningJobs =
      ListHyperParameterTuningJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SageMaker.ListHyperParameterTuningJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHyperParameterTuningJobsResponse'
            Core.<$> ( x Core..:? "HyperParameterTuningJobSummaries"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListHyperParameterTuningJobs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^. Lens.field @"hyperParameterTuningJobSummaries") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListHyperParameterTuningJobsResponse' smart constructor.
data ListHyperParameterTuningJobsResponse = ListHyperParameterTuningJobsResponse'
  { -- | A list of 'HyperParameterTuningJobSummary' objects that describe the tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
    hyperParameterTuningJobSummaries :: [Types.HyperParameterTuningJobSummary],
    -- | If the result of this @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListHyperParameterTuningJobsResponse' value with any optional fields omitted.
mkListHyperParameterTuningJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListHyperParameterTuningJobsResponse
mkListHyperParameterTuningJobsResponse responseStatus =
  ListHyperParameterTuningJobsResponse'
    { hyperParameterTuningJobSummaries =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of 'HyperParameterTuningJobSummary' objects that describe the tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
--
-- /Note:/ Consider using 'hyperParameterTuningJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrrsHyperParameterTuningJobSummaries :: Lens.Lens' ListHyperParameterTuningJobsResponse [Types.HyperParameterTuningJobSummary]
lhptjrrsHyperParameterTuningJobSummaries = Lens.field @"hyperParameterTuningJobSummaries"
{-# DEPRECATED lhptjrrsHyperParameterTuningJobSummaries "Use generic-lens or generic-optics with 'hyperParameterTuningJobSummaries' instead." #-}

-- | If the result of this @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrrsNextToken :: Lens.Lens' ListHyperParameterTuningJobsResponse (Core.Maybe Types.NextToken)
lhptjrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhptjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrrsResponseStatus :: Lens.Lens' ListHyperParameterTuningJobsResponse Core.Int
lhptjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lhptjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
