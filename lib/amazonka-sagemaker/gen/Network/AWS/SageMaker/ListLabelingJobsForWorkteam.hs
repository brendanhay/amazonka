{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListLabelingJobsForWorkteam (..),
    mkListLabelingJobsForWorkteam,

    -- ** Request lenses
    lljfwWorkteamArn,
    lljfwCreationTimeAfter,
    lljfwCreationTimeBefore,
    lljfwJobReferenceCodeContains,
    lljfwMaxResults,
    lljfwNextToken,
    lljfwSortBy,
    lljfwSortOrder,

    -- * Destructuring the response
    ListLabelingJobsForWorkteamResponse (..),
    mkListLabelingJobsForWorkteamResponse,

    -- ** Response lenses
    lljfwrrsLabelingJobSummaryList,
    lljfwrrsNextToken,
    lljfwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListLabelingJobsForWorkteam' smart constructor.
data ListLabelingJobsForWorkteam = ListLabelingJobsForWorkteam'
  { -- | The Amazon Resource Name (ARN) of the work team for which you want to see labeling jobs for.
    workteamArn :: Types.WorkteamArn,
    -- | A filter that returns only labeling jobs created after the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only labeling jobs created before the specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter the limits jobs to only the ones whose job reference code contains the specified string.
    jobReferenceCodeContains :: Core.Maybe Types.JobReferenceCodeContains,
    -- | The maximum number of labeling jobs to return in each page of the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the result of the previous @ListLabelingJobsForWorkteam@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.ListLabelingJobsForWorkteamSortByOptions,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.SortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListLabelingJobsForWorkteam' value with any optional fields omitted.
mkListLabelingJobsForWorkteam ::
  -- | 'workteamArn'
  Types.WorkteamArn ->
  ListLabelingJobsForWorkteam
mkListLabelingJobsForWorkteam workteamArn =
  ListLabelingJobsForWorkteam'
    { workteamArn,
      creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      jobReferenceCodeContains = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the work team for which you want to see labeling jobs for.
--
-- /Note:/ Consider using 'workteamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwWorkteamArn :: Lens.Lens' ListLabelingJobsForWorkteam Types.WorkteamArn
lljfwWorkteamArn = Lens.field @"workteamArn"
{-# DEPRECATED lljfwWorkteamArn "Use generic-lens or generic-optics with 'workteamArn' instead." #-}

-- | A filter that returns only labeling jobs created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwCreationTimeAfter :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Core.NominalDiffTime)
lljfwCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lljfwCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only labeling jobs created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwCreationTimeBefore :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Core.NominalDiffTime)
lljfwCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lljfwCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter the limits jobs to only the ones whose job reference code contains the specified string.
--
-- /Note:/ Consider using 'jobReferenceCodeContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwJobReferenceCodeContains :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Types.JobReferenceCodeContains)
lljfwJobReferenceCodeContains = Lens.field @"jobReferenceCodeContains"
{-# DEPRECATED lljfwJobReferenceCodeContains "Use generic-lens or generic-optics with 'jobReferenceCodeContains' instead." #-}

-- | The maximum number of labeling jobs to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwMaxResults :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Core.Natural)
lljfwMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lljfwMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the result of the previous @ListLabelingJobsForWorkteam@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwNextToken :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Types.NextToken)
lljfwNextToken = Lens.field @"nextToken"
{-# DEPRECATED lljfwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwSortBy :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Types.ListLabelingJobsForWorkteamSortByOptions)
lljfwSortBy = Lens.field @"sortBy"
{-# DEPRECATED lljfwSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwSortOrder :: Lens.Lens' ListLabelingJobsForWorkteam (Core.Maybe Types.SortOrder)
lljfwSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lljfwSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListLabelingJobsForWorkteam where
  toJSON ListLabelingJobsForWorkteam {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkteamArn" Core..= workteamArn),
            ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("JobReferenceCodeContains" Core..=)
              Core.<$> jobReferenceCodeContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListLabelingJobsForWorkteam where
  type
    Rs ListLabelingJobsForWorkteam =
      ListLabelingJobsForWorkteamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListLabelingJobsForWorkteam")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLabelingJobsForWorkteamResponse'
            Core.<$> (x Core..:? "LabelingJobSummaryList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLabelingJobsForWorkteam where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"labelingJobSummaryList") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListLabelingJobsForWorkteamResponse' smart constructor.
data ListLabelingJobsForWorkteamResponse = ListLabelingJobsForWorkteamResponse'
  { -- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
    labelingJobSummaryList :: [Types.LabelingJobForWorkteamSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListLabelingJobsForWorkteamResponse' value with any optional fields omitted.
mkListLabelingJobsForWorkteamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLabelingJobsForWorkteamResponse
mkListLabelingJobsForWorkteamResponse responseStatus =
  ListLabelingJobsForWorkteamResponse'
    { labelingJobSummaryList =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
--
-- /Note:/ Consider using 'labelingJobSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrrsLabelingJobSummaryList :: Lens.Lens' ListLabelingJobsForWorkteamResponse [Types.LabelingJobForWorkteamSummary]
lljfwrrsLabelingJobSummaryList = Lens.field @"labelingJobSummaryList"
{-# DEPRECATED lljfwrrsLabelingJobSummaryList "Use generic-lens or generic-optics with 'labelingJobSummaryList' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrrsNextToken :: Lens.Lens' ListLabelingJobsForWorkteamResponse (Core.Maybe Types.NextToken)
lljfwrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lljfwrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrrsResponseStatus :: Lens.Lens' ListLabelingJobsForWorkteamResponse Core.Int
lljfwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lljfwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
