{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListExperiments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the experiments in your account. The list can be filtered to show only experiments that were created in a specific time range. The list can be sorted by experiment name or creation time.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListExperiments
  ( -- * Creating a request
    ListExperiments (..),
    mkListExperiments,

    -- ** Request lenses
    leCreatedAfter,
    leCreatedBefore,
    leMaxResults,
    leNextToken,
    leSortBy,
    leSortOrder,

    -- * Destructuring the response
    ListExperimentsResponse (..),
    mkListExperimentsResponse,

    -- ** Response lenses
    lerrsExperimentSummaries,
    lerrsNextToken,
    lerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListExperiments' smart constructor.
data ListExperiments = ListExperiments'
  { -- | A filter that returns only experiments created after the specified time.
    createdAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only experiments created before the specified time.
    createdBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of experiments to return in the response. The default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The property used to sort results. The default value is @CreationTime@ .
    sortBy :: Core.Maybe Types.SortExperimentsBy,
    -- | The sort order. The default value is @Descending@ .
    sortOrder :: Core.Maybe Types.SortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListExperiments' value with any optional fields omitted.
mkListExperiments ::
  ListExperiments
mkListExperiments =
  ListExperiments'
    { createdAfter = Core.Nothing,
      createdBefore = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A filter that returns only experiments created after the specified time.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreatedAfter :: Lens.Lens' ListExperiments (Core.Maybe Core.NominalDiffTime)
leCreatedAfter = Lens.field @"createdAfter"
{-# DEPRECATED leCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | A filter that returns only experiments created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreatedBefore :: Lens.Lens' ListExperiments (Core.Maybe Core.NominalDiffTime)
leCreatedBefore = Lens.field @"createdBefore"
{-# DEPRECATED leCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

-- | The maximum number of experiments to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListExperiments (Core.Maybe Core.Natural)
leMaxResults = Lens.field @"maxResults"
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExperiments (Core.Maybe Types.NextToken)
leNextToken = Lens.field @"nextToken"
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leSortBy :: Lens.Lens' ListExperiments (Core.Maybe Types.SortExperimentsBy)
leSortBy = Lens.field @"sortBy"
{-# DEPRECATED leSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leSortOrder :: Lens.Lens' ListExperiments (Core.Maybe Types.SortOrder)
leSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED leSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListExperiments where
  toJSON ListExperiments {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListExperiments where
  type Rs ListExperiments = ListExperimentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListExperiments")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExperimentsResponse'
            Core.<$> (x Core..:? "ExperimentSummaries")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListExperiments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"experimentSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListExperimentsResponse' smart constructor.
data ListExperimentsResponse = ListExperimentsResponse'
  { -- | A list of the summaries of your experiments.
    experimentSummaries :: Core.Maybe [Types.ExperimentSummary],
    -- | A token for getting the next set of experiments, if there are any.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListExperimentsResponse' value with any optional fields omitted.
mkListExperimentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListExperimentsResponse
mkListExperimentsResponse responseStatus =
  ListExperimentsResponse'
    { experimentSummaries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of the summaries of your experiments.
--
-- /Note:/ Consider using 'experimentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsExperimentSummaries :: Lens.Lens' ListExperimentsResponse (Core.Maybe [Types.ExperimentSummary])
lerrsExperimentSummaries = Lens.field @"experimentSummaries"
{-# DEPRECATED lerrsExperimentSummaries "Use generic-lens or generic-optics with 'experimentSummaries' instead." #-}

-- | A token for getting the next set of experiments, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListExperimentsResponse (Core.Maybe Types.NextToken)
lerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListExperimentsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
