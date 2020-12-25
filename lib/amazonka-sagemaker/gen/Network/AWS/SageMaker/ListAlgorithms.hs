{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListAlgorithms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the machine learning algorithms that have been created.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAlgorithms
  ( -- * Creating a request
    ListAlgorithms (..),
    mkListAlgorithms,

    -- ** Request lenses
    lCreationTimeAfter,
    lCreationTimeBefore,
    lMaxResults,
    lNameContains,
    lNextToken,
    lSortBy,
    lSortOrder,

    -- * Destructuring the response
    ListAlgorithmsResponse (..),
    mkListAlgorithmsResponse,

    -- ** Response lenses
    larfrsAlgorithmSummaryList,
    larfrsNextToken,
    larfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListAlgorithms' smart constructor.
data ListAlgorithms = ListAlgorithms'
  { -- | A filter that returns only algorithms created after the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only algorithms created before the specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of algorithms to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The parameter by which to sort the results. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.AlgorithmSortBy,
    -- | The sort order for the results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.SortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAlgorithms' value with any optional fields omitted.
mkListAlgorithms ::
  ListAlgorithms
mkListAlgorithms =
  ListAlgorithms'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A filter that returns only algorithms created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCreationTimeAfter :: Lens.Lens' ListAlgorithms (Core.Maybe Core.NominalDiffTime)
lCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only algorithms created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCreationTimeBefore :: Lens.Lens' ListAlgorithms (Core.Maybe Core.NominalDiffTime)
lCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of algorithms to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListAlgorithms (Core.Maybe Core.Natural)
lMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNameContains :: Lens.Lens' ListAlgorithms (Core.Maybe Types.NameContains)
lNameContains = Lens.field @"nameContains"
{-# DEPRECATED lNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListAlgorithms (Core.Maybe Types.NextToken)
lNextToken = Lens.field @"nextToken"
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The parameter by which to sort the results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSortBy :: Lens.Lens' ListAlgorithms (Core.Maybe Types.AlgorithmSortBy)
lSortBy = Lens.field @"sortBy"
{-# DEPRECATED lSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for the results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSortOrder :: Lens.Lens' ListAlgorithms (Core.Maybe Types.SortOrder)
lSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListAlgorithms where
  toJSON ListAlgorithms {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListAlgorithms where
  type Rs ListAlgorithms = ListAlgorithmsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListAlgorithms")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlgorithmsResponse'
            Core.<$> (x Core..:? "AlgorithmSummaryList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAlgorithms where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"algorithmSummaryList") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAlgorithmsResponse' smart constructor.
data ListAlgorithmsResponse = ListAlgorithmsResponse'
  { -- | >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
    algorithmSummaryList :: [Types.AlgorithmSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAlgorithmsResponse' value with any optional fields omitted.
mkListAlgorithmsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAlgorithmsResponse
mkListAlgorithmsResponse responseStatus =
  ListAlgorithmsResponse'
    { algorithmSummaryList = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
--
-- /Note:/ Consider using 'algorithmSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larfrsAlgorithmSummaryList :: Lens.Lens' ListAlgorithmsResponse [Types.AlgorithmSummary]
larfrsAlgorithmSummaryList = Lens.field @"algorithmSummaryList"
{-# DEPRECATED larfrsAlgorithmSummaryList "Use generic-lens or generic-optics with 'algorithmSummaryList' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larfrsNextToken :: Lens.Lens' ListAlgorithmsResponse (Core.Maybe Types.NextToken)
larfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED larfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larfrsResponseStatus :: Lens.Lens' ListAlgorithmsResponse Core.Int
larfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED larfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
