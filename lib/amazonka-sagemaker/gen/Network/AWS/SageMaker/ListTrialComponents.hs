{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrialComponents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trial components in your account. You can sort the list by trial component name or creation time. You can filter the list to show only components that were created in a specific time range. You can also filter on one of the following:
--
--
--     * @ExperimentName@
--
--
--     * @SourceArn@
--
--
--     * @TrialName@
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrialComponents
  ( -- * Creating a request
    ListTrialComponents (..),
    mkListTrialComponents,

    -- ** Request lenses
    ltcCreatedAfter,
    ltcCreatedBefore,
    ltcExperimentName,
    ltcMaxResults,
    ltcNextToken,
    ltcSortBy,
    ltcSortOrder,
    ltcSourceArn,
    ltcTrialName,

    -- * Destructuring the response
    ListTrialComponentsResponse (..),
    mkListTrialComponentsResponse,

    -- ** Response lenses
    ltcrrsNextToken,
    ltcrrsTrialComponentSummaries,
    ltcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListTrialComponents' smart constructor.
data ListTrialComponents = ListTrialComponents'
  { -- | A filter that returns only components created after the specified time.
    createdAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only components created before the specified time.
    createdBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only components that are part of the specified experiment. If you specify @ExperimentName@ , you can't filter by @SourceArn@ or @TrialName@ .
    experimentName :: Core.Maybe Types.ExperimentEntityName,
    -- | The maximum number of components to return in the response. The default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous call to @ListTrialComponents@ didn't return the full set of components, the call returns a token for getting the next set of components.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The property used to sort results. The default value is @CreationTime@ .
    sortBy :: Core.Maybe Types.SortTrialComponentsBy,
    -- | The sort order. The default value is @Descending@ .
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A filter that returns only components that have the specified source Amazon Resource Name (ARN). If you specify @SourceArn@ , you can't filter by @ExperimentName@ or @TrialName@ .
    sourceArn :: Core.Maybe Types.String256,
    -- | A filter that returns only components that are part of the specified trial. If you specify @TrialName@ , you can't filter by @ExperimentName@ or @SourceArn@ .
    trialName :: Core.Maybe Types.ExperimentEntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTrialComponents' value with any optional fields omitted.
mkListTrialComponents ::
  ListTrialComponents
mkListTrialComponents =
  ListTrialComponents'
    { createdAfter = Core.Nothing,
      createdBefore = Core.Nothing,
      experimentName = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing,
      sourceArn = Core.Nothing,
      trialName = Core.Nothing
    }

-- | A filter that returns only components created after the specified time.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcCreatedAfter :: Lens.Lens' ListTrialComponents (Core.Maybe Core.NominalDiffTime)
ltcCreatedAfter = Lens.field @"createdAfter"
{-# DEPRECATED ltcCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | A filter that returns only components created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcCreatedBefore :: Lens.Lens' ListTrialComponents (Core.Maybe Core.NominalDiffTime)
ltcCreatedBefore = Lens.field @"createdBefore"
{-# DEPRECATED ltcCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

-- | A filter that returns only components that are part of the specified experiment. If you specify @ExperimentName@ , you can't filter by @SourceArn@ or @TrialName@ .
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcExperimentName :: Lens.Lens' ListTrialComponents (Core.Maybe Types.ExperimentEntityName)
ltcExperimentName = Lens.field @"experimentName"
{-# DEPRECATED ltcExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The maximum number of components to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcMaxResults :: Lens.Lens' ListTrialComponents (Core.Maybe Core.Natural)
ltcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous call to @ListTrialComponents@ didn't return the full set of components, the call returns a token for getting the next set of components.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcNextToken :: Lens.Lens' ListTrialComponents (Core.Maybe Types.NextToken)
ltcNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSortBy :: Lens.Lens' ListTrialComponents (Core.Maybe Types.SortTrialComponentsBy)
ltcSortBy = Lens.field @"sortBy"
{-# DEPRECATED ltcSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSortOrder :: Lens.Lens' ListTrialComponents (Core.Maybe Types.SortOrder)
ltcSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED ltcSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only components that have the specified source Amazon Resource Name (ARN). If you specify @SourceArn@ , you can't filter by @ExperimentName@ or @TrialName@ .
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSourceArn :: Lens.Lens' ListTrialComponents (Core.Maybe Types.String256)
ltcSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED ltcSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | A filter that returns only components that are part of the specified trial. If you specify @TrialName@ , you can't filter by @ExperimentName@ or @SourceArn@ .
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcTrialName :: Lens.Lens' ListTrialComponents (Core.Maybe Types.ExperimentEntityName)
ltcTrialName = Lens.field @"trialName"
{-# DEPRECATED ltcTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Core.FromJSON ListTrialComponents where
  toJSON ListTrialComponents {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("ExperimentName" Core..=) Core.<$> experimentName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("SourceArn" Core..=) Core.<$> sourceArn,
            ("TrialName" Core..=) Core.<$> trialName
          ]
      )

instance Core.AWSRequest ListTrialComponents where
  type Rs ListTrialComponents = ListTrialComponentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListTrialComponents")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrialComponentsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TrialComponentSummaries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTrialComponents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"trialComponentSummaries" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTrialComponentsResponse' smart constructor.
data ListTrialComponentsResponse = ListTrialComponentsResponse'
  { -- | A token for getting the next set of components, if there are any.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of the summaries of your trial components.
    trialComponentSummaries :: Core.Maybe [Types.TrialComponentSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTrialComponentsResponse' value with any optional fields omitted.
mkListTrialComponentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTrialComponentsResponse
mkListTrialComponentsResponse responseStatus =
  ListTrialComponentsResponse'
    { nextToken = Core.Nothing,
      trialComponentSummaries = Core.Nothing,
      responseStatus
    }

-- | A token for getting the next set of components, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrrsNextToken :: Lens.Lens' ListTrialComponentsResponse (Core.Maybe Types.NextToken)
ltcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the summaries of your trial components.
--
-- /Note:/ Consider using 'trialComponentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrrsTrialComponentSummaries :: Lens.Lens' ListTrialComponentsResponse (Core.Maybe [Types.TrialComponentSummary])
ltcrrsTrialComponentSummaries = Lens.field @"trialComponentSummaries"
{-# DEPRECATED ltcrrsTrialComponentSummaries "Use generic-lens or generic-optics with 'trialComponentSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrrsResponseStatus :: Lens.Lens' ListTrialComponentsResponse Core.Int
ltcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
