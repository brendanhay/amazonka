{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trials in your account. Specify an experiment name to limit the list to the trials that are part of that experiment. Specify a trial component name to limit the list to the trials that associated with that trial component. The list can be filtered to show only trials that were created in a specific time range. The list can be sorted by trial name or creation time.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrials
  ( -- * Creating a request
    ListTrials (..),
    mkListTrials,

    -- ** Request lenses
    ltsCreatedAfter,
    ltsCreatedBefore,
    ltsExperimentName,
    ltsMaxResults,
    ltsNextToken,
    ltsSortBy,
    ltsSortOrder,
    ltsTrialComponentName,

    -- * Destructuring the response
    ListTrialsResponse (..),
    mkListTrialsResponse,

    -- ** Response lenses
    ltrrsNextToken,
    ltrrsTrialSummaries,
    ltrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListTrials' smart constructor.
data ListTrials = ListTrials'
  { -- | A filter that returns only trials created after the specified time.
    createdAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only trials created before the specified time.
    createdBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only trials that are part of the specified experiment.
    experimentName :: Core.Maybe Types.ExperimentEntityName,
    -- | The maximum number of trials to return in the response. The default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous call to @ListTrials@ didn't return the full set of trials, the call returns a token for getting the next set of trials.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The property used to sort results. The default value is @CreationTime@ .
    sortBy :: Core.Maybe Types.SortTrialsBy,
    -- | The sort order. The default value is @Descending@ .
    sortOrder :: Core.Maybe Types.SortOrder,
    -- | A filter that returns only trials that are associated with the specified trial component.
    trialComponentName :: Core.Maybe Types.ExperimentEntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTrials' value with any optional fields omitted.
mkListTrials ::
  ListTrials
mkListTrials =
  ListTrials'
    { createdAfter = Core.Nothing,
      createdBefore = Core.Nothing,
      experimentName = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing,
      trialComponentName = Core.Nothing
    }

-- | A filter that returns only trials created after the specified time.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsCreatedAfter :: Lens.Lens' ListTrials (Core.Maybe Core.NominalDiffTime)
ltsCreatedAfter = Lens.field @"createdAfter"
{-# DEPRECATED ltsCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | A filter that returns only trials created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsCreatedBefore :: Lens.Lens' ListTrials (Core.Maybe Core.NominalDiffTime)
ltsCreatedBefore = Lens.field @"createdBefore"
{-# DEPRECATED ltsCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

-- | A filter that returns only trials that are part of the specified experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsExperimentName :: Lens.Lens' ListTrials (Core.Maybe Types.ExperimentEntityName)
ltsExperimentName = Lens.field @"experimentName"
{-# DEPRECATED ltsExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The maximum number of trials to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsMaxResults :: Lens.Lens' ListTrials (Core.Maybe Core.Natural)
ltsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous call to @ListTrials@ didn't return the full set of trials, the call returns a token for getting the next set of trials.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsNextToken :: Lens.Lens' ListTrials (Core.Maybe Types.NextToken)
ltsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsSortBy :: Lens.Lens' ListTrials (Core.Maybe Types.SortTrialsBy)
ltsSortBy = Lens.field @"sortBy"
{-# DEPRECATED ltsSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsSortOrder :: Lens.Lens' ListTrials (Core.Maybe Types.SortOrder)
ltsSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED ltsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only trials that are associated with the specified trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsTrialComponentName :: Lens.Lens' ListTrials (Core.Maybe Types.ExperimentEntityName)
ltsTrialComponentName = Lens.field @"trialComponentName"
{-# DEPRECATED ltsTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

instance Core.FromJSON ListTrials where
  toJSON ListTrials {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("ExperimentName" Core..=) Core.<$> experimentName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("TrialComponentName" Core..=) Core.<$> trialComponentName
          ]
      )

instance Core.AWSRequest ListTrials where
  type Rs ListTrials = ListTrialsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListTrials")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrialsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TrialSummaries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTrials where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"trialSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTrialsResponse' smart constructor.
data ListTrialsResponse = ListTrialsResponse'
  { -- | A token for getting the next set of trials, if there are any.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of the summaries of your trials.
    trialSummaries :: Core.Maybe [Types.TrialSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTrialsResponse' value with any optional fields omitted.
mkListTrialsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTrialsResponse
mkListTrialsResponse responseStatus =
  ListTrialsResponse'
    { nextToken = Core.Nothing,
      trialSummaries = Core.Nothing,
      responseStatus
    }

-- | A token for getting the next set of trials, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTrialsResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the summaries of your trials.
--
-- /Note:/ Consider using 'trialSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTrialSummaries :: Lens.Lens' ListTrialsResponse (Core.Maybe [Types.TrialSummary])
ltrrsTrialSummaries = Lens.field @"trialSummaries"
{-# DEPRECATED ltrrsTrialSummaries "Use generic-lens or generic-optics with 'trialSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTrialsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
