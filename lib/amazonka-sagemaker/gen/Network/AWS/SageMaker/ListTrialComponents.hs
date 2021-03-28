{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListTrialComponents (..)
    , mkListTrialComponents
    -- ** Request lenses
    , ltcCreatedAfter
    , ltcCreatedBefore
    , ltcExperimentName
    , ltcMaxResults
    , ltcNextToken
    , ltcSortBy
    , ltcSortOrder
    , ltcSourceArn
    , ltcTrialName

    -- * Destructuring the response
    , ListTrialComponentsResponse (..)
    , mkListTrialComponentsResponse
    -- ** Response lenses
    , ltcrrsNextToken
    , ltcrrsTrialComponentSummaries
    , ltcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListTrialComponents' smart constructor.
data ListTrialComponents = ListTrialComponents'
  { createdAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only components created after the specified time.
  , createdBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only components created before the specified time.
  , experimentName :: Core.Maybe Types.ExperimentEntityName
    -- ^ A filter that returns only components that are part of the specified experiment. If you specify @ExperimentName@ , you can't filter by @SourceArn@ or @TrialName@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of components to return in the response. The default value is 10.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous call to @ListTrialComponents@ didn't return the full set of components, the call returns a token for getting the next set of components.
  , sortBy :: Core.Maybe Types.SortTrialComponentsBy
    -- ^ The property used to sort results. The default value is @CreationTime@ .
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order. The default value is @Descending@ .
  , sourceArn :: Core.Maybe Types.String256
    -- ^ A filter that returns only components that have the specified source Amazon Resource Name (ARN). If you specify @SourceArn@ , you can't filter by @ExperimentName@ or @TrialName@ .
  , trialName :: Core.Maybe Types.ExperimentEntityName
    -- ^ A filter that returns only components that are part of the specified trial. If you specify @TrialName@ , you can't filter by @ExperimentName@ or @SourceArn@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTrialComponents' value with any optional fields omitted.
mkListTrialComponents
    :: ListTrialComponents
mkListTrialComponents
  = ListTrialComponents'{createdAfter = Core.Nothing,
                         createdBefore = Core.Nothing, experimentName = Core.Nothing,
                         maxResults = Core.Nothing, nextToken = Core.Nothing,
                         sortBy = Core.Nothing, sortOrder = Core.Nothing,
                         sourceArn = Core.Nothing, trialName = Core.Nothing}

-- | A filter that returns only components created after the specified time.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcCreatedAfter :: Lens.Lens' ListTrialComponents (Core.Maybe Core.NominalDiffTime)
ltcCreatedAfter = Lens.field @"createdAfter"
{-# INLINEABLE ltcCreatedAfter #-}
{-# DEPRECATED createdAfter "Use generic-lens or generic-optics with 'createdAfter' instead"  #-}

-- | A filter that returns only components created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcCreatedBefore :: Lens.Lens' ListTrialComponents (Core.Maybe Core.NominalDiffTime)
ltcCreatedBefore = Lens.field @"createdBefore"
{-# INLINEABLE ltcCreatedBefore #-}
{-# DEPRECATED createdBefore "Use generic-lens or generic-optics with 'createdBefore' instead"  #-}

-- | A filter that returns only components that are part of the specified experiment. If you specify @ExperimentName@ , you can't filter by @SourceArn@ or @TrialName@ .
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcExperimentName :: Lens.Lens' ListTrialComponents (Core.Maybe Types.ExperimentEntityName)
ltcExperimentName = Lens.field @"experimentName"
{-# INLINEABLE ltcExperimentName #-}
{-# DEPRECATED experimentName "Use generic-lens or generic-optics with 'experimentName' instead"  #-}

-- | The maximum number of components to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcMaxResults :: Lens.Lens' ListTrialComponents (Core.Maybe Core.Natural)
ltcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous call to @ListTrialComponents@ didn't return the full set of components, the call returns a token for getting the next set of components.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcNextToken :: Lens.Lens' ListTrialComponents (Core.Maybe Types.NextToken)
ltcNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSortBy :: Lens.Lens' ListTrialComponents (Core.Maybe Types.SortTrialComponentsBy)
ltcSortBy = Lens.field @"sortBy"
{-# INLINEABLE ltcSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSortOrder :: Lens.Lens' ListTrialComponents (Core.Maybe Types.SortOrder)
ltcSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE ltcSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

-- | A filter that returns only components that have the specified source Amazon Resource Name (ARN). If you specify @SourceArn@ , you can't filter by @ExperimentName@ or @TrialName@ .
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSourceArn :: Lens.Lens' ListTrialComponents (Core.Maybe Types.String256)
ltcSourceArn = Lens.field @"sourceArn"
{-# INLINEABLE ltcSourceArn #-}
{-# DEPRECATED sourceArn "Use generic-lens or generic-optics with 'sourceArn' instead"  #-}

-- | A filter that returns only components that are part of the specified trial. If you specify @TrialName@ , you can't filter by @ExperimentName@ or @SourceArn@ .
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcTrialName :: Lens.Lens' ListTrialComponents (Core.Maybe Types.ExperimentEntityName)
ltcTrialName = Lens.field @"trialName"
{-# INLINEABLE ltcTrialName #-}
{-# DEPRECATED trialName "Use generic-lens or generic-optics with 'trialName' instead"  #-}

instance Core.ToQuery ListTrialComponents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTrialComponents where
        toHeaders ListTrialComponents{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListTrialComponents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTrialComponents where
        toJSON ListTrialComponents{..}
          = Core.object
              (Core.catMaybes
                 [("CreatedAfter" Core..=) Core.<$> createdAfter,
                  ("CreatedBefore" Core..=) Core.<$> createdBefore,
                  ("ExperimentName" Core..=) Core.<$> experimentName,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder,
                  ("SourceArn" Core..=) Core.<$> sourceArn,
                  ("TrialName" Core..=) Core.<$> trialName])

instance Core.AWSRequest ListTrialComponents where
        type Rs ListTrialComponents = ListTrialComponentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTrialComponentsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "TrialComponentSummaries"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTrialComponents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"trialComponentSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTrialComponentsResponse' smart constructor.
data ListTrialComponentsResponse = ListTrialComponentsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token for getting the next set of components, if there are any.
  , trialComponentSummaries :: Core.Maybe [Types.TrialComponentSummary]
    -- ^ A list of the summaries of your trial components.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTrialComponentsResponse' value with any optional fields omitted.
mkListTrialComponentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTrialComponentsResponse
mkListTrialComponentsResponse responseStatus
  = ListTrialComponentsResponse'{nextToken = Core.Nothing,
                                 trialComponentSummaries = Core.Nothing, responseStatus}

-- | A token for getting the next set of components, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrrsNextToken :: Lens.Lens' ListTrialComponentsResponse (Core.Maybe Types.NextToken)
ltcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of the summaries of your trial components.
--
-- /Note:/ Consider using 'trialComponentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrrsTrialComponentSummaries :: Lens.Lens' ListTrialComponentsResponse (Core.Maybe [Types.TrialComponentSummary])
ltcrrsTrialComponentSummaries = Lens.field @"trialComponentSummaries"
{-# INLINEABLE ltcrrsTrialComponentSummaries #-}
{-# DEPRECATED trialComponentSummaries "Use generic-lens or generic-optics with 'trialComponentSummaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrrsResponseStatus :: Lens.Lens' ListTrialComponentsResponse Core.Int
ltcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
