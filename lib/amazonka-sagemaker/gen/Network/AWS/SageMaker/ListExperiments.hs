{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListExperiments (..)
    , mkListExperiments
    -- ** Request lenses
    , leCreatedAfter
    , leCreatedBefore
    , leMaxResults
    , leNextToken
    , leSortBy
    , leSortOrder

    -- * Destructuring the response
    , ListExperimentsResponse (..)
    , mkListExperimentsResponse
    -- ** Response lenses
    , lerrsExperimentSummaries
    , lerrsNextToken
    , lerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListExperiments' smart constructor.
data ListExperiments = ListExperiments'
  { createdAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only experiments created after the specified time.
  , createdBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only experiments created before the specified time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of experiments to return in the response. The default value is 10.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
  , sortBy :: Core.Maybe Types.SortExperimentsBy
    -- ^ The property used to sort results. The default value is @CreationTime@ .
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ The sort order. The default value is @Descending@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListExperiments' value with any optional fields omitted.
mkListExperiments
    :: ListExperiments
mkListExperiments
  = ListExperiments'{createdAfter = Core.Nothing,
                     createdBefore = Core.Nothing, maxResults = Core.Nothing,
                     nextToken = Core.Nothing, sortBy = Core.Nothing,
                     sortOrder = Core.Nothing}

-- | A filter that returns only experiments created after the specified time.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreatedAfter :: Lens.Lens' ListExperiments (Core.Maybe Core.NominalDiffTime)
leCreatedAfter = Lens.field @"createdAfter"
{-# INLINEABLE leCreatedAfter #-}
{-# DEPRECATED createdAfter "Use generic-lens or generic-optics with 'createdAfter' instead"  #-}

-- | A filter that returns only experiments created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreatedBefore :: Lens.Lens' ListExperiments (Core.Maybe Core.NominalDiffTime)
leCreatedBefore = Lens.field @"createdBefore"
{-# INLINEABLE leCreatedBefore #-}
{-# DEPRECATED createdBefore "Use generic-lens or generic-optics with 'createdBefore' instead"  #-}

-- | The maximum number of experiments to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListExperiments (Core.Maybe Core.Natural)
leMaxResults = Lens.field @"maxResults"
{-# INLINEABLE leMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExperiments (Core.Maybe Types.NextToken)
leNextToken = Lens.field @"nextToken"
{-# INLINEABLE leNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leSortBy :: Lens.Lens' ListExperiments (Core.Maybe Types.SortExperimentsBy)
leSortBy = Lens.field @"sortBy"
{-# INLINEABLE leSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leSortOrder :: Lens.Lens' ListExperiments (Core.Maybe Types.SortOrder)
leSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE leSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListExperiments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListExperiments where
        toHeaders ListExperiments{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListExperiments") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListExperiments where
        toJSON ListExperiments{..}
          = Core.object
              (Core.catMaybes
                 [("CreatedAfter" Core..=) Core.<$> createdAfter,
                  ("CreatedBefore" Core..=) Core.<$> createdBefore,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortBy" Core..=) Core.<$> sortBy,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListExperiments where
        type Rs ListExperiments = ListExperimentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListExperimentsResponse' Core.<$>
                   (x Core..:? "ExperimentSummaries") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListExperiments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"experimentSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListExperimentsResponse' smart constructor.
data ListExperimentsResponse = ListExperimentsResponse'
  { experimentSummaries :: Core.Maybe [Types.ExperimentSummary]
    -- ^ A list of the summaries of your experiments.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token for getting the next set of experiments, if there are any.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListExperimentsResponse' value with any optional fields omitted.
mkListExperimentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListExperimentsResponse
mkListExperimentsResponse responseStatus
  = ListExperimentsResponse'{experimentSummaries = Core.Nothing,
                             nextToken = Core.Nothing, responseStatus}

-- | A list of the summaries of your experiments.
--
-- /Note:/ Consider using 'experimentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsExperimentSummaries :: Lens.Lens' ListExperimentsResponse (Core.Maybe [Types.ExperimentSummary])
lerrsExperimentSummaries = Lens.field @"experimentSummaries"
{-# INLINEABLE lerrsExperimentSummaries #-}
{-# DEPRECATED experimentSummaries "Use generic-lens or generic-optics with 'experimentSummaries' instead"  #-}

-- | A token for getting the next set of experiments, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsNextToken :: Lens.Lens' ListExperimentsResponse (Core.Maybe Types.NextToken)
lerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrsResponseStatus :: Lens.Lens' ListExperimentsResponse Core.Int
lerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
