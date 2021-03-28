{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListHumanTaskUis
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the human task user interfaces in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListHumanTaskUis
    (
    -- * Creating a request
      ListHumanTaskUis (..)
    , mkListHumanTaskUis
    -- ** Request lenses
    , lhtuCreationTimeAfter
    , lhtuCreationTimeBefore
    , lhtuMaxResults
    , lhtuNextToken
    , lhtuSortOrder

    -- * Destructuring the response
    , ListHumanTaskUisResponse (..)
    , mkListHumanTaskUisResponse
    -- ** Response lenses
    , lhturrsHumanTaskUiSummaries
    , lhturrsNextToken
    , lhturrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListHumanTaskUis' smart constructor.
data ListHumanTaskUis = ListHumanTaskUis'
  { creationTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only human task user interfaces with a creation time greater than or equal to the specified timestamp.
  , creationTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ A filter that returns only human task user interfaces that were created before the specified timestamp.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to resume pagination.
  , sortOrder :: Core.Maybe Types.SortOrder
    -- ^ An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListHumanTaskUis' value with any optional fields omitted.
mkListHumanTaskUis
    :: ListHumanTaskUis
mkListHumanTaskUis
  = ListHumanTaskUis'{creationTimeAfter = Core.Nothing,
                      creationTimeBefore = Core.Nothing, maxResults = Core.Nothing,
                      nextToken = Core.Nothing, sortOrder = Core.Nothing}

-- | A filter that returns only human task user interfaces with a creation time greater than or equal to the specified timestamp.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuCreationTimeAfter :: Lens.Lens' ListHumanTaskUis (Core.Maybe Core.NominalDiffTime)
lhtuCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# INLINEABLE lhtuCreationTimeAfter #-}
{-# DEPRECATED creationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead"  #-}

-- | A filter that returns only human task user interfaces that were created before the specified timestamp.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuCreationTimeBefore :: Lens.Lens' ListHumanTaskUis (Core.Maybe Core.NominalDiffTime)
lhtuCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# INLINEABLE lhtuCreationTimeBefore #-}
{-# DEPRECATED creationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead"  #-}

-- | The total number of items to return. If the total number of available items is more than the value specified in @MaxResults@ , then a @NextToken@ will be provided in the output that you can use to resume pagination.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuMaxResults :: Lens.Lens' ListHumanTaskUis (Core.Maybe Core.Natural)
lhtuMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lhtuMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuNextToken :: Lens.Lens' ListHumanTaskUis (Core.Maybe Types.NextToken)
lhtuNextToken = Lens.field @"nextToken"
{-# INLINEABLE lhtuNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An optional value that specifies whether you want the results sorted in @Ascending@ or @Descending@ order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhtuSortOrder :: Lens.Lens' ListHumanTaskUis (Core.Maybe Types.SortOrder)
lhtuSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lhtuSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListHumanTaskUis where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListHumanTaskUis where
        toHeaders ListHumanTaskUis{..}
          = Core.pure ("X-Amz-Target", "SageMaker.ListHumanTaskUis") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListHumanTaskUis where
        toJSON ListHumanTaskUis{..}
          = Core.object
              (Core.catMaybes
                 [("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
                  ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListHumanTaskUis where
        type Rs ListHumanTaskUis = ListHumanTaskUisResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListHumanTaskUisResponse' Core.<$>
                   (x Core..:? "HumanTaskUiSummaries" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListHumanTaskUis where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"humanTaskUiSummaries") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListHumanTaskUisResponse' smart constructor.
data ListHumanTaskUisResponse = ListHumanTaskUisResponse'
  { humanTaskUiSummaries :: [Types.HumanTaskUiSummary]
    -- ^ An array of objects describing the human task user interfaces.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to resume pagination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListHumanTaskUisResponse' value with any optional fields omitted.
mkListHumanTaskUisResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListHumanTaskUisResponse
mkListHumanTaskUisResponse responseStatus
  = ListHumanTaskUisResponse'{humanTaskUiSummaries = Core.mempty,
                              nextToken = Core.Nothing, responseStatus}

-- | An array of objects describing the human task user interfaces.
--
-- /Note:/ Consider using 'humanTaskUiSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhturrsHumanTaskUiSummaries :: Lens.Lens' ListHumanTaskUisResponse [Types.HumanTaskUiSummary]
lhturrsHumanTaskUiSummaries = Lens.field @"humanTaskUiSummaries"
{-# INLINEABLE lhturrsHumanTaskUiSummaries #-}
{-# DEPRECATED humanTaskUiSummaries "Use generic-lens or generic-optics with 'humanTaskUiSummaries' instead"  #-}

-- | A token to resume pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhturrsNextToken :: Lens.Lens' ListHumanTaskUisResponse (Core.Maybe Types.NextToken)
lhturrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lhturrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhturrsResponseStatus :: Lens.Lens' ListHumanTaskUisResponse Core.Int
lhturrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lhturrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
