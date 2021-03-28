{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListResourceComplianceSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a resource-level summary count. The summary includes information about compliant and non-compliant statuses and detailed compliance-item severity counts, according to the filter criteria you specify.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListResourceComplianceSummaries
    (
    -- * Creating a request
      ListResourceComplianceSummaries (..)
    , mkListResourceComplianceSummaries
    -- ** Request lenses
    , lrcsFilters
    , lrcsMaxResults
    , lrcsNextToken

    -- * Destructuring the response
    , ListResourceComplianceSummariesResponse (..)
    , mkListResourceComplianceSummariesResponse
    -- ** Response lenses
    , lrcsrrsNextToken
    , lrcsrrsResourceComplianceSummaryItems
    , lrcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListResourceComplianceSummaries' smart constructor.
data ListResourceComplianceSummaries = ListResourceComplianceSummaries'
  { filters :: Core.Maybe [Types.ComplianceStringFilter]
    -- ^ One or more filters. Use a filter to return a more specific list of results.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceComplianceSummaries' value with any optional fields omitted.
mkListResourceComplianceSummaries
    :: ListResourceComplianceSummaries
mkListResourceComplianceSummaries
  = ListResourceComplianceSummaries'{filters = Core.Nothing,
                                     maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsFilters :: Lens.Lens' ListResourceComplianceSummaries (Core.Maybe [Types.ComplianceStringFilter])
lrcsFilters = Lens.field @"filters"
{-# INLINEABLE lrcsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsMaxResults :: Lens.Lens' ListResourceComplianceSummaries (Core.Maybe Core.Natural)
lrcsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrcsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsNextToken :: Lens.Lens' ListResourceComplianceSummaries (Core.Maybe Types.NextToken)
lrcsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrcsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListResourceComplianceSummaries where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResourceComplianceSummaries where
        toHeaders ListResourceComplianceSummaries{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.ListResourceComplianceSummaries")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResourceComplianceSummaries where
        toJSON ListResourceComplianceSummaries{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListResourceComplianceSummaries where
        type Rs ListResourceComplianceSummaries =
             ListResourceComplianceSummariesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourceComplianceSummariesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "ResourceComplianceSummaryItems"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListResourceComplianceSummaries where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"resourceComplianceSummaryItems" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListResourceComplianceSummariesResponse' smart constructor.
data ListResourceComplianceSummariesResponse = ListResourceComplianceSummariesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. Use this token to get the next set of results.
  , resourceComplianceSummaryItems :: Core.Maybe [Types.ResourceComplianceSummaryItem]
    -- ^ A summary count for specified or targeted managed instances. Summary count includes information about compliant and non-compliant State Manager associations, patch status, or custom items according to the filter criteria that you specify. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListResourceComplianceSummariesResponse' value with any optional fields omitted.
mkListResourceComplianceSummariesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourceComplianceSummariesResponse
mkListResourceComplianceSummariesResponse responseStatus
  = ListResourceComplianceSummariesResponse'{nextToken =
                                               Core.Nothing,
                                             resourceComplianceSummaryItems = Core.Nothing,
                                             responseStatus}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsrrsNextToken :: Lens.Lens' ListResourceComplianceSummariesResponse (Core.Maybe Types.NextToken)
lrcsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrcsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A summary count for specified or targeted managed instances. Summary count includes information about compliant and non-compliant State Manager associations, patch status, or custom items according to the filter criteria that you specify. 
--
-- /Note:/ Consider using 'resourceComplianceSummaryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsrrsResourceComplianceSummaryItems :: Lens.Lens' ListResourceComplianceSummariesResponse (Core.Maybe [Types.ResourceComplianceSummaryItem])
lrcsrrsResourceComplianceSummaryItems = Lens.field @"resourceComplianceSummaryItems"
{-# INLINEABLE lrcsrrsResourceComplianceSummaryItems #-}
{-# DEPRECATED resourceComplianceSummaryItems "Use generic-lens or generic-optics with 'resourceComplianceSummaryItems' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrcsrrsResponseStatus :: Lens.Lens' ListResourceComplianceSummariesResponse Core.Int
lrcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
