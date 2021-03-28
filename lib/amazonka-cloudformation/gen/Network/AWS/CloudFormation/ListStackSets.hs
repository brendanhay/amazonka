{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack sets that are associated with the user.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackSets
    (
    -- * Creating a request
      ListStackSets (..)
    , mkListStackSets
    -- ** Request lenses
    , lssMaxResults
    , lssNextToken
    , lssStatus

    -- * Destructuring the response
    , ListStackSetsResponse (..)
    , mkListStackSetsResponse
    -- ** Response lenses
    , lssrrsNextToken
    , lssrrsSummaries
    , lssrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStackSets' smart constructor.
data ListStackSets = ListStackSets'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSets@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
  , status :: Core.Maybe Types.StackSetStatus
    -- ^ The status of the stack sets that you want to get summary information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStackSets' value with any optional fields omitted.
mkListStackSets
    :: ListStackSets
mkListStackSets
  = ListStackSets'{maxResults = Core.Nothing,
                   nextToken = Core.Nothing, status = Core.Nothing}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssMaxResults :: Lens.Lens' ListStackSets (Core.Maybe Core.Natural)
lssMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lssMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSets@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssNextToken :: Lens.Lens' ListStackSets (Core.Maybe Types.NextToken)
lssNextToken = Lens.field @"nextToken"
{-# INLINEABLE lssNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The status of the stack sets that you want to get summary information about.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssStatus :: Lens.Lens' ListStackSets (Core.Maybe Types.StackSetStatus)
lssStatus = Lens.field @"status"
{-# INLINEABLE lssStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery ListStackSets where
        toQuery ListStackSets{..}
          = Core.toQueryPair "Action" ("ListStackSets" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Status") status

instance Core.ToHeaders ListStackSets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListStackSets where
        type Rs ListStackSets = ListStackSetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListStackSetsResult"
              (\ s h x ->
                 ListStackSetsResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "Summaries" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListStackSets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"summaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListStackSetsResponse' smart constructor.
data ListStackSetsResponse = ListStackSetsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
  , summaries :: Core.Maybe [Types.StackSetSummary]
    -- ^ A list of @StackSetSummary@ structures that contain information about the user's stack sets.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListStackSetsResponse' value with any optional fields omitted.
mkListStackSetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStackSetsResponse
mkListStackSetsResponse responseStatus
  = ListStackSetsResponse'{nextToken = Core.Nothing,
                           summaries = Core.Nothing, responseStatus}

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrrsNextToken :: Lens.Lens' ListStackSetsResponse (Core.Maybe Types.NextToken)
lssrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lssrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of @StackSetSummary@ structures that contain information about the user's stack sets.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrrsSummaries :: Lens.Lens' ListStackSetsResponse (Core.Maybe [Types.StackSetSummary])
lssrrsSummaries = Lens.field @"summaries"
{-# INLINEABLE lssrrsSummaries #-}
{-# DEPRECATED summaries "Use generic-lens or generic-optics with 'summaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssrrsResponseStatus :: Lens.Lens' ListStackSetsResponse Core.Int
lssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
