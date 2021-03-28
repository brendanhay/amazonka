{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingsInBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things you have added to the given billing group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingsInBillingGroup
    (
    -- * Creating a request
      ListThingsInBillingGroup (..)
    , mkListThingsInBillingGroup
    -- ** Request lenses
    , ltibgBillingGroupName
    , ltibgMaxResults
    , ltibgNextToken

    -- * Destructuring the response
    , ListThingsInBillingGroupResponse (..)
    , mkListThingsInBillingGroupResponse
    -- ** Response lenses
    , ltibgrrsNextToken
    , ltibgrrsThings
    , ltibgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingsInBillingGroup' smart constructor.
data ListThingsInBillingGroup = ListThingsInBillingGroup'
  { billingGroupName :: Types.BillingGroupName
    -- ^ The name of the billing group.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return per request.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingsInBillingGroup' value with any optional fields omitted.
mkListThingsInBillingGroup
    :: Types.BillingGroupName -- ^ 'billingGroupName'
    -> ListThingsInBillingGroup
mkListThingsInBillingGroup billingGroupName
  = ListThingsInBillingGroup'{billingGroupName,
                              maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgBillingGroupName :: Lens.Lens' ListThingsInBillingGroup Types.BillingGroupName
ltibgBillingGroupName = Lens.field @"billingGroupName"
{-# INLINEABLE ltibgBillingGroupName #-}
{-# DEPRECATED billingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead"  #-}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgMaxResults :: Lens.Lens' ListThingsInBillingGroup (Core.Maybe Core.Natural)
ltibgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltibgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgNextToken :: Lens.Lens' ListThingsInBillingGroup (Core.Maybe Types.NextToken)
ltibgNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltibgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListThingsInBillingGroup where
        toQuery ListThingsInBillingGroup{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListThingsInBillingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListThingsInBillingGroup where
        type Rs ListThingsInBillingGroup = ListThingsInBillingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/billing-groups/" Core.<> Core.toText billingGroupName Core.<>
                             "/things",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListThingsInBillingGroupResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "things" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListThingsInBillingGroup where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"things" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListThingsInBillingGroupResponse' smart constructor.
data ListThingsInBillingGroupResponse = ListThingsInBillingGroupResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results. Will not be returned if operation has returned all results.
  , things :: Core.Maybe [Types.ThingName]
    -- ^ A list of things in the billing group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingsInBillingGroupResponse' value with any optional fields omitted.
mkListThingsInBillingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListThingsInBillingGroupResponse
mkListThingsInBillingGroupResponse responseStatus
  = ListThingsInBillingGroupResponse'{nextToken = Core.Nothing,
                                      things = Core.Nothing, responseStatus}

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgrrsNextToken :: Lens.Lens' ListThingsInBillingGroupResponse (Core.Maybe Types.NextToken)
ltibgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltibgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of things in the billing group.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgrrsThings :: Lens.Lens' ListThingsInBillingGroupResponse (Core.Maybe [Types.ThingName])
ltibgrrsThings = Lens.field @"things"
{-# INLINEABLE ltibgrrsThings #-}
{-# DEPRECATED things "Use generic-lens or generic-optics with 'things' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgrrsResponseStatus :: Lens.Lens' ListThingsInBillingGroupResponse Core.Int
ltibgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltibgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
