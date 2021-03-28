{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListBillingGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the billing groups you have created.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListBillingGroups
    (
    -- * Creating a request
      ListBillingGroups (..)
    , mkListBillingGroups
    -- ** Request lenses
    , lbgMaxResults
    , lbgNamePrefixFilter
    , lbgNextToken

    -- * Destructuring the response
    , ListBillingGroupsResponse (..)
    , mkListBillingGroupsResponse
    -- ** Response lenses
    , lbgrrsBillingGroups
    , lbgrrsNextToken
    , lbgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBillingGroups' smart constructor.
data ListBillingGroups = ListBillingGroups'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return per request.
  , namePrefixFilter :: Core.Maybe Types.NamePrefixFilter
    -- ^ Limit the results to billing groups whose names have the given prefix.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBillingGroups' value with any optional fields omitted.
mkListBillingGroups
    :: ListBillingGroups
mkListBillingGroups
  = ListBillingGroups'{maxResults = Core.Nothing,
                       namePrefixFilter = Core.Nothing, nextToken = Core.Nothing}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgMaxResults :: Lens.Lens' ListBillingGroups (Core.Maybe Core.Natural)
lbgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lbgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Limit the results to billing groups whose names have the given prefix.
--
-- /Note:/ Consider using 'namePrefixFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgNamePrefixFilter :: Lens.Lens' ListBillingGroups (Core.Maybe Types.NamePrefixFilter)
lbgNamePrefixFilter = Lens.field @"namePrefixFilter"
{-# INLINEABLE lbgNamePrefixFilter #-}
{-# DEPRECATED namePrefixFilter "Use generic-lens or generic-optics with 'namePrefixFilter' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgNextToken :: Lens.Lens' ListBillingGroups (Core.Maybe Types.NextToken)
lbgNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListBillingGroups where
        toQuery ListBillingGroups{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "namePrefixFilter")
                namePrefixFilter
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListBillingGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListBillingGroups where
        type Rs ListBillingGroups = ListBillingGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/billing-groups",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBillingGroupsResponse' Core.<$>
                   (x Core..:? "billingGroups") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBillingGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"billingGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListBillingGroupsResponse' smart constructor.
data ListBillingGroupsResponse = ListBillingGroupsResponse'
  { billingGroups :: Core.Maybe [Types.GroupNameAndArn]
    -- ^ The list of billing groups.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results, or __null__ if there are no additional results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBillingGroupsResponse' value with any optional fields omitted.
mkListBillingGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBillingGroupsResponse
mkListBillingGroupsResponse responseStatus
  = ListBillingGroupsResponse'{billingGroups = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | The list of billing groups.
--
-- /Note:/ Consider using 'billingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrrsBillingGroups :: Lens.Lens' ListBillingGroupsResponse (Core.Maybe [Types.GroupNameAndArn])
lbgrrsBillingGroups = Lens.field @"billingGroups"
{-# INLINEABLE lbgrrsBillingGroups #-}
{-# DEPRECATED billingGroups "Use generic-lens or generic-optics with 'billingGroups' instead"  #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrrsNextToken :: Lens.Lens' ListBillingGroupsResponse (Core.Maybe Types.NextToken)
lbgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrrsResponseStatus :: Lens.Lens' ListBillingGroupsResponse Core.Int
lbgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
