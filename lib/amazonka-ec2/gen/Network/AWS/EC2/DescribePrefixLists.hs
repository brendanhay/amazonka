{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePrefixLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes available AWS services in a prefix list format, which includes the prefix list name and prefix list ID of the service and the IP address range for the service.
--
-- We recommend that you use 'DescribeManagedPrefixLists' instead.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribePrefixLists
    (
    -- * Creating a request
      DescribePrefixLists (..)
    , mkDescribePrefixLists
    -- ** Request lenses
    , dplDryRun
    , dplFilters
    , dplMaxResults
    , dplNextToken
    , dplPrefixListIds

    -- * Destructuring the response
    , DescribePrefixListsResponse (..)
    , mkDescribePrefixListsResponse
    -- ** Response lenses
    , dplrrsNextToken
    , dplrrsPrefixLists
    , dplrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePrefixLists' smart constructor.
data DescribePrefixLists = DescribePrefixLists'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @prefix-list-id@ : The ID of a prefix list.
--
--
--     * @prefix-list-name@ : The name of a prefix list.
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , prefixListIds :: Core.Maybe [Types.PrefixListResourceId]
    -- ^ One or more prefix list IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePrefixLists' value with any optional fields omitted.
mkDescribePrefixLists
    :: DescribePrefixLists
mkDescribePrefixLists
  = DescribePrefixLists'{dryRun = Core.Nothing,
                         filters = Core.Nothing, maxResults = Core.Nothing,
                         nextToken = Core.Nothing, prefixListIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplDryRun :: Lens.Lens' DescribePrefixLists (Core.Maybe Core.Bool)
dplDryRun = Lens.field @"dryRun"
{-# INLINEABLE dplDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @prefix-list-id@ : The ID of a prefix list.
--
--
--     * @prefix-list-name@ : The name of a prefix list.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplFilters :: Lens.Lens' DescribePrefixLists (Core.Maybe [Types.Filter])
dplFilters = Lens.field @"filters"
{-# INLINEABLE dplFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplMaxResults :: Lens.Lens' DescribePrefixLists (Core.Maybe Core.Int)
dplMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dplMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplNextToken :: Lens.Lens' DescribePrefixLists (Core.Maybe Core.Text)
dplNextToken = Lens.field @"nextToken"
{-# INLINEABLE dplNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more prefix list IDs.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplPrefixListIds :: Lens.Lens' DescribePrefixLists (Core.Maybe [Types.PrefixListResourceId])
dplPrefixListIds = Lens.field @"prefixListIds"
{-# INLINEABLE dplPrefixListIds #-}
{-# DEPRECATED prefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead"  #-}

instance Core.ToQuery DescribePrefixLists where
        toQuery DescribePrefixLists{..}
          = Core.toQueryPair "Action" ("DescribePrefixLists" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "PrefixListId")
                prefixListIds

instance Core.ToHeaders DescribePrefixLists where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribePrefixLists where
        type Rs DescribePrefixLists = DescribePrefixListsResponse
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
          = Response.receiveXML
              (\ s h x ->
                 DescribePrefixListsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "prefixListSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribePrefixLists where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"prefixLists" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribePrefixListsResponse' smart constructor.
data DescribePrefixListsResponse = DescribePrefixListsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , prefixLists :: Core.Maybe [Types.PrefixList]
    -- ^ All available prefix lists.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePrefixListsResponse' value with any optional fields omitted.
mkDescribePrefixListsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePrefixListsResponse
mkDescribePrefixListsResponse responseStatus
  = DescribePrefixListsResponse'{nextToken = Core.Nothing,
                                 prefixLists = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrrsNextToken :: Lens.Lens' DescribePrefixListsResponse (Core.Maybe Core.Text)
dplrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dplrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | All available prefix lists.
--
-- /Note:/ Consider using 'prefixLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrrsPrefixLists :: Lens.Lens' DescribePrefixListsResponse (Core.Maybe [Types.PrefixList])
dplrrsPrefixLists = Lens.field @"prefixLists"
{-# INLINEABLE dplrrsPrefixLists #-}
{-# DEPRECATED prefixLists "Use generic-lens or generic-optics with 'prefixLists' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrrsResponseStatus :: Lens.Lens' DescribePrefixListsResponse Core.Int
dplrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dplrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
