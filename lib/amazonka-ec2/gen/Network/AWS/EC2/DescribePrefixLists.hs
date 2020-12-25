{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribePrefixLists (..),
    mkDescribePrefixLists,

    -- ** Request lenses
    dplDryRun,
    dplFilters,
    dplMaxResults,
    dplNextToken,
    dplPrefixListIds,

    -- * Destructuring the response
    DescribePrefixListsResponse (..),
    mkDescribePrefixListsResponse,

    -- ** Response lenses
    dplrrsNextToken,
    dplrrsPrefixLists,
    dplrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePrefixLists' smart constructor.
data DescribePrefixLists = DescribePrefixLists'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @prefix-list-id@ : The ID of a prefix list.
    --
    --
    --     * @prefix-list-name@ : The name of a prefix list.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | One or more prefix list IDs.
    prefixListIds :: Core.Maybe [Types.PrefixListResourceId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePrefixLists' value with any optional fields omitted.
mkDescribePrefixLists ::
  DescribePrefixLists
mkDescribePrefixLists =
  DescribePrefixLists'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      prefixListIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplDryRun :: Lens.Lens' DescribePrefixLists (Core.Maybe Core.Bool)
dplDryRun = Lens.field @"dryRun"
{-# DEPRECATED dplDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
{-# DEPRECATED dplFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplMaxResults :: Lens.Lens' DescribePrefixLists (Core.Maybe Core.Int)
dplMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dplMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplNextToken :: Lens.Lens' DescribePrefixLists (Core.Maybe Types.String)
dplNextToken = Lens.field @"nextToken"
{-# DEPRECATED dplNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more prefix list IDs.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplPrefixListIds :: Lens.Lens' DescribePrefixLists (Core.Maybe [Types.PrefixListResourceId])
dplPrefixListIds = Lens.field @"prefixListIds"
{-# DEPRECATED dplPrefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead." #-}

instance Core.AWSRequest DescribePrefixLists where
  type Rs DescribePrefixLists = DescribePrefixListsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribePrefixLists")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryList "PrefixListId" Core.<$> prefixListIds)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribePrefixListsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "prefixListSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribePrefixLists where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"prefixLists" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribePrefixListsResponse' smart constructor.
data DescribePrefixListsResponse = DescribePrefixListsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | All available prefix lists.
    prefixLists :: Core.Maybe [Types.PrefixList],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePrefixListsResponse' value with any optional fields omitted.
mkDescribePrefixListsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePrefixListsResponse
mkDescribePrefixListsResponse responseStatus =
  DescribePrefixListsResponse'
    { nextToken = Core.Nothing,
      prefixLists = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrrsNextToken :: Lens.Lens' DescribePrefixListsResponse (Core.Maybe Types.String)
dplrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dplrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | All available prefix lists.
--
-- /Note:/ Consider using 'prefixLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrrsPrefixLists :: Lens.Lens' DescribePrefixListsResponse (Core.Maybe [Types.PrefixList])
dplrrsPrefixLists = Lens.field @"prefixLists"
{-# DEPRECATED dplrrsPrefixLists "Use generic-lens or generic-optics with 'prefixLists' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplrrsResponseStatus :: Lens.Lens' DescribePrefixListsResponse Core.Int
dplrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dplrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
