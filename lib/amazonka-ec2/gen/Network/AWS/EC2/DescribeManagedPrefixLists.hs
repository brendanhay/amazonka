{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeManagedPrefixLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your managed prefix lists and any AWS-managed prefix lists.
--
-- To view the entries for your prefix list, use 'GetManagedPrefixListEntries' .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeManagedPrefixLists
  ( -- * Creating a request
    DescribeManagedPrefixLists (..),
    mkDescribeManagedPrefixLists,

    -- ** Request lenses
    dmplsDryRun,
    dmplsFilters,
    dmplsMaxResults,
    dmplsNextToken,
    dmplsPrefixListIds,

    -- * Destructuring the response
    DescribeManagedPrefixListsResponse (..),
    mkDescribeManagedPrefixListsResponse,

    -- ** Response lenses
    dmplrfrsNextToken,
    dmplrfrsPrefixLists,
    dmplrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeManagedPrefixLists' smart constructor.
data DescribeManagedPrefixLists = DescribeManagedPrefixLists'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @owner-id@ - The ID of the prefix list owner.
    --
    --
    --     * @prefix-list-id@ - The ID of the prefix list.
    --
    --
    --     * @prefix-list-name@ - The name of the prefix list.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | One or more prefix list IDs.
    prefixListIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeManagedPrefixLists' value with any optional fields omitted.
mkDescribeManagedPrefixLists ::
  DescribeManagedPrefixLists
mkDescribeManagedPrefixLists =
  DescribeManagedPrefixLists'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      prefixListIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsDryRun :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe Core.Bool)
dmplsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dmplsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @owner-id@ - The ID of the prefix list owner.
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @prefix-list-name@ - The name of the prefix list.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsFilters :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe [Types.Filter])
dmplsFilters = Lens.field @"filters"
{-# DEPRECATED dmplsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsMaxResults :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe Core.Natural)
dmplsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dmplsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsNextToken :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe Types.NextToken)
dmplsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmplsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more prefix list IDs.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplsPrefixListIds :: Lens.Lens' DescribeManagedPrefixLists (Core.Maybe [Types.String])
dmplsPrefixListIds = Lens.field @"prefixListIds"
{-# DEPRECATED dmplsPrefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead." #-}

instance Core.AWSRequest DescribeManagedPrefixLists where
  type
    Rs DescribeManagedPrefixLists =
      DescribeManagedPrefixListsResponse
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
            ( Core.pure ("Action", "DescribeManagedPrefixLists")
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
          DescribeManagedPrefixListsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "prefixListSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeManagedPrefixLists where
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

-- | /See:/ 'mkDescribeManagedPrefixListsResponse' smart constructor.
data DescribeManagedPrefixListsResponse = DescribeManagedPrefixListsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the prefix lists.
    prefixLists :: Core.Maybe [Types.ManagedPrefixList],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeManagedPrefixListsResponse' value with any optional fields omitted.
mkDescribeManagedPrefixListsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeManagedPrefixListsResponse
mkDescribeManagedPrefixListsResponse responseStatus =
  DescribeManagedPrefixListsResponse'
    { nextToken = Core.Nothing,
      prefixLists = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrfrsNextToken :: Lens.Lens' DescribeManagedPrefixListsResponse (Core.Maybe Types.NextToken)
dmplrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmplrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the prefix lists.
--
-- /Note:/ Consider using 'prefixLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrfrsPrefixLists :: Lens.Lens' DescribeManagedPrefixListsResponse (Core.Maybe [Types.ManagedPrefixList])
dmplrfrsPrefixLists = Lens.field @"prefixLists"
{-# DEPRECATED dmplrfrsPrefixLists "Use generic-lens or generic-optics with 'prefixLists' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmplrfrsResponseStatus :: Lens.Lens' DescribeManagedPrefixListsResponse Core.Int
dmplrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmplrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
