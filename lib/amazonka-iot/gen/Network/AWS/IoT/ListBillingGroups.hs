{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListBillingGroups (..),
    mkListBillingGroups,

    -- ** Request lenses
    lbgMaxResults,
    lbgNamePrefixFilter,
    lbgNextToken,

    -- * Destructuring the response
    ListBillingGroupsResponse (..),
    mkListBillingGroupsResponse,

    -- ** Response lenses
    lbgrrsBillingGroups,
    lbgrrsNextToken,
    lbgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBillingGroups' smart constructor.
data ListBillingGroups = ListBillingGroups'
  { -- | The maximum number of results to return per request.
    maxResults :: Core.Maybe Core.Natural,
    -- | Limit the results to billing groups whose names have the given prefix.
    namePrefixFilter :: Core.Maybe Types.NamePrefixFilter,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBillingGroups' value with any optional fields omitted.
mkListBillingGroups ::
  ListBillingGroups
mkListBillingGroups =
  ListBillingGroups'
    { maxResults = Core.Nothing,
      namePrefixFilter = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgMaxResults :: Lens.Lens' ListBillingGroups (Core.Maybe Core.Natural)
lbgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lbgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Limit the results to billing groups whose names have the given prefix.
--
-- /Note:/ Consider using 'namePrefixFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgNamePrefixFilter :: Lens.Lens' ListBillingGroups (Core.Maybe Types.NamePrefixFilter)
lbgNamePrefixFilter = Lens.field @"namePrefixFilter"
{-# DEPRECATED lbgNamePrefixFilter "Use generic-lens or generic-optics with 'namePrefixFilter' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgNextToken :: Lens.Lens' ListBillingGroups (Core.Maybe Types.NextToken)
lbgNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListBillingGroups where
  type Rs ListBillingGroups = ListBillingGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/billing-groups",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "namePrefixFilter" Core.<$> namePrefixFilter)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBillingGroupsResponse'
            Core.<$> (x Core..:? "billingGroups")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListBillingGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"billingGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListBillingGroupsResponse' smart constructor.
data ListBillingGroupsResponse = ListBillingGroupsResponse'
  { -- | The list of billing groups.
    billingGroups :: Core.Maybe [Types.GroupNameAndArn],
    -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBillingGroupsResponse' value with any optional fields omitted.
mkListBillingGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBillingGroupsResponse
mkListBillingGroupsResponse responseStatus =
  ListBillingGroupsResponse'
    { billingGroups = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of billing groups.
--
-- /Note:/ Consider using 'billingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrrsBillingGroups :: Lens.Lens' ListBillingGroupsResponse (Core.Maybe [Types.GroupNameAndArn])
lbgrrsBillingGroups = Lens.field @"billingGroups"
{-# DEPRECATED lbgrrsBillingGroups "Use generic-lens or generic-optics with 'billingGroups' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrrsNextToken :: Lens.Lens' ListBillingGroupsResponse (Core.Maybe Types.NextToken)
lbgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrrsResponseStatus :: Lens.Lens' ListBillingGroupsResponse Core.Int
lbgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
