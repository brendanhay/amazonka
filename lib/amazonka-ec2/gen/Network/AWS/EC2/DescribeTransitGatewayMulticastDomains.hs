{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway multicast domains.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
  ( -- * Creating a request
    DescribeTransitGatewayMulticastDomains (..),
    mkDescribeTransitGatewayMulticastDomains,

    -- ** Request lenses
    dtgmdsDryRun,
    dtgmdsFilters,
    dtgmdsMaxResults,
    dtgmdsNextToken,
    dtgmdsTransitGatewayMulticastDomainIds,

    -- * Destructuring the response
    DescribeTransitGatewayMulticastDomainsResponse (..),
    mkDescribeTransitGatewayMulticastDomainsResponse,

    -- ** Response lenses
    dtgmdrgrsNextToken,
    dtgmdrgrsTransitGatewayMulticastDomains,
    dtgmdrgrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTransitGatewayMulticastDomains' smart constructor.
data DescribeTransitGatewayMulticastDomains = DescribeTransitGatewayMulticastDomains'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @state@ - The state of the transit gateway multicast domain. Valid values are @pending@ | @available@ | @deleting@ | @deleted@ .
    --
    --
    --     * @transit-gateway-id@ - The ID of the transit gateway.
    --
    --
    --     * @transit-gateway-multicast-domain-id@ - The ID of the transit gateway multicast domain.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainIds :: Core.Maybe [Types.TransitGatewayMulticastDomainId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTransitGatewayMulticastDomains' value with any optional fields omitted.
mkDescribeTransitGatewayMulticastDomains ::
  DescribeTransitGatewayMulticastDomains
mkDescribeTransitGatewayMulticastDomains =
  DescribeTransitGatewayMulticastDomains'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      transitGatewayMulticastDomainIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsDryRun :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe Core.Bool)
dtgmdsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgmdsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @state@ - The state of the transit gateway multicast domain. Valid values are @pending@ | @available@ | @deleting@ | @deleted@ .
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--     * @transit-gateway-multicast-domain-id@ - The ID of the transit gateway multicast domain.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsFilters :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe [Types.Filter])
dtgmdsFilters = Lens.field @"filters"
{-# DEPRECATED dtgmdsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsMaxResults :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe Core.Natural)
dtgmdsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dtgmdsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsNextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe Types.String)
dtgmdsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtgmdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsTransitGatewayMulticastDomainIds :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe [Types.TransitGatewayMulticastDomainId])
dtgmdsTransitGatewayMulticastDomainIds = Lens.field @"transitGatewayMulticastDomainIds"
{-# DEPRECATED dtgmdsTransitGatewayMulticastDomainIds "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainIds' instead." #-}

instance Core.AWSRequest DescribeTransitGatewayMulticastDomains where
  type
    Rs DescribeTransitGatewayMulticastDomains =
      DescribeTransitGatewayMulticastDomainsResponse
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
            ( Core.pure ("Action", "DescribeTransitGatewayMulticastDomains")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryList "TransitGatewayMulticastDomainIds"
                            Core.<$> transitGatewayMulticastDomainIds
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayMulticastDomainsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "transitGatewayMulticastDomains"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTransitGatewayMulticastDomains where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"transitGatewayMulticastDomains" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeTransitGatewayMulticastDomainsResponse' smart constructor.
data DescribeTransitGatewayMulticastDomainsResponse = DescribeTransitGatewayMulticastDomainsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the transit gateway multicast domains.
    transitGatewayMulticastDomains :: Core.Maybe [Types.TransitGatewayMulticastDomain],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTransitGatewayMulticastDomainsResponse' value with any optional fields omitted.
mkDescribeTransitGatewayMulticastDomainsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTransitGatewayMulticastDomainsResponse
mkDescribeTransitGatewayMulticastDomainsResponse responseStatus =
  DescribeTransitGatewayMulticastDomainsResponse'
    { nextToken =
        Core.Nothing,
      transitGatewayMulticastDomains = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrgrsNextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse (Core.Maybe Types.String)
dtgmdrgrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtgmdrgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the transit gateway multicast domains.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrgrsTransitGatewayMulticastDomains :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse (Core.Maybe [Types.TransitGatewayMulticastDomain])
dtgmdrgrsTransitGatewayMulticastDomains = Lens.field @"transitGatewayMulticastDomains"
{-# DEPRECATED dtgmdrgrsTransitGatewayMulticastDomains "Use generic-lens or generic-optics with 'transitGatewayMulticastDomains' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrgrsResponseStatus :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse Core.Int
dtgmdrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgmdrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
