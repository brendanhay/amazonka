{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeTransitGatewayMulticastDomains (..)
    , mkDescribeTransitGatewayMulticastDomains
    -- ** Request lenses
    , dtgmdsDryRun
    , dtgmdsFilters
    , dtgmdsMaxResults
    , dtgmdsNextToken
    , dtgmdsTransitGatewayMulticastDomainIds

    -- * Destructuring the response
    , DescribeTransitGatewayMulticastDomainsResponse (..)
    , mkDescribeTransitGatewayMulticastDomainsResponse
    -- ** Response lenses
    , dtgmdrgrsNextToken
    , dtgmdrgrsTransitGatewayMulticastDomains
    , dtgmdrgrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTransitGatewayMulticastDomains' smart constructor.
data DescribeTransitGatewayMulticastDomains = DescribeTransitGatewayMulticastDomains'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , transitGatewayMulticastDomainIds :: Core.Maybe [Types.TransitGatewayMulticastDomainId]
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTransitGatewayMulticastDomains' value with any optional fields omitted.
mkDescribeTransitGatewayMulticastDomains
    :: DescribeTransitGatewayMulticastDomains
mkDescribeTransitGatewayMulticastDomains
  = DescribeTransitGatewayMulticastDomains'{dryRun = Core.Nothing,
                                            filters = Core.Nothing, maxResults = Core.Nothing,
                                            nextToken = Core.Nothing,
                                            transitGatewayMulticastDomainIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsDryRun :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe Core.Bool)
dtgmdsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgmdsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE dtgmdsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsMaxResults :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe Core.Natural)
dtgmdsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dtgmdsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsNextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe Core.Text)
dtgmdsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtgmdsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsTransitGatewayMulticastDomainIds :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Core.Maybe [Types.TransitGatewayMulticastDomainId])
dtgmdsTransitGatewayMulticastDomainIds = Lens.field @"transitGatewayMulticastDomainIds"
{-# INLINEABLE dtgmdsTransitGatewayMulticastDomainIds #-}
{-# DEPRECATED transitGatewayMulticastDomainIds "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainIds' instead"  #-}

instance Core.ToQuery DescribeTransitGatewayMulticastDomains where
        toQuery DescribeTransitGatewayMulticastDomains{..}
          = Core.toQueryPair "Action"
              ("DescribeTransitGatewayMulticastDomains" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "TransitGatewayMulticastDomainIds")
                transitGatewayMulticastDomainIds

instance Core.ToHeaders DescribeTransitGatewayMulticastDomains
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeTransitGatewayMulticastDomains
         where
        type Rs DescribeTransitGatewayMulticastDomains =
             DescribeTransitGatewayMulticastDomainsResponse
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
                 DescribeTransitGatewayMulticastDomainsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "transitGatewayMulticastDomains" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTransitGatewayMulticastDomains
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"transitGatewayMulticastDomains" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeTransitGatewayMulticastDomainsResponse' smart constructor.
data DescribeTransitGatewayMulticastDomainsResponse = DescribeTransitGatewayMulticastDomainsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , transitGatewayMulticastDomains :: Core.Maybe [Types.TransitGatewayMulticastDomain]
    -- ^ Information about the transit gateway multicast domains.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTransitGatewayMulticastDomainsResponse' value with any optional fields omitted.
mkDescribeTransitGatewayMulticastDomainsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTransitGatewayMulticastDomainsResponse
mkDescribeTransitGatewayMulticastDomainsResponse responseStatus
  = DescribeTransitGatewayMulticastDomainsResponse'{nextToken =
                                                      Core.Nothing,
                                                    transitGatewayMulticastDomains = Core.Nothing,
                                                    responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrgrsNextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse (Core.Maybe Core.Text)
dtgmdrgrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtgmdrgrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the transit gateway multicast domains.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrgrsTransitGatewayMulticastDomains :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse (Core.Maybe [Types.TransitGatewayMulticastDomain])
dtgmdrgrsTransitGatewayMulticastDomains = Lens.field @"transitGatewayMulticastDomains"
{-# INLINEABLE dtgmdrgrsTransitGatewayMulticastDomains #-}
{-# DEPRECATED transitGatewayMulticastDomains "Use generic-lens or generic-optics with 'transitGatewayMulticastDomains' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrgrsResponseStatus :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse Core.Int
dtgmdrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgmdrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
