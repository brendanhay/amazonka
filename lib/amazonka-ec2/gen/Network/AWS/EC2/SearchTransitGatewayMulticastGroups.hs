{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.SearchTransitGatewayMulticastGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches one or more transit gateway multicast groups and returns the group membership information.
--
-- This operation returns paginated results.
module Network.AWS.EC2.SearchTransitGatewayMulticastGroups
    (
    -- * Creating a request
      SearchTransitGatewayMulticastGroups (..)
    , mkSearchTransitGatewayMulticastGroups
    -- ** Request lenses
    , stgmgDryRun
    , stgmgFilters
    , stgmgMaxResults
    , stgmgNextToken
    , stgmgTransitGatewayMulticastDomainId

    -- * Destructuring the response
    , SearchTransitGatewayMulticastGroupsResponse (..)
    , mkSearchTransitGatewayMulticastGroupsResponse
    -- ** Response lenses
    , stgmgrrsMulticastGroups
    , stgmgrrsNextToken
    , stgmgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchTransitGatewayMulticastGroups' smart constructor.
data SearchTransitGatewayMulticastGroups = SearchTransitGatewayMulticastGroups'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
--
--
--     * @group-ip-address@ - The IP address of the transit gateway multicast group.
--
--
--     * @is-group-member@ - The resource is a group member. Valid values are @true@ | @false@ .
--
--
--     * @is-group-source@ - The resource is a group source. Valid values are @true@ | @false@ .
--
--
--     * @member-type@ - The member type. Valid values are @igmp@ | @static@ .
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The type of resource. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @tgw-peering@ .
--
--
--     * @source-type@ - The source type. Valid values are @igmp@ | @static@ .
--
--
--     * @state@ - The state of the subnet association. Valid values are @associated@ | @associated@ | @disassociated@ | @disassociating@ .
--
--
--     * @subnet-id@ - The ID of the subnet.
--
--
--     * @transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchTransitGatewayMulticastGroups' value with any optional fields omitted.
mkSearchTransitGatewayMulticastGroups
    :: SearchTransitGatewayMulticastGroups
mkSearchTransitGatewayMulticastGroups
  = SearchTransitGatewayMulticastGroups'{dryRun = Core.Nothing,
                                         filters = Core.Nothing, maxResults = Core.Nothing,
                                         nextToken = Core.Nothing,
                                         transitGatewayMulticastDomainId = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgDryRun :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe Core.Bool)
stgmgDryRun = Lens.field @"dryRun"
{-# INLINEABLE stgmgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. The possible values are:
--
--
--     * @group-ip-address@ - The IP address of the transit gateway multicast group.
--
--
--     * @is-group-member@ - The resource is a group member. Valid values are @true@ | @false@ .
--
--
--     * @is-group-source@ - The resource is a group source. Valid values are @true@ | @false@ .
--
--
--     * @member-type@ - The member type. Valid values are @igmp@ | @static@ .
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The type of resource. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @tgw-peering@ .
--
--
--     * @source-type@ - The source type. Valid values are @igmp@ | @static@ .
--
--
--     * @state@ - The state of the subnet association. Valid values are @associated@ | @associated@ | @disassociated@ | @disassociating@ .
--
--
--     * @subnet-id@ - The ID of the subnet.
--
--
--     * @transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgFilters :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe [Types.Filter])
stgmgFilters = Lens.field @"filters"
{-# INLINEABLE stgmgFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgMaxResults :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe Core.Natural)
stgmgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE stgmgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgNextToken :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe Core.Text)
stgmgNextToken = Lens.field @"nextToken"
{-# INLINEABLE stgmgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgTransitGatewayMulticastDomainId :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe Types.TransitGatewayMulticastDomainId)
stgmgTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE stgmgTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.ToQuery SearchTransitGatewayMulticastGroups where
        toQuery SearchTransitGatewayMulticastGroups{..}
          = Core.toQueryPair "Action"
              ("SearchTransitGatewayMulticastGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TransitGatewayMulticastDomainId")
                transitGatewayMulticastDomainId

instance Core.ToHeaders SearchTransitGatewayMulticastGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SearchTransitGatewayMulticastGroups where
        type Rs SearchTransitGatewayMulticastGroups =
             SearchTransitGatewayMulticastGroupsResponse
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
                 SearchTransitGatewayMulticastGroupsResponse' Core.<$>
                   (x Core..@? "multicastGroups" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager SearchTransitGatewayMulticastGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"multicastGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkSearchTransitGatewayMulticastGroupsResponse' smart constructor.
data SearchTransitGatewayMulticastGroupsResponse = SearchTransitGatewayMulticastGroupsResponse'
  { multicastGroups :: Core.Maybe [Types.TransitGatewayMulticastGroup]
    -- ^ Information about the transit gateway multicast group.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchTransitGatewayMulticastGroupsResponse' value with any optional fields omitted.
mkSearchTransitGatewayMulticastGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchTransitGatewayMulticastGroupsResponse
mkSearchTransitGatewayMulticastGroupsResponse responseStatus
  = SearchTransitGatewayMulticastGroupsResponse'{multicastGroups =
                                                   Core.Nothing,
                                                 nextToken = Core.Nothing, responseStatus}

-- | Information about the transit gateway multicast group.
--
-- /Note:/ Consider using 'multicastGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrrsMulticastGroups :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse (Core.Maybe [Types.TransitGatewayMulticastGroup])
stgmgrrsMulticastGroups = Lens.field @"multicastGroups"
{-# INLINEABLE stgmgrrsMulticastGroups #-}
{-# DEPRECATED multicastGroups "Use generic-lens or generic-optics with 'multicastGroups' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrrsNextToken :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse (Core.Maybe Core.Text)
stgmgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE stgmgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrrsResponseStatus :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse Core.Int
stgmgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE stgmgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
