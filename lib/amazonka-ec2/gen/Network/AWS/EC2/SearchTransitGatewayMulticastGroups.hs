{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SearchTransitGatewayMulticastGroups (..),
    mkSearchTransitGatewayMulticastGroups,

    -- ** Request lenses
    stgmgDryRun,
    stgmgFilters,
    stgmgMaxResults,
    stgmgNextToken,
    stgmgTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    SearchTransitGatewayMulticastGroupsResponse (..),
    mkSearchTransitGatewayMulticastGroupsResponse,

    -- ** Response lenses
    stgmgrrsMulticastGroups,
    stgmgrrsNextToken,
    stgmgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchTransitGatewayMulticastGroups' smart constructor.
data SearchTransitGatewayMulticastGroups = SearchTransitGatewayMulticastGroups'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.TransitGatewayMulticastDomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchTransitGatewayMulticastGroups' value with any optional fields omitted.
mkSearchTransitGatewayMulticastGroups ::
  SearchTransitGatewayMulticastGroups
mkSearchTransitGatewayMulticastGroups =
  SearchTransitGatewayMulticastGroups'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgDryRun :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe Core.Bool)
stgmgDryRun = Lens.field @"dryRun"
{-# DEPRECATED stgmgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
{-# DEPRECATED stgmgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgMaxResults :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe Core.Natural)
stgmgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED stgmgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgNextToken :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe Types.String)
stgmgNextToken = Lens.field @"nextToken"
{-# DEPRECATED stgmgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgTransitGatewayMulticastDomainId :: Lens.Lens' SearchTransitGatewayMulticastGroups (Core.Maybe Types.TransitGatewayMulticastDomainId)
stgmgTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED stgmgTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance Core.AWSRequest SearchTransitGatewayMulticastGroups where
  type
    Rs SearchTransitGatewayMulticastGroups =
      SearchTransitGatewayMulticastGroupsResponse
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
            ( Core.pure ("Action", "SearchTransitGatewayMulticastGroups")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryValue "TransitGatewayMulticastDomainId"
                            Core.<$> transitGatewayMulticastDomainId
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          SearchTransitGatewayMulticastGroupsResponse'
            Core.<$> (x Core..@? "multicastGroups" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager SearchTransitGatewayMulticastGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"multicastGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkSearchTransitGatewayMulticastGroupsResponse' smart constructor.
data SearchTransitGatewayMulticastGroupsResponse = SearchTransitGatewayMulticastGroupsResponse'
  { -- | Information about the transit gateway multicast group.
    multicastGroups :: Core.Maybe [Types.TransitGatewayMulticastGroup],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchTransitGatewayMulticastGroupsResponse' value with any optional fields omitted.
mkSearchTransitGatewayMulticastGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchTransitGatewayMulticastGroupsResponse
mkSearchTransitGatewayMulticastGroupsResponse responseStatus =
  SearchTransitGatewayMulticastGroupsResponse'
    { multicastGroups =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the transit gateway multicast group.
--
-- /Note:/ Consider using 'multicastGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrrsMulticastGroups :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse (Core.Maybe [Types.TransitGatewayMulticastGroup])
stgmgrrsMulticastGroups = Lens.field @"multicastGroups"
{-# DEPRECATED stgmgrrsMulticastGroups "Use generic-lens or generic-optics with 'multicastGroups' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrrsNextToken :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse (Core.Maybe Types.String)
stgmgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED stgmgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrrsResponseStatus :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse Core.Int
stgmgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED stgmgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
