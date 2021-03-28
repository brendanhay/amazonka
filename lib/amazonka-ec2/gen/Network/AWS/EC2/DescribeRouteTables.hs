{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeRouteTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your route tables.
--
-- Each subnet in your VPC must be associated with a route table. If a subnet is not explicitly associated with any route table, it is implicitly associated with the main route table. This command does not return the subnet ID for implicit associations.
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route Tables> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeRouteTables
    (
    -- * Creating a request
      DescribeRouteTables (..)
    , mkDescribeRouteTables
    -- ** Request lenses
    , drtsDryRun
    , drtsFilters
    , drtsMaxResults
    , drtsNextToken
    , drtsRouteTableIds

    -- * Destructuring the response
    , DescribeRouteTablesResponse (..)
    , mkDescribeRouteTablesResponse
    -- ** Response lenses
    , drtrrsNextToken
    , drtrrsRouteTables
    , drtrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRouteTables' smart constructor.
data DescribeRouteTables = DescribeRouteTables'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @association.route-table-association-id@ - The ID of an association ID for the route table.
--
--
--     * @association.route-table-id@ - The ID of the route table involved in the association.
--
--
--     * @association.subnet-id@ - The ID of the subnet involved in the association.
--
--
--     * @association.main@ - Indicates whether the route table is the main route table for the VPC (@true@ | @false@ ). Route tables that do not have an association ID are not returned in the response.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the route table.
--
--
--     * @route-table-id@ - The ID of the route table.
--
--
--     * @route.destination-cidr-block@ - The IPv4 CIDR range specified in a route in the table.
--
--
--     * @route.destination-ipv6-cidr-block@ - The IPv6 CIDR range specified in a route in the route table.
--
--
--     * @route.destination-prefix-list-id@ - The ID (prefix) of the AWS service specified in a route in the table.
--
--
--     * @route.egress-only-internet-gateway-id@ - The ID of an egress-only Internet gateway specified in a route in the route table.
--
--
--     * @route.gateway-id@ - The ID of a gateway specified in a route in the table.
--
--
--     * @route.instance-id@ - The ID of an instance specified in a route in the table.
--
--
--     * @route.nat-gateway-id@ - The ID of a NAT gateway.
--
--
--     * @route.transit-gateway-id@ - The ID of a transit gateway.
--
--
--     * @route.origin@ - Describes how the route was created. @CreateRouteTable@ indicates that the route was automatically created when the route table was created; @CreateRoute@ indicates that the route was manually added to the route table; @EnableVgwRoutePropagation@ indicates that the route was propagated by route propagation.
--
--
--     * @route.state@ - The state of a route in the route table (@active@ | @blackhole@ ). The blackhole state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, the specified NAT instance has been terminated, and so on).
--
--
--     * @route.vpc-peering-connection-id@ - The ID of a VPC peering connection specified in a route in the table.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC for the route table.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , routeTableIds :: Core.Maybe [Types.RouteTableId]
    -- ^ One or more route table IDs.
--
-- Default: Describes all your route tables.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRouteTables' value with any optional fields omitted.
mkDescribeRouteTables
    :: DescribeRouteTables
mkDescribeRouteTables
  = DescribeRouteTables'{dryRun = Core.Nothing,
                         filters = Core.Nothing, maxResults = Core.Nothing,
                         nextToken = Core.Nothing, routeTableIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsDryRun :: Lens.Lens' DescribeRouteTables (Core.Maybe Core.Bool)
drtsDryRun = Lens.field @"dryRun"
{-# INLINEABLE drtsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @association.route-table-association-id@ - The ID of an association ID for the route table.
--
--
--     * @association.route-table-id@ - The ID of the route table involved in the association.
--
--
--     * @association.subnet-id@ - The ID of the subnet involved in the association.
--
--
--     * @association.main@ - Indicates whether the route table is the main route table for the VPC (@true@ | @false@ ). Route tables that do not have an association ID are not returned in the response.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the route table.
--
--
--     * @route-table-id@ - The ID of the route table.
--
--
--     * @route.destination-cidr-block@ - The IPv4 CIDR range specified in a route in the table.
--
--
--     * @route.destination-ipv6-cidr-block@ - The IPv6 CIDR range specified in a route in the route table.
--
--
--     * @route.destination-prefix-list-id@ - The ID (prefix) of the AWS service specified in a route in the table.
--
--
--     * @route.egress-only-internet-gateway-id@ - The ID of an egress-only Internet gateway specified in a route in the route table.
--
--
--     * @route.gateway-id@ - The ID of a gateway specified in a route in the table.
--
--
--     * @route.instance-id@ - The ID of an instance specified in a route in the table.
--
--
--     * @route.nat-gateway-id@ - The ID of a NAT gateway.
--
--
--     * @route.transit-gateway-id@ - The ID of a transit gateway.
--
--
--     * @route.origin@ - Describes how the route was created. @CreateRouteTable@ indicates that the route was automatically created when the route table was created; @CreateRoute@ indicates that the route was manually added to the route table; @EnableVgwRoutePropagation@ indicates that the route was propagated by route propagation.
--
--
--     * @route.state@ - The state of a route in the route table (@active@ | @blackhole@ ). The blackhole state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, the specified NAT instance has been terminated, and so on).
--
--
--     * @route.vpc-peering-connection-id@ - The ID of a VPC peering connection specified in a route in the table.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC for the route table.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsFilters :: Lens.Lens' DescribeRouteTables (Core.Maybe [Types.Filter])
drtsFilters = Lens.field @"filters"
{-# INLINEABLE drtsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsMaxResults :: Lens.Lens' DescribeRouteTables (Core.Maybe Core.Natural)
drtsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE drtsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsNextToken :: Lens.Lens' DescribeRouteTables (Core.Maybe Core.Text)
drtsNextToken = Lens.field @"nextToken"
{-# INLINEABLE drtsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more route table IDs.
--
-- Default: Describes all your route tables.
--
-- /Note:/ Consider using 'routeTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsRouteTableIds :: Lens.Lens' DescribeRouteTables (Core.Maybe [Types.RouteTableId])
drtsRouteTableIds = Lens.field @"routeTableIds"
{-# INLINEABLE drtsRouteTableIds #-}
{-# DEPRECATED routeTableIds "Use generic-lens or generic-optics with 'routeTableIds' instead"  #-}

instance Core.ToQuery DescribeRouteTables where
        toQuery DescribeRouteTables{..}
          = Core.toQueryPair "Action" ("DescribeRouteTables" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RouteTableId")
                routeTableIds

instance Core.ToHeaders DescribeRouteTables where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeRouteTables where
        type Rs DescribeRouteTables = DescribeRouteTablesResponse
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
                 DescribeRouteTablesResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "routeTableSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeRouteTables where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"routeTables" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output of DescribeRouteTables.
--
-- /See:/ 'mkDescribeRouteTablesResponse' smart constructor.
data DescribeRouteTablesResponse = DescribeRouteTablesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , routeTables :: Core.Maybe [Types.RouteTable]
    -- ^ Information about one or more route tables.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRouteTablesResponse' value with any optional fields omitted.
mkDescribeRouteTablesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRouteTablesResponse
mkDescribeRouteTablesResponse responseStatus
  = DescribeRouteTablesResponse'{nextToken = Core.Nothing,
                                 routeTables = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrrsNextToken :: Lens.Lens' DescribeRouteTablesResponse (Core.Maybe Core.Text)
drtrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE drtrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about one or more route tables.
--
-- /Note:/ Consider using 'routeTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrrsRouteTables :: Lens.Lens' DescribeRouteTablesResponse (Core.Maybe [Types.RouteTable])
drtrrsRouteTables = Lens.field @"routeTables"
{-# INLINEABLE drtrrsRouteTables #-}
{-# DEPRECATED routeTables "Use generic-lens or generic-optics with 'routeTables' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrrsResponseStatus :: Lens.Lens' DescribeRouteTablesResponse Core.Int
drtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
