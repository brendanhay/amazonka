{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeRouteTables (..),
    mkDescribeRouteTables,

    -- ** Request lenses
    drtsDryRun,
    drtsFilters,
    drtsMaxResults,
    drtsNextToken,
    drtsRouteTableIds,

    -- * Destructuring the response
    DescribeRouteTablesResponse (..),
    mkDescribeRouteTablesResponse,

    -- ** Response lenses
    drtrrsNextToken,
    drtrrsRouteTables,
    drtrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRouteTables' smart constructor.
data DescribeRouteTables = DescribeRouteTables'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | One or more route table IDs.
    --
    -- Default: Describes all your route tables.
    routeTableIds :: Core.Maybe [Types.RouteTableId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRouteTables' value with any optional fields omitted.
mkDescribeRouteTables ::
  DescribeRouteTables
mkDescribeRouteTables =
  DescribeRouteTables'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      routeTableIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsDryRun :: Lens.Lens' DescribeRouteTables (Core.Maybe Core.Bool)
drtsDryRun = Lens.field @"dryRun"
{-# DEPRECATED drtsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
{-# DEPRECATED drtsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsMaxResults :: Lens.Lens' DescribeRouteTables (Core.Maybe Core.Natural)
drtsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED drtsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsNextToken :: Lens.Lens' DescribeRouteTables (Core.Maybe Types.String)
drtsNextToken = Lens.field @"nextToken"
{-# DEPRECATED drtsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more route table IDs.
--
-- Default: Describes all your route tables.
--
-- /Note:/ Consider using 'routeTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsRouteTableIds :: Lens.Lens' DescribeRouteTables (Core.Maybe [Types.RouteTableId])
drtsRouteTableIds = Lens.field @"routeTableIds"
{-# DEPRECATED drtsRouteTableIds "Use generic-lens or generic-optics with 'routeTableIds' instead." #-}

instance Core.AWSRequest DescribeRouteTables where
  type Rs DescribeRouteTables = DescribeRouteTablesResponse
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
            ( Core.pure ("Action", "DescribeRouteTables")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryList "RouteTableId" Core.<$> routeTableIds)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeRouteTablesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "routeTableSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeRouteTables where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"routeTables" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Contains the output of DescribeRouteTables.
--
-- /See:/ 'mkDescribeRouteTablesResponse' smart constructor.
data DescribeRouteTablesResponse = DescribeRouteTablesResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about one or more route tables.
    routeTables :: Core.Maybe [Types.RouteTable],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRouteTablesResponse' value with any optional fields omitted.
mkDescribeRouteTablesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRouteTablesResponse
mkDescribeRouteTablesResponse responseStatus =
  DescribeRouteTablesResponse'
    { nextToken = Core.Nothing,
      routeTables = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrrsNextToken :: Lens.Lens' DescribeRouteTablesResponse (Core.Maybe Types.String)
drtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED drtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about one or more route tables.
--
-- /Note:/ Consider using 'routeTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrrsRouteTables :: Lens.Lens' DescribeRouteTablesResponse (Core.Maybe [Types.RouteTable])
drtrrsRouteTables = Lens.field @"routeTables"
{-# DEPRECATED drtrrsRouteTables "Use generic-lens or generic-optics with 'routeTables' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrrsResponseStatus :: Lens.Lens' DescribeRouteTablesResponse Core.Int
drtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
