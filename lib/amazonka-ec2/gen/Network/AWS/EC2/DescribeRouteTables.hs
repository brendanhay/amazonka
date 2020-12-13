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
    drtsFilters,
    drtsNextToken,
    drtsDryRun,
    drtsMaxResults,
    drtsRouteTableIds,

    -- * Destructuring the response
    DescribeRouteTablesResponse (..),
    mkDescribeRouteTablesResponse,

    -- ** Response lenses
    drtrsNextToken,
    drtrsRouteTables,
    drtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRouteTables' smart constructor.
data DescribeRouteTables = DescribeRouteTables'
  { -- | One or more filters.
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
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | One or more route table IDs.
    --
    -- Default: Describes all your route tables.
    routeTableIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRouteTables' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
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
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'routeTableIds' - One or more route table IDs.
--
-- Default: Describes all your route tables.
mkDescribeRouteTables ::
  DescribeRouteTables
mkDescribeRouteTables =
  DescribeRouteTables'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      routeTableIds = Lude.Nothing
    }

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
drtsFilters :: Lens.Lens' DescribeRouteTables (Lude.Maybe [Filter])
drtsFilters = Lens.lens (filters :: DescribeRouteTables -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeRouteTables)
{-# DEPRECATED drtsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsNextToken :: Lens.Lens' DescribeRouteTables (Lude.Maybe Lude.Text)
drtsNextToken = Lens.lens (nextToken :: DescribeRouteTables -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRouteTables)
{-# DEPRECATED drtsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsDryRun :: Lens.Lens' DescribeRouteTables (Lude.Maybe Lude.Bool)
drtsDryRun = Lens.lens (dryRun :: DescribeRouteTables -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeRouteTables)
{-# DEPRECATED drtsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsMaxResults :: Lens.Lens' DescribeRouteTables (Lude.Maybe Lude.Natural)
drtsMaxResults = Lens.lens (maxResults :: DescribeRouteTables -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeRouteTables)
{-# DEPRECATED drtsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | One or more route table IDs.
--
-- Default: Describes all your route tables.
--
-- /Note:/ Consider using 'routeTableIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsRouteTableIds :: Lens.Lens' DescribeRouteTables (Lude.Maybe [Lude.Text])
drtsRouteTableIds = Lens.lens (routeTableIds :: DescribeRouteTables -> Lude.Maybe [Lude.Text]) (\s a -> s {routeTableIds = a} :: DescribeRouteTables)
{-# DEPRECATED drtsRouteTableIds "Use generic-lens or generic-optics with 'routeTableIds' instead." #-}

instance Page.AWSPager DescribeRouteTables where
  page rq rs
    | Page.stop (rs Lens.^. drtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drtrsRouteTables) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drtsNextToken Lens..~ rs Lens.^. drtrsNextToken

instance Lude.AWSRequest DescribeRouteTables where
  type Rs DescribeRouteTables = DescribeRouteTablesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeRouteTablesResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "routeTableSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRouteTables where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeRouteTables where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRouteTables where
  toQuery DescribeRouteTables' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeRouteTables" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        Lude.toQuery
          (Lude.toQueryList "RouteTableId" Lude.<$> routeTableIds)
      ]

-- | Contains the output of DescribeRouteTables.
--
-- /See:/ 'mkDescribeRouteTablesResponse' smart constructor.
data DescribeRouteTablesResponse = DescribeRouteTablesResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about one or more route tables.
    routeTables :: Lude.Maybe [RouteTable],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRouteTablesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'routeTables' - Information about one or more route tables.
-- * 'responseStatus' - The response status code.
mkDescribeRouteTablesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRouteTablesResponse
mkDescribeRouteTablesResponse pResponseStatus_ =
  DescribeRouteTablesResponse'
    { nextToken = Lude.Nothing,
      routeTables = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrsNextToken :: Lens.Lens' DescribeRouteTablesResponse (Lude.Maybe Lude.Text)
drtrsNextToken = Lens.lens (nextToken :: DescribeRouteTablesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRouteTablesResponse)
{-# DEPRECATED drtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about one or more route tables.
--
-- /Note:/ Consider using 'routeTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrsRouteTables :: Lens.Lens' DescribeRouteTablesResponse (Lude.Maybe [RouteTable])
drtrsRouteTables = Lens.lens (routeTables :: DescribeRouteTablesResponse -> Lude.Maybe [RouteTable]) (\s a -> s {routeTables = a} :: DescribeRouteTablesResponse)
{-# DEPRECATED drtrsRouteTables "Use generic-lens or generic-optics with 'routeTables' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrsResponseStatus :: Lens.Lens' DescribeRouteTablesResponse Lude.Int
drtrsResponseStatus = Lens.lens (responseStatus :: DescribeRouteTablesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRouteTablesResponse)
{-# DEPRECATED drtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
