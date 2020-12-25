{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNatGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your NAT gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNatGateways
  ( -- * Creating a request
    DescribeNatGateways (..),
    mkDescribeNatGateways,

    -- ** Request lenses
    dngDryRun,
    dngFilter,
    dngMaxResults,
    dngNatGatewayIds,
    dngNextToken,

    -- * Destructuring the response
    DescribeNatGatewaysResponse (..),
    mkDescribeNatGatewaysResponse,

    -- ** Response lenses
    dngrrsNatGateways,
    dngrrsNextToken,
    dngrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeNatGateways' smart constructor.
data DescribeNatGateways = DescribeNatGateways'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @nat-gateway-id@ - The ID of the NAT gateway.
    --
    --
    --     * @state@ - The state of the NAT gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
    --
    --
    --     * @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
    filter :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more NAT gateway IDs.
    natGatewayIds :: Core.Maybe [Types.NatGatewayId],
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNatGateways' value with any optional fields omitted.
mkDescribeNatGateways ::
  DescribeNatGateways
mkDescribeNatGateways =
  DescribeNatGateways'
    { dryRun = Core.Nothing,
      filter = Core.Nothing,
      maxResults = Core.Nothing,
      natGatewayIds = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngDryRun :: Lens.Lens' DescribeNatGateways (Core.Maybe Core.Bool)
dngDryRun = Lens.field @"dryRun"
{-# DEPRECATED dngDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @nat-gateway-id@ - The ID of the NAT gateway.
--
--
--     * @state@ - The state of the NAT gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngFilter :: Lens.Lens' DescribeNatGateways (Core.Maybe [Types.Filter])
dngFilter = Lens.field @"filter"
{-# DEPRECATED dngFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngMaxResults :: Lens.Lens' DescribeNatGateways (Core.Maybe Core.Natural)
dngMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dngMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | One or more NAT gateway IDs.
--
-- /Note:/ Consider using 'natGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngNatGatewayIds :: Lens.Lens' DescribeNatGateways (Core.Maybe [Types.NatGatewayId])
dngNatGatewayIds = Lens.field @"natGatewayIds"
{-# DEPRECATED dngNatGatewayIds "Use generic-lens or generic-optics with 'natGatewayIds' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngNextToken :: Lens.Lens' DescribeNatGateways (Core.Maybe Types.NextToken)
dngNextToken = Lens.field @"nextToken"
{-# DEPRECATED dngNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeNatGateways where
  type Rs DescribeNatGateways = DescribeNatGatewaysResponse
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
            ( Core.pure ("Action", "DescribeNatGateways")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filter)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryList "NatGatewayId" Core.<$> natGatewayIds)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNatGatewaysResponse'
            Core.<$> (x Core..@? "natGatewaySet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeNatGateways where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"natGateways" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeNatGatewaysResponse' smart constructor.
data DescribeNatGatewaysResponse = DescribeNatGatewaysResponse'
  { -- | Information about the NAT gateways.
    natGateways :: Core.Maybe [Types.NatGateway],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeNatGatewaysResponse' value with any optional fields omitted.
mkDescribeNatGatewaysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeNatGatewaysResponse
mkDescribeNatGatewaysResponse responseStatus =
  DescribeNatGatewaysResponse'
    { natGateways = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the NAT gateways.
--
-- /Note:/ Consider using 'natGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrrsNatGateways :: Lens.Lens' DescribeNatGatewaysResponse (Core.Maybe [Types.NatGateway])
dngrrsNatGateways = Lens.field @"natGateways"
{-# DEPRECATED dngrrsNatGateways "Use generic-lens or generic-optics with 'natGateways' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrrsNextToken :: Lens.Lens' DescribeNatGatewaysResponse (Core.Maybe Types.NextToken)
dngrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dngrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrrsResponseStatus :: Lens.Lens' DescribeNatGatewaysResponse Core.Int
dngrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dngrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
