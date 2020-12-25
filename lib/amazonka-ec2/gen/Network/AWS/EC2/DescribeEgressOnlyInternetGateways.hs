{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeEgressOnlyInternetGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your egress-only internet gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeEgressOnlyInternetGateways
  ( -- * Creating a request
    DescribeEgressOnlyInternetGateways (..),
    mkDescribeEgressOnlyInternetGateways,

    -- ** Request lenses
    deoigDryRun,
    deoigEgressOnlyInternetGatewayIds,
    deoigFilters,
    deoigMaxResults,
    deoigNextToken,

    -- * Destructuring the response
    DescribeEgressOnlyInternetGatewaysResponse (..),
    mkDescribeEgressOnlyInternetGatewaysResponse,

    -- ** Response lenses
    deoigrrsEgressOnlyInternetGateways,
    deoigrrsNextToken,
    deoigrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEgressOnlyInternetGateways' smart constructor.
data DescribeEgressOnlyInternetGateways = DescribeEgressOnlyInternetGateways'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more egress-only internet gateway IDs.
    egressOnlyInternetGatewayIds :: Core.Maybe [Types.EgressOnlyInternetGatewayId],
    -- | One or more filters.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEgressOnlyInternetGateways' value with any optional fields omitted.
mkDescribeEgressOnlyInternetGateways ::
  DescribeEgressOnlyInternetGateways
mkDescribeEgressOnlyInternetGateways =
  DescribeEgressOnlyInternetGateways'
    { dryRun = Core.Nothing,
      egressOnlyInternetGatewayIds = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigDryRun :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe Core.Bool)
deoigDryRun = Lens.field @"dryRun"
{-# DEPRECATED deoigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more egress-only internet gateway IDs.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigEgressOnlyInternetGatewayIds :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe [Types.EgressOnlyInternetGatewayId])
deoigEgressOnlyInternetGatewayIds = Lens.field @"egressOnlyInternetGatewayIds"
{-# DEPRECATED deoigEgressOnlyInternetGatewayIds "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayIds' instead." #-}

-- | One or more filters.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigFilters :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe [Types.Filter])
deoigFilters = Lens.field @"filters"
{-# DEPRECATED deoigFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigMaxResults :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe Core.Natural)
deoigMaxResults = Lens.field @"maxResults"
{-# DEPRECATED deoigMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigNextToken :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe Types.NextToken)
deoigNextToken = Lens.field @"nextToken"
{-# DEPRECATED deoigNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeEgressOnlyInternetGateways where
  type
    Rs DescribeEgressOnlyInternetGateways =
      DescribeEgressOnlyInternetGatewaysResponse
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
            ( Core.pure ("Action", "DescribeEgressOnlyInternetGateways")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> ( Core.toQueryList "EgressOnlyInternetGatewayId"
                            Core.<$> egressOnlyInternetGatewayIds
                        )
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeEgressOnlyInternetGatewaysResponse'
            Core.<$> ( x Core..@? "egressOnlyInternetGatewaySet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeEgressOnlyInternetGateways where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"egressOnlyInternetGateways" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeEgressOnlyInternetGatewaysResponse' smart constructor.
data DescribeEgressOnlyInternetGatewaysResponse = DescribeEgressOnlyInternetGatewaysResponse'
  { -- | Information about the egress-only internet gateways.
    egressOnlyInternetGateways :: Core.Maybe [Types.EgressOnlyInternetGateway],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEgressOnlyInternetGatewaysResponse' value with any optional fields omitted.
mkDescribeEgressOnlyInternetGatewaysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEgressOnlyInternetGatewaysResponse
mkDescribeEgressOnlyInternetGatewaysResponse responseStatus =
  DescribeEgressOnlyInternetGatewaysResponse'
    { egressOnlyInternetGateways =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the egress-only internet gateways.
--
-- /Note:/ Consider using 'egressOnlyInternetGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrrsEgressOnlyInternetGateways :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Core.Maybe [Types.EgressOnlyInternetGateway])
deoigrrsEgressOnlyInternetGateways = Lens.field @"egressOnlyInternetGateways"
{-# DEPRECATED deoigrrsEgressOnlyInternetGateways "Use generic-lens or generic-optics with 'egressOnlyInternetGateways' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrrsNextToken :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Core.Maybe Types.String)
deoigrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED deoigrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrrsResponseStatus :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse Core.Int
deoigrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED deoigrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
