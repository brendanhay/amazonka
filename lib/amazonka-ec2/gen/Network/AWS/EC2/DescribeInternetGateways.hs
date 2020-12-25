{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your internet gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInternetGateways
  ( -- * Creating a request
    DescribeInternetGateways (..),
    mkDescribeInternetGateways,

    -- ** Request lenses
    dDryRun,
    dFilters,
    dInternetGatewayIds,
    dMaxResults,
    dNextToken,

    -- * Destructuring the response
    DescribeInternetGatewaysResponse (..),
    mkDescribeInternetGatewaysResponse,

    -- ** Response lenses
    digrrsInternetGateways,
    digrrsNextToken,
    digrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInternetGateways' smart constructor.
data DescribeInternetGateways = DescribeInternetGateways'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.
    --
    --
    --     * @attachment.vpc-id@ - The ID of an attached VPC.
    --
    --
    --     * @internet-gateway-id@ - The ID of the Internet gateway.
    --
    --
    --     * @owner-id@ - The ID of the AWS account that owns the internet gateway.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filters :: Core.Maybe [Types.Filter],
    -- | One or more internet gateway IDs.
    --
    -- Default: Describes all your internet gateways.
    internetGatewayIds :: Core.Maybe [Types.InternetGatewayId],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInternetGateways' value with any optional fields omitted.
mkDescribeInternetGateways ::
  DescribeInternetGateways
mkDescribeInternetGateways =
  DescribeInternetGateways'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      internetGatewayIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDryRun :: Lens.Lens' DescribeInternetGateways (Core.Maybe Core.Bool)
dDryRun = Lens.field @"dryRun"
{-# DEPRECATED dDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.
--
--
--     * @attachment.vpc-id@ - The ID of an attached VPC.
--
--
--     * @internet-gateway-id@ - The ID of the Internet gateway.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the internet gateway.
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
dFilters :: Lens.Lens' DescribeInternetGateways (Core.Maybe [Types.Filter])
dFilters = Lens.field @"filters"
{-# DEPRECATED dFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
--
-- /Note:/ Consider using 'internetGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInternetGatewayIds :: Lens.Lens' DescribeInternetGateways (Core.Maybe [Types.InternetGatewayId])
dInternetGatewayIds = Lens.field @"internetGatewayIds"
{-# DEPRECATED dInternetGatewayIds "Use generic-lens or generic-optics with 'internetGatewayIds' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeInternetGateways (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeInternetGateways (Core.Maybe Types.NextToken)
dNextToken = Lens.field @"nextToken"
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeInternetGateways where
  type Rs DescribeInternetGateways = DescribeInternetGatewaysResponse
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
            ( Core.pure ("Action", "DescribeInternetGateways")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "InternetGatewayId" Core.<$> internetGatewayIds)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInternetGatewaysResponse'
            Core.<$> ( x Core..@? "internetGatewaySet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeInternetGateways where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"internetGateways" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeInternetGatewaysResponse' smart constructor.
data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse'
  { -- | Information about one or more internet gateways.
    internetGateways :: Core.Maybe [Types.InternetGateway],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInternetGatewaysResponse' value with any optional fields omitted.
mkDescribeInternetGatewaysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInternetGatewaysResponse
mkDescribeInternetGatewaysResponse responseStatus =
  DescribeInternetGatewaysResponse'
    { internetGateways =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about one or more internet gateways.
--
-- /Note:/ Consider using 'internetGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrrsInternetGateways :: Lens.Lens' DescribeInternetGatewaysResponse (Core.Maybe [Types.InternetGateway])
digrrsInternetGateways = Lens.field @"internetGateways"
{-# DEPRECATED digrrsInternetGateways "Use generic-lens or generic-optics with 'internetGateways' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrrsNextToken :: Lens.Lens' DescribeInternetGatewaysResponse (Core.Maybe Types.NextToken)
digrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED digrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrrsResponseStatus :: Lens.Lens' DescribeInternetGatewaysResponse Core.Int
digrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED digrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
