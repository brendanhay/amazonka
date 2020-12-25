{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the specified transit gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
  ( -- * Creating a request
    GetTransitGatewayRouteTableAssociations (..),
    mkGetTransitGatewayRouteTableAssociations,

    -- ** Request lenses
    gtgrtaTransitGatewayRouteTableId,
    gtgrtaDryRun,
    gtgrtaFilters,
    gtgrtaMaxResults,
    gtgrtaNextToken,

    -- * Destructuring the response
    GetTransitGatewayRouteTableAssociationsResponse (..),
    mkGetTransitGatewayRouteTableAssociationsResponse,

    -- ** Response lenses
    gtgrtarrsAssociations,
    gtgrtarrsNextToken,
    gtgrtarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTransitGatewayRouteTableAssociations' smart constructor.
data GetTransitGatewayRouteTableAssociations = GetTransitGatewayRouteTableAssociations'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @resource-id@ - The ID of the resource.
    --
    --
    --     * @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
    --
    --
    --     * @transit-gateway-attachment-id@ - The ID of the attachment.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayRouteTableAssociations' value with any optional fields omitted.
mkGetTransitGatewayRouteTableAssociations ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  GetTransitGatewayRouteTableAssociations
mkGetTransitGatewayRouteTableAssociations
  transitGatewayRouteTableId =
    GetTransitGatewayRouteTableAssociations'
      { transitGatewayRouteTableId,
        dryRun = Core.Nothing,
        filters = Core.Nothing,
        maxResults = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaTransitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayRouteTableAssociations Types.TransitGatewayRouteTableId
gtgrtaTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED gtgrtaTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaDryRun :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Core.Maybe Core.Bool)
gtgrtaDryRun = Lens.field @"dryRun"
{-# DEPRECATED gtgrtaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @transit-gateway-attachment-id@ - The ID of the attachment.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaFilters :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Core.Maybe [Types.Filter])
gtgrtaFilters = Lens.field @"filters"
{-# DEPRECATED gtgrtaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaMaxResults :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Core.Maybe Core.Natural)
gtgrtaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gtgrtaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtaNextToken :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Core.Maybe Types.NextToken)
gtgrtaNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtgrtaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetTransitGatewayRouteTableAssociations where
  type
    Rs GetTransitGatewayRouteTableAssociations =
      GetTransitGatewayRouteTableAssociationsResponse
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
            ( Core.pure ("Action", "GetTransitGatewayRouteTableAssociations")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayRouteTableId"
                            transitGatewayRouteTableId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayRouteTableAssociationsResponse'
            Core.<$> (x Core..@? "associations" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetTransitGatewayRouteTableAssociations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"associations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetTransitGatewayRouteTableAssociationsResponse' smart constructor.
data GetTransitGatewayRouteTableAssociationsResponse = GetTransitGatewayRouteTableAssociationsResponse'
  { -- | Information about the associations.
    associations :: Core.Maybe [Types.TransitGatewayRouteTableAssociation],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayRouteTableAssociationsResponse' value with any optional fields omitted.
mkGetTransitGatewayRouteTableAssociationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTransitGatewayRouteTableAssociationsResponse
mkGetTransitGatewayRouteTableAssociationsResponse responseStatus =
  GetTransitGatewayRouteTableAssociationsResponse'
    { associations =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the associations.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtarrsAssociations :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse (Core.Maybe [Types.TransitGatewayRouteTableAssociation])
gtgrtarrsAssociations = Lens.field @"associations"
{-# DEPRECATED gtgrtarrsAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtarrsNextToken :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse (Core.Maybe Types.String)
gtgrtarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtgrtarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtarrsResponseStatus :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse Core.Int
gtgrtarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtgrtarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
