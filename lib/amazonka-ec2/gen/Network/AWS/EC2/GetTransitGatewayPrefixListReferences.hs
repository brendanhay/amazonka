{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayPrefixListReferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the prefix list references in a specified transit gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayPrefixListReferences
  ( -- * Creating a request
    GetTransitGatewayPrefixListReferences (..),
    mkGetTransitGatewayPrefixListReferences,

    -- ** Request lenses
    gtgplrTransitGatewayRouteTableId,
    gtgplrDryRun,
    gtgplrFilters,
    gtgplrMaxResults,
    gtgplrNextToken,

    -- * Destructuring the response
    GetTransitGatewayPrefixListReferencesResponse (..),
    mkGetTransitGatewayPrefixListReferencesResponse,

    -- ** Response lenses
    gtgplrrrsNextToken,
    gtgplrrrsTransitGatewayPrefixListReferences,
    gtgplrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTransitGatewayPrefixListReferences' smart constructor.
data GetTransitGatewayPrefixListReferences = GetTransitGatewayPrefixListReferences'
  { -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @attachment.resource-id@ - The ID of the resource for the attachment.
    --
    --
    --     * @attachment.resource-type@ - The type of resource for the attachment. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
    --
    --
    --     * @attachment.transit-gateway-attachment-id@ - The ID of the attachment.
    --
    --
    --     * @is-blackhole@ - Whether traffic matching the route is blocked (@true@ | @false@ ).
    --
    --
    --     * @prefix-list-id@ - The ID of the prefix list.
    --
    --
    --     * @prefix-list-owner-id@ - The ID of the owner of the prefix list.
    --
    --
    --     * @state@ - The state of the prefix list reference (@pending@ | @available@ | @modifying@ | @deleting@ ).
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayPrefixListReferences' value with any optional fields omitted.
mkGetTransitGatewayPrefixListReferences ::
  -- | 'transitGatewayRouteTableId'
  Types.TransitGatewayRouteTableId ->
  GetTransitGatewayPrefixListReferences
mkGetTransitGatewayPrefixListReferences transitGatewayRouteTableId =
  GetTransitGatewayPrefixListReferences'
    { transitGatewayRouteTableId,
      dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrTransitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayPrefixListReferences Types.TransitGatewayRouteTableId
gtgplrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# DEPRECATED gtgplrTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrDryRun :: Lens.Lens' GetTransitGatewayPrefixListReferences (Core.Maybe Core.Bool)
gtgplrDryRun = Lens.field @"dryRun"
{-# DEPRECATED gtgplrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @attachment.resource-id@ - The ID of the resource for the attachment.
--
--
--     * @attachment.resource-type@ - The type of resource for the attachment. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @attachment.transit-gateway-attachment-id@ - The ID of the attachment.
--
--
--     * @is-blackhole@ - Whether traffic matching the route is blocked (@true@ | @false@ ).
--
--
--     * @prefix-list-id@ - The ID of the prefix list.
--
--
--     * @prefix-list-owner-id@ - The ID of the owner of the prefix list.
--
--
--     * @state@ - The state of the prefix list reference (@pending@ | @available@ | @modifying@ | @deleting@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrFilters :: Lens.Lens' GetTransitGatewayPrefixListReferences (Core.Maybe [Types.Filter])
gtgplrFilters = Lens.field @"filters"
{-# DEPRECATED gtgplrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrMaxResults :: Lens.Lens' GetTransitGatewayPrefixListReferences (Core.Maybe Core.Natural)
gtgplrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gtgplrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrNextToken :: Lens.Lens' GetTransitGatewayPrefixListReferences (Core.Maybe Types.String)
gtgplrNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtgplrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetTransitGatewayPrefixListReferences where
  type
    Rs GetTransitGatewayPrefixListReferences =
      GetTransitGatewayPrefixListReferencesResponse
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
            ( Core.pure ("Action", "GetTransitGatewayPrefixListReferences")
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
          GetTransitGatewayPrefixListReferencesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "transitGatewayPrefixListReferenceSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetTransitGatewayPrefixListReferences where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"transitGatewayPrefixListReferences" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetTransitGatewayPrefixListReferencesResponse' smart constructor.
data GetTransitGatewayPrefixListReferencesResponse = GetTransitGatewayPrefixListReferencesResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the prefix list references.
    transitGatewayPrefixListReferences :: Core.Maybe [Types.TransitGatewayPrefixListReference],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayPrefixListReferencesResponse' value with any optional fields omitted.
mkGetTransitGatewayPrefixListReferencesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTransitGatewayPrefixListReferencesResponse
mkGetTransitGatewayPrefixListReferencesResponse responseStatus =
  GetTransitGatewayPrefixListReferencesResponse'
    { nextToken =
        Core.Nothing,
      transitGatewayPrefixListReferences =
        Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrrsNextToken :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse (Core.Maybe Types.String)
gtgplrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtgplrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the prefix list references.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrrsTransitGatewayPrefixListReferences :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse (Core.Maybe [Types.TransitGatewayPrefixListReference])
gtgplrrrsTransitGatewayPrefixListReferences = Lens.field @"transitGatewayPrefixListReferences"
{-# DEPRECATED gtgplrrrsTransitGatewayPrefixListReferences "Use generic-lens or generic-optics with 'transitGatewayPrefixListReferences' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrrsResponseStatus :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse Core.Int
gtgplrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtgplrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
