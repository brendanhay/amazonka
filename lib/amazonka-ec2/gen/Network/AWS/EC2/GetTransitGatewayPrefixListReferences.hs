{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTransitGatewayPrefixListReferences (..)
    , mkGetTransitGatewayPrefixListReferences
    -- ** Request lenses
    , gtgplrTransitGatewayRouteTableId
    , gtgplrDryRun
    , gtgplrFilters
    , gtgplrMaxResults
    , gtgplrNextToken

    -- * Destructuring the response
    , GetTransitGatewayPrefixListReferencesResponse (..)
    , mkGetTransitGatewayPrefixListReferencesResponse
    -- ** Response lenses
    , gtgplrrrsNextToken
    , gtgplrrrsTransitGatewayPrefixListReferences
    , gtgplrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTransitGatewayPrefixListReferences' smart constructor.
data GetTransitGatewayPrefixListReferences = GetTransitGatewayPrefixListReferences'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the transit gateway route table.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayPrefixListReferences' value with any optional fields omitted.
mkGetTransitGatewayPrefixListReferences
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> GetTransitGatewayPrefixListReferences
mkGetTransitGatewayPrefixListReferences transitGatewayRouteTableId
  = GetTransitGatewayPrefixListReferences'{transitGatewayRouteTableId,
                                           dryRun = Core.Nothing, filters = Core.Nothing,
                                           maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrTransitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayPrefixListReferences Types.TransitGatewayRouteTableId
gtgplrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE gtgplrTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrDryRun :: Lens.Lens' GetTransitGatewayPrefixListReferences (Core.Maybe Core.Bool)
gtgplrDryRun = Lens.field @"dryRun"
{-# INLINEABLE gtgplrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE gtgplrFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrMaxResults :: Lens.Lens' GetTransitGatewayPrefixListReferences (Core.Maybe Core.Natural)
gtgplrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gtgplrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrNextToken :: Lens.Lens' GetTransitGatewayPrefixListReferences (Core.Maybe Core.Text)
gtgplrNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgplrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetTransitGatewayPrefixListReferences where
        toQuery GetTransitGatewayPrefixListReferences{..}
          = Core.toQueryPair "Action"
              ("GetTransitGatewayPrefixListReferences" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders GetTransitGatewayPrefixListReferences where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetTransitGatewayPrefixListReferences
         where
        type Rs GetTransitGatewayPrefixListReferences =
             GetTransitGatewayPrefixListReferencesResponse
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
                 GetTransitGatewayPrefixListReferencesResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "transitGatewayPrefixListReferenceSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetTransitGatewayPrefixListReferences where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"transitGatewayPrefixListReferences" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTransitGatewayPrefixListReferencesResponse' smart constructor.
data GetTransitGatewayPrefixListReferencesResponse = GetTransitGatewayPrefixListReferencesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , transitGatewayPrefixListReferences :: Core.Maybe [Types.TransitGatewayPrefixListReference]
    -- ^ Information about the prefix list references.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayPrefixListReferencesResponse' value with any optional fields omitted.
mkGetTransitGatewayPrefixListReferencesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTransitGatewayPrefixListReferencesResponse
mkGetTransitGatewayPrefixListReferencesResponse responseStatus
  = GetTransitGatewayPrefixListReferencesResponse'{nextToken =
                                                     Core.Nothing,
                                                   transitGatewayPrefixListReferences =
                                                     Core.Nothing,
                                                   responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrrsNextToken :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse (Core.Maybe Core.Text)
gtgplrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgplrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the prefix list references.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrrsTransitGatewayPrefixListReferences :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse (Core.Maybe [Types.TransitGatewayPrefixListReference])
gtgplrrrsTransitGatewayPrefixListReferences = Lens.field @"transitGatewayPrefixListReferences"
{-# INLINEABLE gtgplrrrsTransitGatewayPrefixListReferences #-}
{-# DEPRECATED transitGatewayPrefixListReferences "Use generic-lens or generic-optics with 'transitGatewayPrefixListReferences' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgplrrrsResponseStatus :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse Core.Int
gtgplrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtgplrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
