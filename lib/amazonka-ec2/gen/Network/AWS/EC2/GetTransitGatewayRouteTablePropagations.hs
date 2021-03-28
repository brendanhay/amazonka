{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the route table propagations for the specified transit gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
    (
    -- * Creating a request
      GetTransitGatewayRouteTablePropagations (..)
    , mkGetTransitGatewayRouteTablePropagations
    -- ** Request lenses
    , gtgrtpTransitGatewayRouteTableId
    , gtgrtpDryRun
    , gtgrtpFilters
    , gtgrtpMaxResults
    , gtgrtpNextToken

    -- * Destructuring the response
    , GetTransitGatewayRouteTablePropagationsResponse (..)
    , mkGetTransitGatewayRouteTablePropagationsResponse
    -- ** Response lenses
    , gtgrtprrsNextToken
    , gtgrtprrsTransitGatewayRouteTablePropagations
    , gtgrtprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTransitGatewayRouteTablePropagations' smart constructor.
data GetTransitGatewayRouteTablePropagations = GetTransitGatewayRouteTablePropagations'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the transit gateway route table.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayRouteTablePropagations' value with any optional fields omitted.
mkGetTransitGatewayRouteTablePropagations
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> GetTransitGatewayRouteTablePropagations
mkGetTransitGatewayRouteTablePropagations
  transitGatewayRouteTableId
  = GetTransitGatewayRouteTablePropagations'{transitGatewayRouteTableId,
                                             dryRun = Core.Nothing, filters = Core.Nothing,
                                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpTransitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayRouteTablePropagations Types.TransitGatewayRouteTableId
gtgrtpTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE gtgrtpTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpDryRun :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Core.Maybe Core.Bool)
gtgrtpDryRun = Lens.field @"dryRun"
{-# INLINEABLE gtgrtpDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
gtgrtpFilters :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Core.Maybe [Types.Filter])
gtgrtpFilters = Lens.field @"filters"
{-# INLINEABLE gtgrtpFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpMaxResults :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Core.Maybe Core.Natural)
gtgrtpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gtgrtpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtpNextToken :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Core.Maybe Core.Text)
gtgrtpNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgrtpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetTransitGatewayRouteTablePropagations where
        toQuery GetTransitGatewayRouteTablePropagations{..}
          = Core.toQueryPair "Action"
              ("GetTransitGatewayRouteTablePropagations" :: Core.Text)
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

instance Core.ToHeaders GetTransitGatewayRouteTablePropagations
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetTransitGatewayRouteTablePropagations
         where
        type Rs GetTransitGatewayRouteTablePropagations =
             GetTransitGatewayRouteTablePropagationsResponse
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
                 GetTransitGatewayRouteTablePropagationsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "transitGatewayRouteTablePropagations" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetTransitGatewayRouteTablePropagations
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"transitGatewayRouteTablePropagations" Core..
                   Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTransitGatewayRouteTablePropagationsResponse' smart constructor.
data GetTransitGatewayRouteTablePropagationsResponse = GetTransitGatewayRouteTablePropagationsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , transitGatewayRouteTablePropagations :: Core.Maybe [Types.TransitGatewayRouteTablePropagation]
    -- ^ Information about the route table propagations.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayRouteTablePropagationsResponse' value with any optional fields omitted.
mkGetTransitGatewayRouteTablePropagationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTransitGatewayRouteTablePropagationsResponse
mkGetTransitGatewayRouteTablePropagationsResponse responseStatus
  = GetTransitGatewayRouteTablePropagationsResponse'{nextToken =
                                                       Core.Nothing,
                                                     transitGatewayRouteTablePropagations =
                                                       Core.Nothing,
                                                     responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtprrsNextToken :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse (Core.Maybe Core.Text)
gtgrtprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgrtprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the route table propagations.
--
-- /Note:/ Consider using 'transitGatewayRouteTablePropagations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtprrsTransitGatewayRouteTablePropagations :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse (Core.Maybe [Types.TransitGatewayRouteTablePropagation])
gtgrtprrsTransitGatewayRouteTablePropagations = Lens.field @"transitGatewayRouteTablePropagations"
{-# INLINEABLE gtgrtprrsTransitGatewayRouteTablePropagations #-}
{-# DEPRECATED transitGatewayRouteTablePropagations "Use generic-lens or generic-optics with 'transitGatewayRouteTablePropagations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgrtprrsResponseStatus :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse Core.Int
gtgrtprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtgrtprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
