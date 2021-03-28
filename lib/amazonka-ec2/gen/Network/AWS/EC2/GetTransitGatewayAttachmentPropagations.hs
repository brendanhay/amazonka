{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the route tables to which the specified resource attachment propagates routes.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
    (
    -- * Creating a request
      GetTransitGatewayAttachmentPropagations (..)
    , mkGetTransitGatewayAttachmentPropagations
    -- ** Request lenses
    , gtgapTransitGatewayAttachmentId
    , gtgapDryRun
    , gtgapFilters
    , gtgapMaxResults
    , gtgapNextToken

    -- * Destructuring the response
    , GetTransitGatewayAttachmentPropagationsResponse (..)
    , mkGetTransitGatewayAttachmentPropagationsResponse
    -- ** Response lenses
    , gtgaprrsNextToken
    , gtgaprrsTransitGatewayAttachmentPropagations
    , gtgaprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTransitGatewayAttachmentPropagations' smart constructor.
data GetTransitGatewayAttachmentPropagations = GetTransitGatewayAttachmentPropagations'
  { transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
--
--
--     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayAttachmentPropagations' value with any optional fields omitted.
mkGetTransitGatewayAttachmentPropagations
    :: Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> GetTransitGatewayAttachmentPropagations
mkGetTransitGatewayAttachmentPropagations
  transitGatewayAttachmentId
  = GetTransitGatewayAttachmentPropagations'{transitGatewayAttachmentId,
                                             dryRun = Core.Nothing, filters = Core.Nothing,
                                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapTransitGatewayAttachmentId :: Lens.Lens' GetTransitGatewayAttachmentPropagations Types.TransitGatewayAttachmentId
gtgapTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE gtgapTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapDryRun :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Core.Bool)
gtgapDryRun = Lens.field @"dryRun"
{-# INLINEABLE gtgapDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. The possible values are:
--
--
--     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapFilters :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe [Types.Filter])
gtgapFilters = Lens.field @"filters"
{-# INLINEABLE gtgapFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapMaxResults :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Core.Natural)
gtgapMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gtgapMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapNextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Core.Text)
gtgapNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgapNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetTransitGatewayAttachmentPropagations where
        toQuery GetTransitGatewayAttachmentPropagations{..}
          = Core.toQueryPair "Action"
              ("GetTransitGatewayAttachmentPropagations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders GetTransitGatewayAttachmentPropagations
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetTransitGatewayAttachmentPropagations
         where
        type Rs GetTransitGatewayAttachmentPropagations =
             GetTransitGatewayAttachmentPropagationsResponse
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
                 GetTransitGatewayAttachmentPropagationsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "transitGatewayAttachmentPropagations" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetTransitGatewayAttachmentPropagations
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"transitGatewayAttachmentPropagations" Core..
                   Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTransitGatewayAttachmentPropagationsResponse' smart constructor.
data GetTransitGatewayAttachmentPropagationsResponse = GetTransitGatewayAttachmentPropagationsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , transitGatewayAttachmentPropagations :: Core.Maybe [Types.TransitGatewayAttachmentPropagation]
    -- ^ Information about the propagation route tables.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayAttachmentPropagationsResponse' value with any optional fields omitted.
mkGetTransitGatewayAttachmentPropagationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTransitGatewayAttachmentPropagationsResponse
mkGetTransitGatewayAttachmentPropagationsResponse responseStatus
  = GetTransitGatewayAttachmentPropagationsResponse'{nextToken =
                                                       Core.Nothing,
                                                     transitGatewayAttachmentPropagations =
                                                       Core.Nothing,
                                                     responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprrsNextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Core.Maybe Core.Text)
gtgaprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtgaprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the propagation route tables.
--
-- /Note:/ Consider using 'transitGatewayAttachmentPropagations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprrsTransitGatewayAttachmentPropagations :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Core.Maybe [Types.TransitGatewayAttachmentPropagation])
gtgaprrsTransitGatewayAttachmentPropagations = Lens.field @"transitGatewayAttachmentPropagations"
{-# INLINEABLE gtgaprrsTransitGatewayAttachmentPropagations #-}
{-# DEPRECATED transitGatewayAttachmentPropagations "Use generic-lens or generic-optics with 'transitGatewayAttachmentPropagations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprrsResponseStatus :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse Core.Int
gtgaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtgaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
