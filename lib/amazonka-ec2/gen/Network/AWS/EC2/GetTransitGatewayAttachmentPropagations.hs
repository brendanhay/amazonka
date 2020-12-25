{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetTransitGatewayAttachmentPropagations (..),
    mkGetTransitGatewayAttachmentPropagations,

    -- ** Request lenses
    gtgapTransitGatewayAttachmentId,
    gtgapDryRun,
    gtgapFilters,
    gtgapMaxResults,
    gtgapNextToken,

    -- * Destructuring the response
    GetTransitGatewayAttachmentPropagationsResponse (..),
    mkGetTransitGatewayAttachmentPropagationsResponse,

    -- ** Response lenses
    gtgaprrsNextToken,
    gtgaprrsTransitGatewayAttachmentPropagations,
    gtgaprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTransitGatewayAttachmentPropagations' smart constructor.
data GetTransitGatewayAttachmentPropagations = GetTransitGatewayAttachmentPropagations'
  { -- | The ID of the attachment.
    transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayAttachmentPropagations' value with any optional fields omitted.
mkGetTransitGatewayAttachmentPropagations ::
  -- | 'transitGatewayAttachmentId'
  Types.TransitGatewayAttachmentId ->
  GetTransitGatewayAttachmentPropagations
mkGetTransitGatewayAttachmentPropagations
  transitGatewayAttachmentId =
    GetTransitGatewayAttachmentPropagations'
      { transitGatewayAttachmentId,
        dryRun = Core.Nothing,
        filters = Core.Nothing,
        maxResults = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapTransitGatewayAttachmentId :: Lens.Lens' GetTransitGatewayAttachmentPropagations Types.TransitGatewayAttachmentId
gtgapTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED gtgapTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapDryRun :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Core.Bool)
gtgapDryRun = Lens.field @"dryRun"
{-# DEPRECATED gtgapDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
{-# DEPRECATED gtgapFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapMaxResults :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Core.Natural)
gtgapMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gtgapMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapNextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Types.NextToken)
gtgapNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtgapNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetTransitGatewayAttachmentPropagations where
  type
    Rs GetTransitGatewayAttachmentPropagations =
      GetTransitGatewayAttachmentPropagationsResponse
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
            ( Core.pure ("Action", "GetTransitGatewayAttachmentPropagations")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "TransitGatewayAttachmentId"
                            transitGatewayAttachmentId
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
          GetTransitGatewayAttachmentPropagationsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "transitGatewayAttachmentPropagations"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetTransitGatewayAttachmentPropagations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"transitGatewayAttachmentPropagations"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetTransitGatewayAttachmentPropagationsResponse' smart constructor.
data GetTransitGatewayAttachmentPropagationsResponse = GetTransitGatewayAttachmentPropagationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the propagation route tables.
    transitGatewayAttachmentPropagations :: Core.Maybe [Types.TransitGatewayAttachmentPropagation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTransitGatewayAttachmentPropagationsResponse' value with any optional fields omitted.
mkGetTransitGatewayAttachmentPropagationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTransitGatewayAttachmentPropagationsResponse
mkGetTransitGatewayAttachmentPropagationsResponse responseStatus =
  GetTransitGatewayAttachmentPropagationsResponse'
    { nextToken =
        Core.Nothing,
      transitGatewayAttachmentPropagations =
        Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprrsNextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Core.Maybe Types.NextToken)
gtgaprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtgaprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the propagation route tables.
--
-- /Note:/ Consider using 'transitGatewayAttachmentPropagations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprrsTransitGatewayAttachmentPropagations :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Core.Maybe [Types.TransitGatewayAttachmentPropagation])
gtgaprrsTransitGatewayAttachmentPropagations = Lens.field @"transitGatewayAttachmentPropagations"
{-# DEPRECATED gtgaprrsTransitGatewayAttachmentPropagations "Use generic-lens or generic-optics with 'transitGatewayAttachmentPropagations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprrsResponseStatus :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse Core.Int
gtgaprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtgaprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
