{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTransitGatewayAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more attachments between resources and transit gateways. By default, all attachments are described. Alternatively, you can filter the results by attachment ID, attachment state, resource ID, or resource owner.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayAttachments
    (
    -- * Creating a request
      DescribeTransitGatewayAttachments (..)
    , mkDescribeTransitGatewayAttachments
    -- ** Request lenses
    , dtgaDryRun
    , dtgaFilters
    , dtgaMaxResults
    , dtgaNextToken
    , dtgaTransitGatewayAttachmentIds

    -- * Destructuring the response
    , DescribeTransitGatewayAttachmentsResponse (..)
    , mkDescribeTransitGatewayAttachmentsResponse
    -- ** Response lenses
    , dtgarrsNextToken
    , dtgarrsTransitGatewayAttachments
    , dtgarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTransitGatewayAttachments' smart constructor.
data DescribeTransitGatewayAttachments = DescribeTransitGatewayAttachments'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
--
--
--     * @association.state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).
--
--
--     * @association.transit-gateway-route-table-id@ - The ID of the route table for the transit gateway.
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-owner-id@ - The ID of the AWS account that owns the resource.
--
--
--     * @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @state@ - The state of the attachment. Valid values are @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ | @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ .
--
--
--     * @transit-gateway-attachment-id@ - The ID of the attachment.
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--     * @transit-gateway-owner-id@ - The ID of the AWS account that owns the transit gateway.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , transitGatewayAttachmentIds :: Core.Maybe [Types.TransitGatewayAttachmentId]
    -- ^ The IDs of the attachments.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTransitGatewayAttachments' value with any optional fields omitted.
mkDescribeTransitGatewayAttachments
    :: DescribeTransitGatewayAttachments
mkDescribeTransitGatewayAttachments
  = DescribeTransitGatewayAttachments'{dryRun = Core.Nothing,
                                       filters = Core.Nothing, maxResults = Core.Nothing,
                                       nextToken = Core.Nothing,
                                       transitGatewayAttachmentIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaDryRun :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe Core.Bool)
dtgaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. The possible values are:
--
--
--     * @association.state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).
--
--
--     * @association.transit-gateway-route-table-id@ - The ID of the route table for the transit gateway.
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-owner-id@ - The ID of the AWS account that owns the resource.
--
--
--     * @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .
--
--
--     * @state@ - The state of the attachment. Valid values are @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ | @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ .
--
--
--     * @transit-gateway-attachment-id@ - The ID of the attachment.
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--     * @transit-gateway-owner-id@ - The ID of the AWS account that owns the transit gateway.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaFilters :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe [Types.Filter])
dtgaFilters = Lens.field @"filters"
{-# INLINEABLE dtgaFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaMaxResults :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe Core.Natural)
dtgaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dtgaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaNextToken :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe Core.Text)
dtgaNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtgaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The IDs of the attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaTransitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe [Types.TransitGatewayAttachmentId])
dtgaTransitGatewayAttachmentIds = Lens.field @"transitGatewayAttachmentIds"
{-# INLINEABLE dtgaTransitGatewayAttachmentIds #-}
{-# DEPRECATED transitGatewayAttachmentIds "Use generic-lens or generic-optics with 'transitGatewayAttachmentIds' instead"  #-}

instance Core.ToQuery DescribeTransitGatewayAttachments where
        toQuery DescribeTransitGatewayAttachments{..}
          = Core.toQueryPair "Action"
              ("DescribeTransitGatewayAttachments" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "TransitGatewayAttachmentIds")
                transitGatewayAttachmentIds

instance Core.ToHeaders DescribeTransitGatewayAttachments where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeTransitGatewayAttachments where
        type Rs DescribeTransitGatewayAttachments =
             DescribeTransitGatewayAttachmentsResponse
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
                 DescribeTransitGatewayAttachmentsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "transitGatewayAttachments" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTransitGatewayAttachments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"transitGatewayAttachments" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeTransitGatewayAttachmentsResponse' smart constructor.
data DescribeTransitGatewayAttachmentsResponse = DescribeTransitGatewayAttachmentsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , transitGatewayAttachments :: Core.Maybe [Types.TransitGatewayAttachment]
    -- ^ Information about the attachments.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTransitGatewayAttachmentsResponse' value with any optional fields omitted.
mkDescribeTransitGatewayAttachmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTransitGatewayAttachmentsResponse
mkDescribeTransitGatewayAttachmentsResponse responseStatus
  = DescribeTransitGatewayAttachmentsResponse'{nextToken =
                                                 Core.Nothing,
                                               transitGatewayAttachments = Core.Nothing,
                                               responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarrsNextToken :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse (Core.Maybe Core.Text)
dtgarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtgarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarrsTransitGatewayAttachments :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse (Core.Maybe [Types.TransitGatewayAttachment])
dtgarrsTransitGatewayAttachments = Lens.field @"transitGatewayAttachments"
{-# INLINEABLE dtgarrsTransitGatewayAttachments #-}
{-# DEPRECATED transitGatewayAttachments "Use generic-lens or generic-optics with 'transitGatewayAttachments' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarrsResponseStatus :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse Core.Int
dtgarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
