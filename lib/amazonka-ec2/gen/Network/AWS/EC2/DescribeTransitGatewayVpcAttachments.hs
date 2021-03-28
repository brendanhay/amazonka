{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTransitGatewayVpcAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more VPC attachments. By default, all VPC attachments are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayVpcAttachments
    (
    -- * Creating a request
      DescribeTransitGatewayVpcAttachments (..)
    , mkDescribeTransitGatewayVpcAttachments
    -- ** Request lenses
    , dtgvasDryRun
    , dtgvasFilters
    , dtgvasMaxResults
    , dtgvasNextToken
    , dtgvasTransitGatewayAttachmentIds

    -- * Destructuring the response
    , DescribeTransitGatewayVpcAttachmentsResponse (..)
    , mkDescribeTransitGatewayVpcAttachmentsResponse
    -- ** Response lenses
    , dtgvarfrsNextToken
    , dtgvarfrsTransitGatewayVpcAttachments
    , dtgvarfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTransitGatewayVpcAttachments' smart constructor.
data DescribeTransitGatewayVpcAttachments = DescribeTransitGatewayVpcAttachments'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. The possible values are:
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
--     * @vpc-id@ - The ID of the VPC.
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

-- | Creates a 'DescribeTransitGatewayVpcAttachments' value with any optional fields omitted.
mkDescribeTransitGatewayVpcAttachments
    :: DescribeTransitGatewayVpcAttachments
mkDescribeTransitGatewayVpcAttachments
  = DescribeTransitGatewayVpcAttachments'{dryRun = Core.Nothing,
                                          filters = Core.Nothing, maxResults = Core.Nothing,
                                          nextToken = Core.Nothing,
                                          transitGatewayAttachmentIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvasDryRun :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Core.Maybe Core.Bool)
dtgvasDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgvasDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. The possible values are:
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
--     * @vpc-id@ - The ID of the VPC.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvasFilters :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Core.Maybe [Types.Filter])
dtgvasFilters = Lens.field @"filters"
{-# INLINEABLE dtgvasFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvasMaxResults :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Core.Maybe Core.Natural)
dtgvasMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dtgvasMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvasNextToken :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Core.Maybe Core.Text)
dtgvasNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtgvasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The IDs of the attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvasTransitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayVpcAttachments (Core.Maybe [Types.TransitGatewayAttachmentId])
dtgvasTransitGatewayAttachmentIds = Lens.field @"transitGatewayAttachmentIds"
{-# INLINEABLE dtgvasTransitGatewayAttachmentIds #-}
{-# DEPRECATED transitGatewayAttachmentIds "Use generic-lens or generic-optics with 'transitGatewayAttachmentIds' instead"  #-}

instance Core.ToQuery DescribeTransitGatewayVpcAttachments where
        toQuery DescribeTransitGatewayVpcAttachments{..}
          = Core.toQueryPair "Action"
              ("DescribeTransitGatewayVpcAttachments" :: Core.Text)
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

instance Core.ToHeaders DescribeTransitGatewayVpcAttachments where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeTransitGatewayVpcAttachments where
        type Rs DescribeTransitGatewayVpcAttachments =
             DescribeTransitGatewayVpcAttachmentsResponse
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
                 DescribeTransitGatewayVpcAttachmentsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "transitGatewayVpcAttachments" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeTransitGatewayVpcAttachments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"transitGatewayVpcAttachments" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeTransitGatewayVpcAttachmentsResponse' smart constructor.
data DescribeTransitGatewayVpcAttachmentsResponse = DescribeTransitGatewayVpcAttachmentsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , transitGatewayVpcAttachments :: Core.Maybe [Types.TransitGatewayVpcAttachment]
    -- ^ Information about the VPC attachments.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeTransitGatewayVpcAttachmentsResponse' value with any optional fields omitted.
mkDescribeTransitGatewayVpcAttachmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTransitGatewayVpcAttachmentsResponse
mkDescribeTransitGatewayVpcAttachmentsResponse responseStatus
  = DescribeTransitGatewayVpcAttachmentsResponse'{nextToken =
                                                    Core.Nothing,
                                                  transitGatewayVpcAttachments = Core.Nothing,
                                                  responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvarfrsNextToken :: Lens.Lens' DescribeTransitGatewayVpcAttachmentsResponse (Core.Maybe Core.Text)
dtgvarfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dtgvarfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the VPC attachments.
--
-- /Note:/ Consider using 'transitGatewayVpcAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvarfrsTransitGatewayVpcAttachments :: Lens.Lens' DescribeTransitGatewayVpcAttachmentsResponse (Core.Maybe [Types.TransitGatewayVpcAttachment])
dtgvarfrsTransitGatewayVpcAttachments = Lens.field @"transitGatewayVpcAttachments"
{-# INLINEABLE dtgvarfrsTransitGatewayVpcAttachments #-}
{-# DEPRECATED transitGatewayVpcAttachments "Use generic-lens or generic-optics with 'transitGatewayVpcAttachments' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvarfrsResponseStatus :: Lens.Lens' DescribeTransitGatewayVpcAttachmentsResponse Core.Int
dtgvarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgvarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
