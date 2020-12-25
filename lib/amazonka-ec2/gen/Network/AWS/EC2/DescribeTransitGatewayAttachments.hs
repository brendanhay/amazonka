{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeTransitGatewayAttachments (..),
    mkDescribeTransitGatewayAttachments,

    -- ** Request lenses
    dtgaDryRun,
    dtgaFilters,
    dtgaMaxResults,
    dtgaNextToken,
    dtgaTransitGatewayAttachmentIds,

    -- * Destructuring the response
    DescribeTransitGatewayAttachmentsResponse (..),
    mkDescribeTransitGatewayAttachmentsResponse,

    -- ** Response lenses
    dtgarrsNextToken,
    dtgarrsTransitGatewayAttachments,
    dtgarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTransitGatewayAttachments' smart constructor.
data DescribeTransitGatewayAttachments = DescribeTransitGatewayAttachments'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | The IDs of the attachments.
    transitGatewayAttachmentIds :: Core.Maybe [Types.TransitGatewayAttachmentId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTransitGatewayAttachments' value with any optional fields omitted.
mkDescribeTransitGatewayAttachments ::
  DescribeTransitGatewayAttachments
mkDescribeTransitGatewayAttachments =
  DescribeTransitGatewayAttachments'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      transitGatewayAttachmentIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaDryRun :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe Core.Bool)
dtgaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
{-# DEPRECATED dtgaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaMaxResults :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe Core.Natural)
dtgaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dtgaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaNextToken :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe Types.String)
dtgaNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtgaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaTransitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayAttachments (Core.Maybe [Types.TransitGatewayAttachmentId])
dtgaTransitGatewayAttachmentIds = Lens.field @"transitGatewayAttachmentIds"
{-# DEPRECATED dtgaTransitGatewayAttachmentIds "Use generic-lens or generic-optics with 'transitGatewayAttachmentIds' instead." #-}

instance Core.AWSRequest DescribeTransitGatewayAttachments where
  type
    Rs DescribeTransitGatewayAttachments =
      DescribeTransitGatewayAttachmentsResponse
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
            ( Core.pure ("Action", "DescribeTransitGatewayAttachments")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryList "TransitGatewayAttachmentIds"
                            Core.<$> transitGatewayAttachmentIds
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayAttachmentsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "transitGatewayAttachments"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTransitGatewayAttachments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"transitGatewayAttachments" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeTransitGatewayAttachmentsResponse' smart constructor.
data DescribeTransitGatewayAttachmentsResponse = DescribeTransitGatewayAttachmentsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the attachments.
    transitGatewayAttachments :: Core.Maybe [Types.TransitGatewayAttachment],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTransitGatewayAttachmentsResponse' value with any optional fields omitted.
mkDescribeTransitGatewayAttachmentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTransitGatewayAttachmentsResponse
mkDescribeTransitGatewayAttachmentsResponse responseStatus =
  DescribeTransitGatewayAttachmentsResponse'
    { nextToken =
        Core.Nothing,
      transitGatewayAttachments = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarrsNextToken :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse (Core.Maybe Types.String)
dtgarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtgarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarrsTransitGatewayAttachments :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse (Core.Maybe [Types.TransitGatewayAttachment])
dtgarrsTransitGatewayAttachments = Lens.field @"transitGatewayAttachments"
{-# DEPRECATED dtgarrsTransitGatewayAttachments "Use generic-lens or generic-optics with 'transitGatewayAttachments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarrsResponseStatus :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse Core.Int
dtgarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
