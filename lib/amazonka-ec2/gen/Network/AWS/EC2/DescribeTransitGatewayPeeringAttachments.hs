{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your transit gateway peering attachments.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
  ( -- * Creating a request
    DescribeTransitGatewayPeeringAttachments (..),
    mkDescribeTransitGatewayPeeringAttachments,

    -- ** Request lenses
    dtgpaDryRun,
    dtgpaFilters,
    dtgpaMaxResults,
    dtgpaNextToken,
    dtgpaTransitGatewayAttachmentIds,

    -- * Destructuring the response
    DescribeTransitGatewayPeeringAttachmentsResponse (..),
    mkDescribeTransitGatewayPeeringAttachmentsResponse,

    -- ** Response lenses
    dtgparrsNextToken,
    dtgparrsTransitGatewayPeeringAttachments,
    dtgparrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTransitGatewayPeeringAttachments' smart constructor.
data DescribeTransitGatewayPeeringAttachments = DescribeTransitGatewayPeeringAttachments'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters. The possible values are:
    --
    --
    --     * @transit-gateway-attachment-id@ - The ID of the transit gateway attachment.
    --
    --
    --     * @local-owner-id@ - The ID of your AWS account.
    --
    --
    --     * @remote-owner-id@ - The ID of the AWS account in the remote Region that owns the transit gateway.
    --
    --
    --     * @state@ - The state of the peering attachment. Valid values are @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ | @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @transit-gateway-id@ - The ID of the transit gateway.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String,
    -- | One or more IDs of the transit gateway peering attachments.
    transitGatewayAttachmentIds :: Core.Maybe [Types.TransitGatewayAttachmentId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTransitGatewayPeeringAttachments' value with any optional fields omitted.
mkDescribeTransitGatewayPeeringAttachments ::
  DescribeTransitGatewayPeeringAttachments
mkDescribeTransitGatewayPeeringAttachments =
  DescribeTransitGatewayPeeringAttachments'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      transitGatewayAttachmentIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaDryRun :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe Core.Bool)
dtgpaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dtgpaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @transit-gateway-attachment-id@ - The ID of the transit gateway attachment.
--
--
--     * @local-owner-id@ - The ID of your AWS account.
--
--
--     * @remote-owner-id@ - The ID of the AWS account in the remote Region that owns the transit gateway.
--
--
--     * @state@ - The state of the peering attachment. Valid values are @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ | @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaFilters :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe [Types.Filter])
dtgpaFilters = Lens.field @"filters"
{-# DEPRECATED dtgpaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaMaxResults :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe Core.Natural)
dtgpaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dtgpaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaNextToken :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe Types.String)
dtgpaNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtgpaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more IDs of the transit gateway peering attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaTransitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Core.Maybe [Types.TransitGatewayAttachmentId])
dtgpaTransitGatewayAttachmentIds = Lens.field @"transitGatewayAttachmentIds"
{-# DEPRECATED dtgpaTransitGatewayAttachmentIds "Use generic-lens or generic-optics with 'transitGatewayAttachmentIds' instead." #-}

instance Core.AWSRequest DescribeTransitGatewayPeeringAttachments where
  type
    Rs DescribeTransitGatewayPeeringAttachments =
      DescribeTransitGatewayPeeringAttachmentsResponse
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
            ( Core.pure ("Action", "DescribeTransitGatewayPeeringAttachments")
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
          DescribeTransitGatewayPeeringAttachmentsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "transitGatewayPeeringAttachments"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeTransitGatewayPeeringAttachments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"transitGatewayPeeringAttachments" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeTransitGatewayPeeringAttachmentsResponse' smart constructor.
data DescribeTransitGatewayPeeringAttachmentsResponse = DescribeTransitGatewayPeeringAttachmentsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The transit gateway peering attachments.
    transitGatewayPeeringAttachments :: Core.Maybe [Types.TransitGatewayPeeringAttachment],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTransitGatewayPeeringAttachmentsResponse' value with any optional fields omitted.
mkDescribeTransitGatewayPeeringAttachmentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTransitGatewayPeeringAttachmentsResponse
mkDescribeTransitGatewayPeeringAttachmentsResponse responseStatus =
  DescribeTransitGatewayPeeringAttachmentsResponse'
    { nextToken =
        Core.Nothing,
      transitGatewayPeeringAttachments =
        Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparrsNextToken :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Core.Maybe Types.String)
dtgparrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dtgparrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The transit gateway peering attachments.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparrsTransitGatewayPeeringAttachments :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Core.Maybe [Types.TransitGatewayPeeringAttachment])
dtgparrsTransitGatewayPeeringAttachments = Lens.field @"transitGatewayPeeringAttachments"
{-# DEPRECATED dtgparrsTransitGatewayPeeringAttachments "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgparrsResponseStatus :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse Core.Int
dtgparrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
