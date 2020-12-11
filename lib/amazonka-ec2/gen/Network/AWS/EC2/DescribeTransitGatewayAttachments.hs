{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dtgaFilters,
    dtgaNextToken,
    dtgaTransitGatewayAttachmentIds,
    dtgaDryRun,
    dtgaMaxResults,

    -- * Destructuring the response
    DescribeTransitGatewayAttachmentsResponse (..),
    mkDescribeTransitGatewayAttachmentsResponse,

    -- ** Response lenses
    dtgarsNextToken,
    dtgarsTransitGatewayAttachments,
    dtgarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTransitGatewayAttachments' smart constructor.
data DescribeTransitGatewayAttachments = DescribeTransitGatewayAttachments'
  { filters ::
      Lude.Maybe [Filter],
    nextToken ::
      Lude.Maybe Lude.Text,
    transitGatewayAttachmentIds ::
      Lude.Maybe [Lude.Text],
    dryRun ::
      Lude.Maybe Lude.Bool,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayAttachments' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
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
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'transitGatewayAttachmentIds' - The IDs of the attachments.
mkDescribeTransitGatewayAttachments ::
  DescribeTransitGatewayAttachments
mkDescribeTransitGatewayAttachments =
  DescribeTransitGatewayAttachments'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      transitGatewayAttachmentIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

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
dtgaFilters :: Lens.Lens' DescribeTransitGatewayAttachments (Lude.Maybe [Filter])
dtgaFilters = Lens.lens (filters :: DescribeTransitGatewayAttachments -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTransitGatewayAttachments)
{-# DEPRECATED dtgaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaNextToken :: Lens.Lens' DescribeTransitGatewayAttachments (Lude.Maybe Lude.Text)
dtgaNextToken = Lens.lens (nextToken :: DescribeTransitGatewayAttachments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayAttachments)
{-# DEPRECATED dtgaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaTransitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayAttachments (Lude.Maybe [Lude.Text])
dtgaTransitGatewayAttachmentIds = Lens.lens (transitGatewayAttachmentIds :: DescribeTransitGatewayAttachments -> Lude.Maybe [Lude.Text]) (\s a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayAttachments)
{-# DEPRECATED dtgaTransitGatewayAttachmentIds "Use generic-lens or generic-optics with 'transitGatewayAttachmentIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaDryRun :: Lens.Lens' DescribeTransitGatewayAttachments (Lude.Maybe Lude.Bool)
dtgaDryRun = Lens.lens (dryRun :: DescribeTransitGatewayAttachments -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTransitGatewayAttachments)
{-# DEPRECATED dtgaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaMaxResults :: Lens.Lens' DescribeTransitGatewayAttachments (Lude.Maybe Lude.Natural)
dtgaMaxResults = Lens.lens (maxResults :: DescribeTransitGatewayAttachments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTransitGatewayAttachments)
{-# DEPRECATED dtgaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTransitGatewayAttachments where
  page rq rs
    | Page.stop (rs Lens.^. dtgarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtgarsTransitGatewayAttachments) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtgaNextToken Lens..~ rs Lens.^. dtgarsNextToken

instance Lude.AWSRequest DescribeTransitGatewayAttachments where
  type
    Rs DescribeTransitGatewayAttachments =
      DescribeTransitGatewayAttachmentsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTransitGatewayAttachmentsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "transitGatewayAttachments" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTransitGatewayAttachments where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTransitGatewayAttachments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTransitGatewayAttachments where
  toQuery DescribeTransitGatewayAttachments' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTransitGatewayAttachments" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "TransitGatewayAttachmentIds"
              Lude.<$> transitGatewayAttachmentIds
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeTransitGatewayAttachmentsResponse' smart constructor.
data DescribeTransitGatewayAttachmentsResponse = DescribeTransitGatewayAttachmentsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    transitGatewayAttachments ::
      Lude.Maybe
        [TransitGatewayAttachment],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayAttachmentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayAttachments' - Information about the attachments.
mkDescribeTransitGatewayAttachmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTransitGatewayAttachmentsResponse
mkDescribeTransitGatewayAttachmentsResponse pResponseStatus_ =
  DescribeTransitGatewayAttachmentsResponse'
    { nextToken =
        Lude.Nothing,
      transitGatewayAttachments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarsNextToken :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse (Lude.Maybe Lude.Text)
dtgarsNextToken = Lens.lens (nextToken :: DescribeTransitGatewayAttachmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayAttachmentsResponse)
{-# DEPRECATED dtgarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarsTransitGatewayAttachments :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse (Lude.Maybe [TransitGatewayAttachment])
dtgarsTransitGatewayAttachments = Lens.lens (transitGatewayAttachments :: DescribeTransitGatewayAttachmentsResponse -> Lude.Maybe [TransitGatewayAttachment]) (\s a -> s {transitGatewayAttachments = a} :: DescribeTransitGatewayAttachmentsResponse)
{-# DEPRECATED dtgarsTransitGatewayAttachments "Use generic-lens or generic-optics with 'transitGatewayAttachments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarsResponseStatus :: Lens.Lens' DescribeTransitGatewayAttachmentsResponse Lude.Int
dtgarsResponseStatus = Lens.lens (responseStatus :: DescribeTransitGatewayAttachmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTransitGatewayAttachmentsResponse)
{-# DEPRECATED dtgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
