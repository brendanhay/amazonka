{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTransitGatewayVPCAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more VPC attachments. By default, all VPC attachments are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayVPCAttachments
  ( -- * Creating a request
    DescribeTransitGatewayVPCAttachments (..),
    mkDescribeTransitGatewayVPCAttachments,

    -- ** Request lenses
    dtgvaFilters,
    dtgvaNextToken,
    dtgvaTransitGatewayAttachmentIds,
    dtgvaDryRun,
    dtgvaMaxResults,

    -- * Destructuring the response
    DescribeTransitGatewayVPCAttachmentsResponse (..),
    mkDescribeTransitGatewayVPCAttachmentsResponse,

    -- ** Response lenses
    dtgvpcarsTransitGatewayVPCAttachments,
    dtgvpcarsNextToken,
    dtgvpcarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTransitGatewayVPCAttachments' smart constructor.
data DescribeTransitGatewayVPCAttachments = DescribeTransitGatewayVPCAttachments'
  { -- | One or more filters. The possible values are:
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
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The IDs of the attachments.
    transitGatewayAttachmentIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayVPCAttachments' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. The possible values are:
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
-- * 'nextToken' - The token for the next page of results.
-- * 'transitGatewayAttachmentIds' - The IDs of the attachments.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeTransitGatewayVPCAttachments ::
  DescribeTransitGatewayVPCAttachments
mkDescribeTransitGatewayVPCAttachments =
  DescribeTransitGatewayVPCAttachments'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      transitGatewayAttachmentIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

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
dtgvaFilters :: Lens.Lens' DescribeTransitGatewayVPCAttachments (Lude.Maybe [Filter])
dtgvaFilters = Lens.lens (filters :: DescribeTransitGatewayVPCAttachments -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTransitGatewayVPCAttachments)
{-# DEPRECATED dtgvaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvaNextToken :: Lens.Lens' DescribeTransitGatewayVPCAttachments (Lude.Maybe Lude.Text)
dtgvaNextToken = Lens.lens (nextToken :: DescribeTransitGatewayVPCAttachments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayVPCAttachments)
{-# DEPRECATED dtgvaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvaTransitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayVPCAttachments (Lude.Maybe [Lude.Text])
dtgvaTransitGatewayAttachmentIds = Lens.lens (transitGatewayAttachmentIds :: DescribeTransitGatewayVPCAttachments -> Lude.Maybe [Lude.Text]) (\s a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayVPCAttachments)
{-# DEPRECATED dtgvaTransitGatewayAttachmentIds "Use generic-lens or generic-optics with 'transitGatewayAttachmentIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvaDryRun :: Lens.Lens' DescribeTransitGatewayVPCAttachments (Lude.Maybe Lude.Bool)
dtgvaDryRun = Lens.lens (dryRun :: DescribeTransitGatewayVPCAttachments -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTransitGatewayVPCAttachments)
{-# DEPRECATED dtgvaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvaMaxResults :: Lens.Lens' DescribeTransitGatewayVPCAttachments (Lude.Maybe Lude.Natural)
dtgvaMaxResults = Lens.lens (maxResults :: DescribeTransitGatewayVPCAttachments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTransitGatewayVPCAttachments)
{-# DEPRECATED dtgvaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTransitGatewayVPCAttachments where
  page rq rs
    | Page.stop (rs Lens.^. dtgvpcarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtgvpcarsTransitGatewayVPCAttachments) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtgvaNextToken Lens..~ rs Lens.^. dtgvpcarsNextToken

instance Lude.AWSRequest DescribeTransitGatewayVPCAttachments where
  type
    Rs DescribeTransitGatewayVPCAttachments =
      DescribeTransitGatewayVPCAttachmentsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTransitGatewayVPCAttachmentsResponse'
            Lude.<$> ( x Lude..@? "transitGatewayVpcAttachments" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTransitGatewayVPCAttachments where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTransitGatewayVPCAttachments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTransitGatewayVPCAttachments where
  toQuery DescribeTransitGatewayVPCAttachments' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTransitGatewayVpcAttachments" :: Lude.ByteString),
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

-- | /See:/ 'mkDescribeTransitGatewayVPCAttachmentsResponse' smart constructor.
data DescribeTransitGatewayVPCAttachmentsResponse = DescribeTransitGatewayVPCAttachmentsResponse'
  { -- | Information about the VPC attachments.
    transitGatewayVPCAttachments :: Lude.Maybe [TransitGatewayVPCAttachment],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayVPCAttachmentsResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayVPCAttachments' - Information about the VPC attachments.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeTransitGatewayVPCAttachmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTransitGatewayVPCAttachmentsResponse
mkDescribeTransitGatewayVPCAttachmentsResponse pResponseStatus_ =
  DescribeTransitGatewayVPCAttachmentsResponse'
    { transitGatewayVPCAttachments =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPC attachments.
--
-- /Note:/ Consider using 'transitGatewayVPCAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvpcarsTransitGatewayVPCAttachments :: Lens.Lens' DescribeTransitGatewayVPCAttachmentsResponse (Lude.Maybe [TransitGatewayVPCAttachment])
dtgvpcarsTransitGatewayVPCAttachments = Lens.lens (transitGatewayVPCAttachments :: DescribeTransitGatewayVPCAttachmentsResponse -> Lude.Maybe [TransitGatewayVPCAttachment]) (\s a -> s {transitGatewayVPCAttachments = a} :: DescribeTransitGatewayVPCAttachmentsResponse)
{-# DEPRECATED dtgvpcarsTransitGatewayVPCAttachments "Use generic-lens or generic-optics with 'transitGatewayVPCAttachments' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvpcarsNextToken :: Lens.Lens' DescribeTransitGatewayVPCAttachmentsResponse (Lude.Maybe Lude.Text)
dtgvpcarsNextToken = Lens.lens (nextToken :: DescribeTransitGatewayVPCAttachmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayVPCAttachmentsResponse)
{-# DEPRECATED dtgvpcarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgvpcarsResponseStatus :: Lens.Lens' DescribeTransitGatewayVPCAttachmentsResponse Lude.Int
dtgvpcarsResponseStatus = Lens.lens (responseStatus :: DescribeTransitGatewayVPCAttachmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTransitGatewayVPCAttachmentsResponse)
{-# DEPRECATED dtgvpcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
