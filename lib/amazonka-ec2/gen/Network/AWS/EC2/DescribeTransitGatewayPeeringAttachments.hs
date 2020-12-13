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
    dtgpaFilters,
    dtgpaNextToken,
    dtgpaTransitGatewayAttachmentIds,
    dtgpaDryRun,
    dtgpaMaxResults,

    -- * Destructuring the response
    DescribeTransitGatewayPeeringAttachmentsResponse (..),
    mkDescribeTransitGatewayPeeringAttachmentsResponse,

    -- ** Response lenses
    dtgpasrsTransitGatewayPeeringAttachments,
    dtgpasrsNextToken,
    dtgpasrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTransitGatewayPeeringAttachments' smart constructor.
data DescribeTransitGatewayPeeringAttachments = DescribeTransitGatewayPeeringAttachments'
  { -- | One or more filters. The possible values are:
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
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more IDs of the transit gateway peering attachments.
    transitGatewayAttachmentIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayPeeringAttachments' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. The possible values are:
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
-- * 'nextToken' - The token for the next page of results.
-- * 'transitGatewayAttachmentIds' - One or more IDs of the transit gateway peering attachments.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeTransitGatewayPeeringAttachments ::
  DescribeTransitGatewayPeeringAttachments
mkDescribeTransitGatewayPeeringAttachments =
  DescribeTransitGatewayPeeringAttachments'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      transitGatewayAttachmentIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

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
dtgpaFilters :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Lude.Maybe [Filter])
dtgpaFilters = Lens.lens (filters :: DescribeTransitGatewayPeeringAttachments -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTransitGatewayPeeringAttachments)
{-# DEPRECATED dtgpaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaNextToken :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Lude.Maybe Lude.Text)
dtgpaNextToken = Lens.lens (nextToken :: DescribeTransitGatewayPeeringAttachments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayPeeringAttachments)
{-# DEPRECATED dtgpaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more IDs of the transit gateway peering attachments.
--
-- /Note:/ Consider using 'transitGatewayAttachmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaTransitGatewayAttachmentIds :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Lude.Maybe [Lude.Text])
dtgpaTransitGatewayAttachmentIds = Lens.lens (transitGatewayAttachmentIds :: DescribeTransitGatewayPeeringAttachments -> Lude.Maybe [Lude.Text]) (\s a -> s {transitGatewayAttachmentIds = a} :: DescribeTransitGatewayPeeringAttachments)
{-# DEPRECATED dtgpaTransitGatewayAttachmentIds "Use generic-lens or generic-optics with 'transitGatewayAttachmentIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaDryRun :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Lude.Maybe Lude.Bool)
dtgpaDryRun = Lens.lens (dryRun :: DescribeTransitGatewayPeeringAttachments -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTransitGatewayPeeringAttachments)
{-# DEPRECATED dtgpaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpaMaxResults :: Lens.Lens' DescribeTransitGatewayPeeringAttachments (Lude.Maybe Lude.Natural)
dtgpaMaxResults = Lens.lens (maxResults :: DescribeTransitGatewayPeeringAttachments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTransitGatewayPeeringAttachments)
{-# DEPRECATED dtgpaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTransitGatewayPeeringAttachments where
  page rq rs
    | Page.stop (rs Lens.^. dtgpasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtgpasrsTransitGatewayPeeringAttachments) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtgpaNextToken Lens..~ rs Lens.^. dtgpasrsNextToken

instance Lude.AWSRequest DescribeTransitGatewayPeeringAttachments where
  type
    Rs DescribeTransitGatewayPeeringAttachments =
      DescribeTransitGatewayPeeringAttachmentsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTransitGatewayPeeringAttachmentsResponse'
            Lude.<$> ( x Lude..@? "transitGatewayPeeringAttachments" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTransitGatewayPeeringAttachments where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTransitGatewayPeeringAttachments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTransitGatewayPeeringAttachments where
  toQuery DescribeTransitGatewayPeeringAttachments' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTransitGatewayPeeringAttachments" :: Lude.ByteString),
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

-- | /See:/ 'mkDescribeTransitGatewayPeeringAttachmentsResponse' smart constructor.
data DescribeTransitGatewayPeeringAttachmentsResponse = DescribeTransitGatewayPeeringAttachmentsResponse'
  { -- | The transit gateway peering attachments.
    transitGatewayPeeringAttachments :: Lude.Maybe [TransitGatewayPeeringAttachment],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayPeeringAttachmentsResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayPeeringAttachments' - The transit gateway peering attachments.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeTransitGatewayPeeringAttachmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTransitGatewayPeeringAttachmentsResponse
mkDescribeTransitGatewayPeeringAttachmentsResponse pResponseStatus_ =
  DescribeTransitGatewayPeeringAttachmentsResponse'
    { transitGatewayPeeringAttachments =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachments.
--
-- /Note:/ Consider using 'transitGatewayPeeringAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpasrsTransitGatewayPeeringAttachments :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Lude.Maybe [TransitGatewayPeeringAttachment])
dtgpasrsTransitGatewayPeeringAttachments = Lens.lens (transitGatewayPeeringAttachments :: DescribeTransitGatewayPeeringAttachmentsResponse -> Lude.Maybe [TransitGatewayPeeringAttachment]) (\s a -> s {transitGatewayPeeringAttachments = a} :: DescribeTransitGatewayPeeringAttachmentsResponse)
{-# DEPRECATED dtgpasrsTransitGatewayPeeringAttachments "Use generic-lens or generic-optics with 'transitGatewayPeeringAttachments' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpasrsNextToken :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Lude.Maybe Lude.Text)
dtgpasrsNextToken = Lens.lens (nextToken :: DescribeTransitGatewayPeeringAttachmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayPeeringAttachmentsResponse)
{-# DEPRECATED dtgpasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpasrsResponseStatus :: Lens.Lens' DescribeTransitGatewayPeeringAttachmentsResponse Lude.Int
dtgpasrsResponseStatus = Lens.lens (responseStatus :: DescribeTransitGatewayPeeringAttachmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTransitGatewayPeeringAttachmentsResponse)
{-# DEPRECATED dtgpasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
