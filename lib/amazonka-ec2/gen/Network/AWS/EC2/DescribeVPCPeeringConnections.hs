{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCPeeringConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC peering connections.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCPeeringConnections
  ( -- * Creating a request
    DescribeVPCPeeringConnections (..),
    mkDescribeVPCPeeringConnections,

    -- ** Request lenses
    dvpcpcFilters,
    dvpcpcNextToken,
    dvpcpcVPCPeeringConnectionIds,
    dvpcpcDryRun,
    dvpcpcMaxResults,

    -- * Destructuring the response
    DescribeVPCPeeringConnectionsResponse (..),
    mkDescribeVPCPeeringConnectionsResponse,

    -- ** Response lenses
    dvpcrsNextToken,
    dvpcrsVPCPeeringConnections,
    dvpcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCPeeringConnections' smart constructor.
data DescribeVPCPeeringConnections = DescribeVPCPeeringConnections'
  { -- | One or more filters.
    --
    --
    --     * @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter VPC.
    --
    --
    --     * @accepter-vpc-info.owner-id@ - The AWS account ID of the owner of the accepter VPC.
    --
    --
    --     * @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.
    --
    --
    --     * @expiration-time@ - The expiration date and time for the VPC peering connection.
    --
    --
    --     * @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the requester's VPC.
    --
    --
    --     * @requester-vpc-info.owner-id@ - The AWS account ID of the owner of the requester VPC.
    --
    --
    --     * @requester-vpc-info.vpc-id@ - The ID of the requester VPC.
    --
    --
    --     * @status-code@ - The status of the VPC peering connection (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ | @active@ | @deleting@ | @deleted@ | @rejected@ ).
    --
    --
    --     * @status-message@ - A message that provides more information about the status of the VPC peering connection, if applicable.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-peering-connection-id@ - The ID of the VPC peering connection.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more VPC peering connection IDs.
    --
    -- Default: Describes all your VPC peering connections.
    vpcPeeringConnectionIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCPeeringConnections' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter VPC.
--
--
--     * @accepter-vpc-info.owner-id@ - The AWS account ID of the owner of the accepter VPC.
--
--
--     * @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.
--
--
--     * @expiration-time@ - The expiration date and time for the VPC peering connection.
--
--
--     * @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the requester's VPC.
--
--
--     * @requester-vpc-info.owner-id@ - The AWS account ID of the owner of the requester VPC.
--
--
--     * @requester-vpc-info.vpc-id@ - The ID of the requester VPC.
--
--
--     * @status-code@ - The status of the VPC peering connection (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ | @active@ | @deleting@ | @deleted@ | @rejected@ ).
--
--
--     * @status-message@ - A message that provides more information about the status of the VPC peering connection, if applicable.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-peering-connection-id@ - The ID of the VPC peering connection.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'vpcPeeringConnectionIds' - One or more VPC peering connection IDs.
--
-- Default: Describes all your VPC peering connections.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeVPCPeeringConnections ::
  DescribeVPCPeeringConnections
mkDescribeVPCPeeringConnections =
  DescribeVPCPeeringConnections'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      vpcPeeringConnectionIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter VPC.
--
--
--     * @accepter-vpc-info.owner-id@ - The AWS account ID of the owner of the accepter VPC.
--
--
--     * @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.
--
--
--     * @expiration-time@ - The expiration date and time for the VPC peering connection.
--
--
--     * @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the requester's VPC.
--
--
--     * @requester-vpc-info.owner-id@ - The AWS account ID of the owner of the requester VPC.
--
--
--     * @requester-vpc-info.vpc-id@ - The ID of the requester VPC.
--
--
--     * @status-code@ - The status of the VPC peering connection (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ | @active@ | @deleting@ | @deleted@ | @rejected@ ).
--
--
--     * @status-message@ - A message that provides more information about the status of the VPC peering connection, if applicable.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-peering-connection-id@ - The ID of the VPC peering connection.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcpcFilters :: Lens.Lens' DescribeVPCPeeringConnections (Lude.Maybe [Filter])
dvpcpcFilters = Lens.lens (filters :: DescribeVPCPeeringConnections -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCPeeringConnections)
{-# DEPRECATED dvpcpcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcpcNextToken :: Lens.Lens' DescribeVPCPeeringConnections (Lude.Maybe Lude.Text)
dvpcpcNextToken = Lens.lens (nextToken :: DescribeVPCPeeringConnections -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCPeeringConnections)
{-# DEPRECATED dvpcpcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more VPC peering connection IDs.
--
-- Default: Describes all your VPC peering connections.
--
-- /Note:/ Consider using 'vpcPeeringConnectionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcpcVPCPeeringConnectionIds :: Lens.Lens' DescribeVPCPeeringConnections (Lude.Maybe [Lude.Text])
dvpcpcVPCPeeringConnectionIds = Lens.lens (vpcPeeringConnectionIds :: DescribeVPCPeeringConnections -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcPeeringConnectionIds = a} :: DescribeVPCPeeringConnections)
{-# DEPRECATED dvpcpcVPCPeeringConnectionIds "Use generic-lens or generic-optics with 'vpcPeeringConnectionIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcpcDryRun :: Lens.Lens' DescribeVPCPeeringConnections (Lude.Maybe Lude.Bool)
dvpcpcDryRun = Lens.lens (dryRun :: DescribeVPCPeeringConnections -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCPeeringConnections)
{-# DEPRECATED dvpcpcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcpcMaxResults :: Lens.Lens' DescribeVPCPeeringConnections (Lude.Maybe Lude.Natural)
dvpcpcMaxResults = Lens.lens (maxResults :: DescribeVPCPeeringConnections -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeVPCPeeringConnections)
{-# DEPRECATED dvpcpcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCPeeringConnections where
  page rq rs
    | Page.stop (rs Lens.^. dvpcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvpcrsVPCPeeringConnections) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvpcpcNextToken Lens..~ rs Lens.^. dvpcrsNextToken

instance Lude.AWSRequest DescribeVPCPeeringConnections where
  type
    Rs DescribeVPCPeeringConnections =
      DescribeVPCPeeringConnectionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCPeeringConnectionsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "vpcPeeringConnectionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCPeeringConnections where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCPeeringConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCPeeringConnections where
  toQuery DescribeVPCPeeringConnections' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeVpcPeeringConnections" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "VpcPeeringConnectionId"
              Lude.<$> vpcPeeringConnectionIds
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVPCPeeringConnectionsResponse' smart constructor.
data DescribeVPCPeeringConnectionsResponse = DescribeVPCPeeringConnectionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the VPC peering connections.
    vpcPeeringConnections :: Lude.Maybe [VPCPeeringConnection],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCPeeringConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'vpcPeeringConnections' - Information about the VPC peering connections.
-- * 'responseStatus' - The response status code.
mkDescribeVPCPeeringConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCPeeringConnectionsResponse
mkDescribeVPCPeeringConnectionsResponse pResponseStatus_ =
  DescribeVPCPeeringConnectionsResponse'
    { nextToken = Lude.Nothing,
      vpcPeeringConnections = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrsNextToken :: Lens.Lens' DescribeVPCPeeringConnectionsResponse (Lude.Maybe Lude.Text)
dvpcrsNextToken = Lens.lens (nextToken :: DescribeVPCPeeringConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCPeeringConnectionsResponse)
{-# DEPRECATED dvpcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the VPC peering connections.
--
-- /Note:/ Consider using 'vpcPeeringConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrsVPCPeeringConnections :: Lens.Lens' DescribeVPCPeeringConnectionsResponse (Lude.Maybe [VPCPeeringConnection])
dvpcrsVPCPeeringConnections = Lens.lens (vpcPeeringConnections :: DescribeVPCPeeringConnectionsResponse -> Lude.Maybe [VPCPeeringConnection]) (\s a -> s {vpcPeeringConnections = a} :: DescribeVPCPeeringConnectionsResponse)
{-# DEPRECATED dvpcrsVPCPeeringConnections "Use generic-lens or generic-optics with 'vpcPeeringConnections' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrsResponseStatus :: Lens.Lens' DescribeVPCPeeringConnectionsResponse Lude.Int
dvpcrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCPeeringConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCPeeringConnectionsResponse)
{-# DEPRECATED dvpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
