{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPCs.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCs
  ( -- * Creating a request
    DescribeVPCs (..),
    mkDescribeVPCs,

    -- ** Request lenses
    dvpcFilters,
    dvpcNextToken,
    dvpcVPCIds,
    dvpcDryRun,
    dvpcMaxResults,

    -- * Destructuring the response
    DescribeVPCsResponse (..),
    mkDescribeVPCsResponse,

    -- ** Response lenses
    dvrsVPCs,
    dvrsNextToken,
    dvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCs' smart constructor.
data DescribeVPCs = DescribeVPCs'
  { -- | One or more filters.
    --
    --
    --     * @cidr@ - The primary IPv4 CIDR block of the VPC. The CIDR block you specify must exactly match the VPC's CIDR block for information to be returned for the VPC. Must contain the slash followed by one or two digits (for example, @/28@ ).
    --
    --
    --     * @cidr-block-association.cidr-block@ - An IPv4 CIDR block associated with the VPC.
    --
    --
    --     * @cidr-block-association.association-id@ - The association ID for an IPv4 CIDR block associated with the VPC.
    --
    --
    --     * @cidr-block-association.state@ - The state of an IPv4 CIDR block associated with the VPC.
    --
    --
    --     * @dhcp-options-id@ - The ID of a set of DHCP options.
    --
    --
    --     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the VPC.
    --
    --
    --     * @ipv6-cidr-block-association.ipv6-pool@ - The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
    --
    --
    --     * @ipv6-cidr-block-association.association-id@ - The association ID for an IPv6 CIDR block associated with the VPC.
    --
    --
    --     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the VPC.
    --
    --
    --     * @isDefault@ - Indicates whether the VPC is the default VPC.
    --
    --
    --     * @owner-id@ - The ID of the AWS account that owns the VPC.
    --
    --
    --     * @state@ - The state of the VPC (@pending@ | @available@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more VPC IDs.
    --
    -- Default: Describes all your VPCs.
    vpcIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCs' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @cidr@ - The primary IPv4 CIDR block of the VPC. The CIDR block you specify must exactly match the VPC's CIDR block for information to be returned for the VPC. Must contain the slash followed by one or two digits (for example, @/28@ ).
--
--
--     * @cidr-block-association.cidr-block@ - An IPv4 CIDR block associated with the VPC.
--
--
--     * @cidr-block-association.association-id@ - The association ID for an IPv4 CIDR block associated with the VPC.
--
--
--     * @cidr-block-association.state@ - The state of an IPv4 CIDR block associated with the VPC.
--
--
--     * @dhcp-options-id@ - The ID of a set of DHCP options.
--
--
--     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the VPC.
--
--
--     * @ipv6-cidr-block-association.ipv6-pool@ - The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
--
--
--     * @ipv6-cidr-block-association.association-id@ - The association ID for an IPv6 CIDR block associated with the VPC.
--
--
--     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the VPC.
--
--
--     * @isDefault@ - Indicates whether the VPC is the default VPC.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the VPC.
--
--
--     * @state@ - The state of the VPC (@pending@ | @available@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'vpcIds' - One or more VPC IDs.
--
-- Default: Describes all your VPCs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeVPCs ::
  DescribeVPCs
mkDescribeVPCs =
  DescribeVPCs'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      vpcIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @cidr@ - The primary IPv4 CIDR block of the VPC. The CIDR block you specify must exactly match the VPC's CIDR block for information to be returned for the VPC. Must contain the slash followed by one or two digits (for example, @/28@ ).
--
--
--     * @cidr-block-association.cidr-block@ - An IPv4 CIDR block associated with the VPC.
--
--
--     * @cidr-block-association.association-id@ - The association ID for an IPv4 CIDR block associated with the VPC.
--
--
--     * @cidr-block-association.state@ - The state of an IPv4 CIDR block associated with the VPC.
--
--
--     * @dhcp-options-id@ - The ID of a set of DHCP options.
--
--
--     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the VPC.
--
--
--     * @ipv6-cidr-block-association.ipv6-pool@ - The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.
--
--
--     * @ipv6-cidr-block-association.association-id@ - The association ID for an IPv6 CIDR block associated with the VPC.
--
--
--     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the VPC.
--
--
--     * @isDefault@ - Indicates whether the VPC is the default VPC.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the VPC.
--
--
--     * @state@ - The state of the VPC (@pending@ | @available@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcFilters :: Lens.Lens' DescribeVPCs (Lude.Maybe [Filter])
dvpcFilters = Lens.lens (filters :: DescribeVPCs -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCs)
{-# DEPRECATED dvpcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcNextToken :: Lens.Lens' DescribeVPCs (Lude.Maybe Lude.Text)
dvpcNextToken = Lens.lens (nextToken :: DescribeVPCs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCs)
{-# DEPRECATED dvpcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more VPC IDs.
--
-- Default: Describes all your VPCs.
--
-- /Note:/ Consider using 'vpcIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcVPCIds :: Lens.Lens' DescribeVPCs (Lude.Maybe [Lude.Text])
dvpcVPCIds = Lens.lens (vpcIds :: DescribeVPCs -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcIds = a} :: DescribeVPCs)
{-# DEPRECATED dvpcVPCIds "Use generic-lens or generic-optics with 'vpcIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcDryRun :: Lens.Lens' DescribeVPCs (Lude.Maybe Lude.Bool)
dvpcDryRun = Lens.lens (dryRun :: DescribeVPCs -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCs)
{-# DEPRECATED dvpcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcMaxResults :: Lens.Lens' DescribeVPCs (Lude.Maybe Lude.Natural)
dvpcMaxResults = Lens.lens (maxResults :: DescribeVPCs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeVPCs)
{-# DEPRECATED dvpcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCs where
  page rq rs
    | Page.stop (rs Lens.^. dvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvrsVPCs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvpcNextToken Lens..~ rs Lens.^. dvrsNextToken

instance Lude.AWSRequest DescribeVPCs where
  type Rs DescribeVPCs = DescribeVPCsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCsResponse'
            Lude.<$> ( x Lude..@? "vpcSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCs where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCs where
  toQuery DescribeVPCs' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVpcs" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "VpcId" Lude.<$> vpcIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVPCsResponse' smart constructor.
data DescribeVPCsResponse = DescribeVPCsResponse'
  { -- | Information about one or more VPCs.
    vpcs :: Lude.Maybe [VPC],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCsResponse' with the minimum fields required to make a request.
--
-- * 'vpcs' - Information about one or more VPCs.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeVPCsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCsResponse
mkDescribeVPCsResponse pResponseStatus_ =
  DescribeVPCsResponse'
    { vpcs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more VPCs.
--
-- /Note:/ Consider using 'vpcs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsVPCs :: Lens.Lens' DescribeVPCsResponse (Lude.Maybe [VPC])
dvrsVPCs = Lens.lens (vpcs :: DescribeVPCsResponse -> Lude.Maybe [VPC]) (\s a -> s {vpcs = a} :: DescribeVPCsResponse)
{-# DEPRECATED dvrsVPCs "Use generic-lens or generic-optics with 'vpcs' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsNextToken :: Lens.Lens' DescribeVPCsResponse (Lude.Maybe Lude.Text)
dvrsNextToken = Lens.lens (nextToken :: DescribeVPCsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCsResponse)
{-# DEPRECATED dvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrsResponseStatus :: Lens.Lens' DescribeVPCsResponse Lude.Int
dvrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCsResponse)
{-# DEPRECATED dvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
