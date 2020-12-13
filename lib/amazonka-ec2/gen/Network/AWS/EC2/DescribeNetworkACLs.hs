{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNetworkACLs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network ACLs.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkACLs
  ( -- * Creating a request
    DescribeNetworkACLs (..),
    mkDescribeNetworkACLs,

    -- ** Request lenses
    dnaclFilters,
    dnaclNextToken,
    dnaclNetworkACLIds,
    dnaclDryRun,
    dnaclMaxResults,

    -- * Destructuring the response
    DescribeNetworkACLsResponse (..),
    mkDescribeNetworkACLsResponse,

    -- ** Response lenses
    dnarsNetworkACLs,
    dnarsNextToken,
    dnarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeNetworkACLs' smart constructor.
data DescribeNetworkACLs = DescribeNetworkACLs'
  { -- | One or more filters.
    --
    --
    --     * @association.association-id@ - The ID of an association ID for the ACL.
    --
    --
    --     * @association.network-acl-id@ - The ID of the network ACL involved in the association.
    --
    --
    --     * @association.subnet-id@ - The ID of the subnet involved in the association.
    --
    --
    --     * @default@ - Indicates whether the ACL is the default network ACL for the VPC.
    --
    --
    --     * @entry.cidr@ - The IPv4 CIDR range specified in the entry.
    --
    --
    --     * @entry.icmp.code@ - The ICMP code specified in the entry, if any.
    --
    --
    --     * @entry.icmp.type@ - The ICMP type specified in the entry, if any.
    --
    --
    --     * @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.
    --
    --
    --     * @entry.port-range.from@ - The start of the port range specified in the entry.
    --
    --
    --     * @entry.port-range.to@ - The end of the port range specified in the entry.
    --
    --
    --     * @entry.protocol@ - The protocol specified in the entry (@tcp@ | @udp@ | @icmp@ or a protocol number).
    --
    --
    --     * @entry.rule-action@ - Allows or denies the matching traffic (@allow@ | @deny@ ).
    --
    --
    --     * @entry.rule-number@ - The number of an entry (in other words, rule) in the set of ACL entries.
    --
    --
    --     * @network-acl-id@ - The ID of the network ACL.
    --
    --
    --     * @owner-id@ - The ID of the AWS account that owns the network ACL.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC for the network ACL.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | One or more network ACL IDs.
    --
    -- Default: Describes all your network ACLs.
    networkACLIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNetworkACLs' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @association.association-id@ - The ID of an association ID for the ACL.
--
--
--     * @association.network-acl-id@ - The ID of the network ACL involved in the association.
--
--
--     * @association.subnet-id@ - The ID of the subnet involved in the association.
--
--
--     * @default@ - Indicates whether the ACL is the default network ACL for the VPC.
--
--
--     * @entry.cidr@ - The IPv4 CIDR range specified in the entry.
--
--
--     * @entry.icmp.code@ - The ICMP code specified in the entry, if any.
--
--
--     * @entry.icmp.type@ - The ICMP type specified in the entry, if any.
--
--
--     * @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.
--
--
--     * @entry.port-range.from@ - The start of the port range specified in the entry.
--
--
--     * @entry.port-range.to@ - The end of the port range specified in the entry.
--
--
--     * @entry.protocol@ - The protocol specified in the entry (@tcp@ | @udp@ | @icmp@ or a protocol number).
--
--
--     * @entry.rule-action@ - Allows or denies the matching traffic (@allow@ | @deny@ ).
--
--
--     * @entry.rule-number@ - The number of an entry (in other words, rule) in the set of ACL entries.
--
--
--     * @network-acl-id@ - The ID of the network ACL.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the network ACL.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC for the network ACL.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'networkACLIds' - One or more network ACL IDs.
--
-- Default: Describes all your network ACLs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeNetworkACLs ::
  DescribeNetworkACLs
mkDescribeNetworkACLs =
  DescribeNetworkACLs'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      networkACLIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @association.association-id@ - The ID of an association ID for the ACL.
--
--
--     * @association.network-acl-id@ - The ID of the network ACL involved in the association.
--
--
--     * @association.subnet-id@ - The ID of the subnet involved in the association.
--
--
--     * @default@ - Indicates whether the ACL is the default network ACL for the VPC.
--
--
--     * @entry.cidr@ - The IPv4 CIDR range specified in the entry.
--
--
--     * @entry.icmp.code@ - The ICMP code specified in the entry, if any.
--
--
--     * @entry.icmp.type@ - The ICMP type specified in the entry, if any.
--
--
--     * @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.
--
--
--     * @entry.port-range.from@ - The start of the port range specified in the entry.
--
--
--     * @entry.port-range.to@ - The end of the port range specified in the entry.
--
--
--     * @entry.protocol@ - The protocol specified in the entry (@tcp@ | @udp@ | @icmp@ or a protocol number).
--
--
--     * @entry.rule-action@ - Allows or denies the matching traffic (@allow@ | @deny@ ).
--
--
--     * @entry.rule-number@ - The number of an entry (in other words, rule) in the set of ACL entries.
--
--
--     * @network-acl-id@ - The ID of the network ACL.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the network ACL.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC for the network ACL.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaclFilters :: Lens.Lens' DescribeNetworkACLs (Lude.Maybe [Filter])
dnaclFilters = Lens.lens (filters :: DescribeNetworkACLs -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeNetworkACLs)
{-# DEPRECATED dnaclFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaclNextToken :: Lens.Lens' DescribeNetworkACLs (Lude.Maybe Lude.Text)
dnaclNextToken = Lens.lens (nextToken :: DescribeNetworkACLs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNetworkACLs)
{-# DEPRECATED dnaclNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more network ACL IDs.
--
-- Default: Describes all your network ACLs.
--
-- /Note:/ Consider using 'networkACLIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaclNetworkACLIds :: Lens.Lens' DescribeNetworkACLs (Lude.Maybe [Lude.Text])
dnaclNetworkACLIds = Lens.lens (networkACLIds :: DescribeNetworkACLs -> Lude.Maybe [Lude.Text]) (\s a -> s {networkACLIds = a} :: DescribeNetworkACLs)
{-# DEPRECATED dnaclNetworkACLIds "Use generic-lens or generic-optics with 'networkACLIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaclDryRun :: Lens.Lens' DescribeNetworkACLs (Lude.Maybe Lude.Bool)
dnaclDryRun = Lens.lens (dryRun :: DescribeNetworkACLs -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeNetworkACLs)
{-# DEPRECATED dnaclDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaclMaxResults :: Lens.Lens' DescribeNetworkACLs (Lude.Maybe Lude.Natural)
dnaclMaxResults = Lens.lens (maxResults :: DescribeNetworkACLs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeNetworkACLs)
{-# DEPRECATED dnaclMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeNetworkACLs where
  page rq rs
    | Page.stop (rs Lens.^. dnarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dnarsNetworkACLs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dnaclNextToken Lens..~ rs Lens.^. dnarsNextToken

instance Lude.AWSRequest DescribeNetworkACLs where
  type Rs DescribeNetworkACLs = DescribeNetworkACLsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeNetworkACLsResponse'
            Lude.<$> ( x Lude..@? "networkAclSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNetworkACLs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeNetworkACLs where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNetworkACLs where
  toQuery DescribeNetworkACLs' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeNetworkAcls" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          (Lude.toQueryList "NetworkAclId" Lude.<$> networkACLIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeNetworkACLsResponse' smart constructor.
data DescribeNetworkACLsResponse = DescribeNetworkACLsResponse'
  { -- | Information about one or more network ACLs.
    networkACLs :: Lude.Maybe [NetworkACL],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNetworkACLsResponse' with the minimum fields required to make a request.
--
-- * 'networkACLs' - Information about one or more network ACLs.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeNetworkACLsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNetworkACLsResponse
mkDescribeNetworkACLsResponse pResponseStatus_ =
  DescribeNetworkACLsResponse'
    { networkACLs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more network ACLs.
--
-- /Note:/ Consider using 'networkACLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnarsNetworkACLs :: Lens.Lens' DescribeNetworkACLsResponse (Lude.Maybe [NetworkACL])
dnarsNetworkACLs = Lens.lens (networkACLs :: DescribeNetworkACLsResponse -> Lude.Maybe [NetworkACL]) (\s a -> s {networkACLs = a} :: DescribeNetworkACLsResponse)
{-# DEPRECATED dnarsNetworkACLs "Use generic-lens or generic-optics with 'networkACLs' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnarsNextToken :: Lens.Lens' DescribeNetworkACLsResponse (Lude.Maybe Lude.Text)
dnarsNextToken = Lens.lens (nextToken :: DescribeNetworkACLsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeNetworkACLsResponse)
{-# DEPRECATED dnarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnarsResponseStatus :: Lens.Lens' DescribeNetworkACLsResponse Lude.Int
dnarsResponseStatus = Lens.lens (responseStatus :: DescribeNetworkACLsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNetworkACLsResponse)
{-# DEPRECATED dnarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
