{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified security groups or all of your security groups.
--
-- A security group is for use with instances either in the EC2-Classic platform or in a specific VPC. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 Security Groups> in the /Amazon Elastic Compute Cloud User Guide/ and <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSecurityGroups
  ( -- * Creating a request
    DescribeSecurityGroups (..),
    mkDescribeSecurityGroups,

    -- ** Request lenses
    dsgsFilters,
    dsgsGroupNames,
    dsgsGroupIds,
    dsgsNextToken,
    dsgsDryRun,
    dsgsMaxResults,

    -- * Destructuring the response
    DescribeSecurityGroupsResponse (..),
    mkDescribeSecurityGroupsResponse,

    -- ** Response lenses
    dsgrsSecurityGroups,
    dsgrsNextToken,
    dsgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSecurityGroups' smart constructor.
data DescribeSecurityGroups = DescribeSecurityGroups'
  { filters ::
      Lude.Maybe [Filter],
    groupNames :: Lude.Maybe [Lude.Text],
    groupIds :: Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSecurityGroups' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters. If using multiple filters for rules, the results include security groups for which any combination of rules - not necessarily a single rule - match all filters.
--
--
--     * @description@ - The description of the security group.
--
--
--     * @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound security group rule.
--
--
--     * @egress.ip-permission.from-port@ - For an outbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.
--
--
--     * @egress.ip-permission.group-id@ - The ID of a security group that has been referenced in an outbound security group rule.
--
--
--     * @egress.ip-permission.group-name@ - The name of a security group that has been referenced in an outbound security group rule.
--
--
--     * @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an outbound security group rule.
--
--
--     * @egress.ip-permission.prefix-list-id@ - The ID of a prefix list to which a security group rule allows outbound access.
--
--
--     * @egress.ip-permission.protocol@ - The IP protocol for an outbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).
--
--
--     * @egress.ip-permission.to-port@ - For an outbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.
--
--
--     * @egress.ip-permission.user-id@ - The ID of an AWS account that has been referenced in an outbound security group rule.
--
--
--     * @group-id@ - The ID of the security group.
--
--
--     * @group-name@ - The name of the security group.
--
--
--     * @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security group rule.
--
--
--     * @ip-permission.from-port@ - For an inbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.
--
--
--     * @ip-permission.group-id@ - The ID of a security group that has been referenced in an inbound security group rule.
--
--
--     * @ip-permission.group-name@ - The name of a security group that has been referenced in an inbound security group rule.
--
--
--     * @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound security group rule.
--
--
--     * @ip-permission.prefix-list-id@ - The ID of a prefix list from which a security group rule allows inbound access.
--
--
--     * @ip-permission.protocol@ - The IP protocol for an inbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).
--
--
--     * @ip-permission.to-port@ - For an inbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.
--
--
--     * @ip-permission.user-id@ - The ID of an AWS account that has been referenced in an inbound security group rule.
--
--
--     * @owner-id@ - The AWS account ID of the owner of the security group.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC specified when the security group was created.
--
--
-- * 'groupIds' - The IDs of the security groups. Required for security groups in a nondefault VPC.
--
-- Default: Describes all your security groups.
-- * 'groupNames' - [EC2-Classic and default VPC only] The names of the security groups. You can specify either the security group name or the security group ID. For security groups in a nondefault VPC, use the @group-name@ filter to describe security groups by name.
--
-- Default: Describes all your security groups.
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value. This value can be between 5 and 1000. If this parameter is not specified, then all results are returned.
-- * 'nextToken' - The token to request the next page of results.
mkDescribeSecurityGroups ::
  DescribeSecurityGroups
mkDescribeSecurityGroups =
  DescribeSecurityGroups'
    { filters = Lude.Nothing,
      groupNames = Lude.Nothing,
      groupIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters. If using multiple filters for rules, the results include security groups for which any combination of rules - not necessarily a single rule - match all filters.
--
--
--     * @description@ - The description of the security group.
--
--
--     * @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound security group rule.
--
--
--     * @egress.ip-permission.from-port@ - For an outbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.
--
--
--     * @egress.ip-permission.group-id@ - The ID of a security group that has been referenced in an outbound security group rule.
--
--
--     * @egress.ip-permission.group-name@ - The name of a security group that has been referenced in an outbound security group rule.
--
--
--     * @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an outbound security group rule.
--
--
--     * @egress.ip-permission.prefix-list-id@ - The ID of a prefix list to which a security group rule allows outbound access.
--
--
--     * @egress.ip-permission.protocol@ - The IP protocol for an outbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).
--
--
--     * @egress.ip-permission.to-port@ - For an outbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.
--
--
--     * @egress.ip-permission.user-id@ - The ID of an AWS account that has been referenced in an outbound security group rule.
--
--
--     * @group-id@ - The ID of the security group.
--
--
--     * @group-name@ - The name of the security group.
--
--
--     * @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security group rule.
--
--
--     * @ip-permission.from-port@ - For an inbound rule, the start of port range for the TCP and UDP protocols, or an ICMP type number.
--
--
--     * @ip-permission.group-id@ - The ID of a security group that has been referenced in an inbound security group rule.
--
--
--     * @ip-permission.group-name@ - The name of a security group that has been referenced in an inbound security group rule.
--
--
--     * @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound security group rule.
--
--
--     * @ip-permission.prefix-list-id@ - The ID of a prefix list from which a security group rule allows inbound access.
--
--
--     * @ip-permission.protocol@ - The IP protocol for an inbound security group rule (@tcp@ | @udp@ | @icmp@ or a protocol number).
--
--
--     * @ip-permission.to-port@ - For an inbound rule, the end of port range for the TCP and UDP protocols, or an ICMP code.
--
--
--     * @ip-permission.user-id@ - The ID of an AWS account that has been referenced in an inbound security group rule.
--
--
--     * @owner-id@ - The AWS account ID of the owner of the security group.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC specified when the security group was created.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsFilters :: Lens.Lens' DescribeSecurityGroups (Lude.Maybe [Filter])
dsgsFilters = Lens.lens (filters :: DescribeSecurityGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeSecurityGroups)
{-# DEPRECATED dsgsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | [EC2-Classic and default VPC only] The names of the security groups. You can specify either the security group name or the security group ID. For security groups in a nondefault VPC, use the @group-name@ filter to describe security groups by name.
--
-- Default: Describes all your security groups.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsGroupNames :: Lens.Lens' DescribeSecurityGroups (Lude.Maybe [Lude.Text])
dsgsGroupNames = Lens.lens (groupNames :: DescribeSecurityGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {groupNames = a} :: DescribeSecurityGroups)
{-# DEPRECATED dsgsGroupNames "Use generic-lens or generic-optics with 'groupNames' instead." #-}

-- | The IDs of the security groups. Required for security groups in a nondefault VPC.
--
-- Default: Describes all your security groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsGroupIds :: Lens.Lens' DescribeSecurityGroups (Lude.Maybe [Lude.Text])
dsgsGroupIds = Lens.lens (groupIds :: DescribeSecurityGroups -> Lude.Maybe [Lude.Text]) (\s a -> s {groupIds = a} :: DescribeSecurityGroups)
{-# DEPRECATED dsgsGroupIds "Use generic-lens or generic-optics with 'groupIds' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsNextToken :: Lens.Lens' DescribeSecurityGroups (Lude.Maybe Lude.Text)
dsgsNextToken = Lens.lens (nextToken :: DescribeSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSecurityGroups)
{-# DEPRECATED dsgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsDryRun :: Lens.Lens' DescribeSecurityGroups (Lude.Maybe Lude.Bool)
dsgsDryRun = Lens.lens (dryRun :: DescribeSecurityGroups -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSecurityGroups)
{-# DEPRECATED dsgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value. This value can be between 5 and 1000. If this parameter is not specified, then all results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgsMaxResults :: Lens.Lens' DescribeSecurityGroups (Lude.Maybe Lude.Natural)
dsgsMaxResults = Lens.lens (maxResults :: DescribeSecurityGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeSecurityGroups)
{-# DEPRECATED dsgsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSecurityGroups where
  page rq rs
    | Page.stop (rs Lens.^. dsgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsgrsSecurityGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsgsNextToken Lens..~ rs Lens.^. dsgrsNextToken

instance Lude.AWSRequest DescribeSecurityGroups where
  type Rs DescribeSecurityGroups = DescribeSecurityGroupsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSecurityGroupsResponse'
            Lude.<$> ( x Lude..@? "securityGroupInfo" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSecurityGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSecurityGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSecurityGroups where
  toQuery DescribeSecurityGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSecurityGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "GroupName" Lude.<$> groupNames),
        Lude.toQuery (Lude.toQueryList "GroupId" Lude.<$> groupIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeSecurityGroupsResponse' smart constructor.
data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse'
  { securityGroups ::
      Lude.Maybe [SecurityGroup],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'securityGroups' - Information about the security groups.
mkDescribeSecurityGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSecurityGroupsResponse
mkDescribeSecurityGroupsResponse pResponseStatus_ =
  DescribeSecurityGroupsResponse'
    { securityGroups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the security groups.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrsSecurityGroups :: Lens.Lens' DescribeSecurityGroupsResponse (Lude.Maybe [SecurityGroup])
dsgrsSecurityGroups = Lens.lens (securityGroups :: DescribeSecurityGroupsResponse -> Lude.Maybe [SecurityGroup]) (\s a -> s {securityGroups = a} :: DescribeSecurityGroupsResponse)
{-# DEPRECATED dsgrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrsNextToken :: Lens.Lens' DescribeSecurityGroupsResponse (Lude.Maybe Lude.Text)
dsgrsNextToken = Lens.lens (nextToken :: DescribeSecurityGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSecurityGroupsResponse)
{-# DEPRECATED dsgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrsResponseStatus :: Lens.Lens' DescribeSecurityGroupsResponse Lude.Int
dsgrsResponseStatus = Lens.lens (responseStatus :: DescribeSecurityGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSecurityGroupsResponse)
{-# DEPRECATED dsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
