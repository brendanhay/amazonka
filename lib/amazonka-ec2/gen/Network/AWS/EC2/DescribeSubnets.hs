{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSubnets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your subnets.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and Subnets> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSubnets
  ( -- * Creating a request
    DescribeSubnets (..),
    mkDescribeSubnets,

    -- ** Request lenses
    dsSubnetIds,
    dsFilters,
    dsNextToken,
    dsDryRun,
    dsMaxResults,

    -- * Destructuring the response
    DescribeSubnetsResponse (..),
    mkDescribeSubnetsResponse,

    -- ** Response lenses
    dsrsSubnets,
    dsrsNextToken,
    dsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSubnets' smart constructor.
data DescribeSubnets = DescribeSubnets'
  { -- | One or more subnet IDs.
    --
    -- Default: Describes all your subnets.
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | One or more filters.
    --
    --
    --     * @availability-zone@ - The Availability Zone for the subnet. You can also use @availabilityZone@ as the filter name.
    --
    --
    --     * @availability-zone-id@ - The ID of the Availability Zone for the subnet. You can also use @availabilityZoneId@ as the filter name.
    --
    --
    --     * @available-ip-address-count@ - The number of IPv4 addresses in the subnet that are available.
    --
    --
    --     * @cidr-block@ - The IPv4 CIDR block of the subnet. The CIDR block you specify must exactly match the subnet's CIDR block for information to be returned for the subnet. You can also use @cidr@ or @cidrBlock@ as the filter names.
    --
    --
    --     * @default-for-az@ - Indicates whether this is the default subnet for the Availability Zone. You can also use @defaultForAz@ as the filter name.
    --
    --
    --     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the subnet.
    --
    --
    --     * @ipv6-cidr-block-association.association-id@ - An association ID for an IPv6 CIDR block associated with the subnet.
    --
    --
    --     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the subnet.
    --
    --
    --     * @owner-id@ - The ID of the AWS account that owns the subnet.
    --
    --
    --     * @state@ - The state of the subnet (@pending@ | @available@ ).
    --
    --
    --     * @subnet-arn@ - The Amazon Resource Name (ARN) of the subnet.
    --
    --
    --     * @subnet-id@ - The ID of the subnet.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC for the subnet.
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubnets' with the minimum fields required to make a request.
--
-- * 'subnetIds' - One or more subnet IDs.
--
-- Default: Describes all your subnets.
-- * 'filters' - One or more filters.
--
--
--     * @availability-zone@ - The Availability Zone for the subnet. You can also use @availabilityZone@ as the filter name.
--
--
--     * @availability-zone-id@ - The ID of the Availability Zone for the subnet. You can also use @availabilityZoneId@ as the filter name.
--
--
--     * @available-ip-address-count@ - The number of IPv4 addresses in the subnet that are available.
--
--
--     * @cidr-block@ - The IPv4 CIDR block of the subnet. The CIDR block you specify must exactly match the subnet's CIDR block for information to be returned for the subnet. You can also use @cidr@ or @cidrBlock@ as the filter names.
--
--
--     * @default-for-az@ - Indicates whether this is the default subnet for the Availability Zone. You can also use @defaultForAz@ as the filter name.
--
--
--     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the subnet.
--
--
--     * @ipv6-cidr-block-association.association-id@ - An association ID for an IPv6 CIDR block associated with the subnet.
--
--
--     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the subnet.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the subnet.
--
--
--     * @state@ - The state of the subnet (@pending@ | @available@ ).
--
--
--     * @subnet-arn@ - The Amazon Resource Name (ARN) of the subnet.
--
--
--     * @subnet-id@ - The ID of the subnet.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC for the subnet.
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeSubnets ::
  DescribeSubnets
mkDescribeSubnets =
  DescribeSubnets'
    { subnetIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more subnet IDs.
--
-- Default: Describes all your subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSubnetIds :: Lens.Lens' DescribeSubnets (Lude.Maybe [Lude.Text])
dsSubnetIds = Lens.lens (subnetIds :: DescribeSubnets -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: DescribeSubnets)
{-# DEPRECATED dsSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | One or more filters.
--
--
--     * @availability-zone@ - The Availability Zone for the subnet. You can also use @availabilityZone@ as the filter name.
--
--
--     * @availability-zone-id@ - The ID of the Availability Zone for the subnet. You can also use @availabilityZoneId@ as the filter name.
--
--
--     * @available-ip-address-count@ - The number of IPv4 addresses in the subnet that are available.
--
--
--     * @cidr-block@ - The IPv4 CIDR block of the subnet. The CIDR block you specify must exactly match the subnet's CIDR block for information to be returned for the subnet. You can also use @cidr@ or @cidrBlock@ as the filter names.
--
--
--     * @default-for-az@ - Indicates whether this is the default subnet for the Availability Zone. You can also use @defaultForAz@ as the filter name.
--
--
--     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the subnet.
--
--
--     * @ipv6-cidr-block-association.association-id@ - An association ID for an IPv6 CIDR block associated with the subnet.
--
--
--     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the subnet.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the subnet.
--
--
--     * @state@ - The state of the subnet (@pending@ | @available@ ).
--
--
--     * @subnet-arn@ - The Amazon Resource Name (ARN) of the subnet.
--
--
--     * @subnet-id@ - The ID of the subnet.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC for the subnet.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFilters :: Lens.Lens' DescribeSubnets (Lude.Maybe [Filter])
dsFilters = Lens.lens (filters :: DescribeSubnets -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeSubnets)
{-# DEPRECATED dsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSubnets (Lude.Maybe Lude.Text)
dsNextToken = Lens.lens (nextToken :: DescribeSubnets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSubnets)
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDryRun :: Lens.Lens' DescribeSubnets (Lude.Maybe Lude.Bool)
dsDryRun = Lens.lens (dryRun :: DescribeSubnets -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSubnets)
{-# DEPRECATED dsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxResults :: Lens.Lens' DescribeSubnets (Lude.Maybe Lude.Natural)
dsMaxResults = Lens.lens (maxResults :: DescribeSubnets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeSubnets)
{-# DEPRECATED dsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSubnets where
  page rq rs
    | Page.stop (rs Lens.^. dsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsrsSubnets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsNextToken Lens..~ rs Lens.^. dsrsNextToken

instance Lude.AWSRequest DescribeSubnets where
  type Rs DescribeSubnets = DescribeSubnetsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSubnetsResponse'
            Lude.<$> ( x Lude..@? "subnetSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSubnets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSubnets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSubnets where
  toQuery DescribeSubnets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSubnets" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "SubnetId" Lude.<$> subnetIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeSubnetsResponse' smart constructor.
data DescribeSubnetsResponse = DescribeSubnetsResponse'
  { -- | Information about one or more subnets.
    subnets :: Lude.Maybe [Subnet],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSubnetsResponse' with the minimum fields required to make a request.
--
-- * 'subnets' - Information about one or more subnets.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeSubnetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSubnetsResponse
mkDescribeSubnetsResponse pResponseStatus_ =
  DescribeSubnetsResponse'
    { subnets = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more subnets.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSubnets :: Lens.Lens' DescribeSubnetsResponse (Lude.Maybe [Subnet])
dsrsSubnets = Lens.lens (subnets :: DescribeSubnetsResponse -> Lude.Maybe [Subnet]) (\s a -> s {subnets = a} :: DescribeSubnetsResponse)
{-# DEPRECATED dsrsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsNextToken :: Lens.Lens' DescribeSubnetsResponse (Lude.Maybe Lude.Text)
dsrsNextToken = Lens.lens (nextToken :: DescribeSubnetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSubnetsResponse)
{-# DEPRECATED dsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeSubnetsResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeSubnetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSubnetsResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
