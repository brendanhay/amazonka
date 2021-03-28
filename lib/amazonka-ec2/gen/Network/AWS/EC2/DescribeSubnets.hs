{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeSubnets (..)
    , mkDescribeSubnets
    -- ** Request lenses
    , dsDryRun
    , dsFilters
    , dsMaxResults
    , dsNextToken
    , dsSubnetIds

    -- * Destructuring the response
    , DescribeSubnetsResponse (..)
    , mkDescribeSubnetsResponse
    -- ** Response lenses
    , dsrrsNextToken
    , dsrrsSubnets
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSubnets' smart constructor.
data DescribeSubnets = DescribeSubnets'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , subnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ One or more subnet IDs.
--
-- Default: Describes all your subnets.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubnets' value with any optional fields omitted.
mkDescribeSubnets
    :: DescribeSubnets
mkDescribeSubnets
  = DescribeSubnets'{dryRun = Core.Nothing, filters = Core.Nothing,
                     maxResults = Core.Nothing, nextToken = Core.Nothing,
                     subnetIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDryRun :: Lens.Lens' DescribeSubnets (Core.Maybe Core.Bool)
dsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
dsFilters :: Lens.Lens' DescribeSubnets (Core.Maybe [Types.Filter])
dsFilters = Lens.field @"filters"
{-# INLINEABLE dsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxResults :: Lens.Lens' DescribeSubnets (Core.Maybe Core.Natural)
dsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSubnets (Core.Maybe Core.Text)
dsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more subnet IDs.
--
-- Default: Describes all your subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSubnetIds :: Lens.Lens' DescribeSubnets (Core.Maybe [Types.SubnetId])
dsSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE dsSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

instance Core.ToQuery DescribeSubnets where
        toQuery DescribeSubnets{..}
          = Core.toQueryPair "Action" ("DescribeSubnets" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SubnetId") subnetIds

instance Core.ToHeaders DescribeSubnets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSubnets where
        type Rs DescribeSubnets = DescribeSubnetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeSubnetsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "subnetSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSubnets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"subnets" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeSubnetsResponse' smart constructor.
data DescribeSubnetsResponse = DescribeSubnetsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , subnets :: Core.Maybe [Types.Subnet]
    -- ^ Information about one or more subnets.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubnetsResponse' value with any optional fields omitted.
mkDescribeSubnetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSubnetsResponse
mkDescribeSubnetsResponse responseStatus
  = DescribeSubnetsResponse'{nextToken = Core.Nothing,
                             subnets = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsNextToken :: Lens.Lens' DescribeSubnetsResponse (Core.Maybe Core.Text)
dsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about one or more subnets.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSubnets :: Lens.Lens' DescribeSubnetsResponse (Core.Maybe [Types.Subnet])
dsrrsSubnets = Lens.field @"subnets"
{-# INLINEABLE dsrrsSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSubnetsResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
