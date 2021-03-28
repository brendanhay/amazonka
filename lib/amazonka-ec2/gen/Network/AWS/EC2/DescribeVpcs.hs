{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPCs.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcs
    (
    -- * Creating a request
      DescribeVpcs (..)
    , mkDescribeVpcs
    -- ** Request lenses
    , dvsDryRun
    , dvsFilters
    , dvsMaxResults
    , dvsNextToken
    , dvsVpcIds

    -- * Destructuring the response
    , DescribeVpcsResponse (..)
    , mkDescribeVpcsResponse
    -- ** Response lenses
    , dvrrsNextToken
    , dvrrsVpcs
    , dvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcs' smart constructor.
data DescribeVpcs = DescribeVpcs'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , vpcIds :: Core.Maybe [Types.VpcId]
    -- ^ One or more VPC IDs.
--
-- Default: Describes all your VPCs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcs' value with any optional fields omitted.
mkDescribeVpcs
    :: DescribeVpcs
mkDescribeVpcs
  = DescribeVpcs'{dryRun = Core.Nothing, filters = Core.Nothing,
                  maxResults = Core.Nothing, nextToken = Core.Nothing,
                  vpcIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsDryRun :: Lens.Lens' DescribeVpcs (Core.Maybe Core.Bool)
dvsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
dvsFilters :: Lens.Lens' DescribeVpcs (Core.Maybe [Types.Filter])
dvsFilters = Lens.field @"filters"
{-# INLINEABLE dvsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsMaxResults :: Lens.Lens' DescribeVpcs (Core.Maybe Core.Natural)
dvsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsNextToken :: Lens.Lens' DescribeVpcs (Core.Maybe Core.Text)
dvsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more VPC IDs.
--
-- Default: Describes all your VPCs.
--
-- /Note:/ Consider using 'vpcIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsVpcIds :: Lens.Lens' DescribeVpcs (Core.Maybe [Types.VpcId])
dvsVpcIds = Lens.field @"vpcIds"
{-# INLINEABLE dvsVpcIds #-}
{-# DEPRECATED vpcIds "Use generic-lens or generic-optics with 'vpcIds' instead"  #-}

instance Core.ToQuery DescribeVpcs where
        toQuery DescribeVpcs{..}
          = Core.toQueryPair "Action" ("DescribeVpcs" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryList "VpcId") vpcIds

instance Core.ToHeaders DescribeVpcs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcs where
        type Rs DescribeVpcs = DescribeVpcsResponse
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
                 DescribeVpcsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "vpcSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"vpcs" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVpcsResponse' smart constructor.
data DescribeVpcsResponse = DescribeVpcsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , vpcs :: Core.Maybe [Types.Vpc]
    -- ^ Information about one or more VPCs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcsResponse' value with any optional fields omitted.
mkDescribeVpcsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcsResponse
mkDescribeVpcsResponse responseStatus
  = DescribeVpcsResponse'{nextToken = Core.Nothing,
                          vpcs = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsNextToken :: Lens.Lens' DescribeVpcsResponse (Core.Maybe Core.Text)
dvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about one or more VPCs.
--
-- /Note:/ Consider using 'vpcs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsVpcs :: Lens.Lens' DescribeVpcsResponse (Core.Maybe [Types.Vpc])
dvrrsVpcs = Lens.field @"vpcs"
{-# INLINEABLE dvrrsVpcs #-}
{-# DEPRECATED vpcs "Use generic-lens or generic-optics with 'vpcs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrrsResponseStatus :: Lens.Lens' DescribeVpcsResponse Core.Int
dvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
