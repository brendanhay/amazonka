{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNetworkAcls
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
module Network.AWS.EC2.DescribeNetworkAcls
    (
    -- * Creating a request
      DescribeNetworkAcls (..)
    , mkDescribeNetworkAcls
    -- ** Request lenses
    , dnasDryRun
    , dnasFilters
    , dnasMaxResults
    , dnasNetworkAclIds
    , dnasNextToken

    -- * Destructuring the response
    , DescribeNetworkAclsResponse (..)
    , mkDescribeNetworkAclsResponse
    -- ** Response lenses
    , dnarrsNetworkAcls
    , dnarrsNextToken
    , dnarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeNetworkAcls' smart constructor.
data DescribeNetworkAcls = DescribeNetworkAcls'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , networkAclIds :: Core.Maybe [Types.NetworkAclId]
    -- ^ One or more network ACL IDs.
--
-- Default: Describes all your network ACLs.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNetworkAcls' value with any optional fields omitted.
mkDescribeNetworkAcls
    :: DescribeNetworkAcls
mkDescribeNetworkAcls
  = DescribeNetworkAcls'{dryRun = Core.Nothing,
                         filters = Core.Nothing, maxResults = Core.Nothing,
                         networkAclIds = Core.Nothing, nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasDryRun :: Lens.Lens' DescribeNetworkAcls (Core.Maybe Core.Bool)
dnasDryRun = Lens.field @"dryRun"
{-# INLINEABLE dnasDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
dnasFilters :: Lens.Lens' DescribeNetworkAcls (Core.Maybe [Types.Filter])
dnasFilters = Lens.field @"filters"
{-# INLINEABLE dnasFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasMaxResults :: Lens.Lens' DescribeNetworkAcls (Core.Maybe Core.Natural)
dnasMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dnasMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | One or more network ACL IDs.
--
-- Default: Describes all your network ACLs.
--
-- /Note:/ Consider using 'networkAclIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasNetworkAclIds :: Lens.Lens' DescribeNetworkAcls (Core.Maybe [Types.NetworkAclId])
dnasNetworkAclIds = Lens.field @"networkAclIds"
{-# INLINEABLE dnasNetworkAclIds #-}
{-# DEPRECATED networkAclIds "Use generic-lens or generic-optics with 'networkAclIds' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnasNextToken :: Lens.Lens' DescribeNetworkAcls (Core.Maybe Core.Text)
dnasNextToken = Lens.field @"nextToken"
{-# INLINEABLE dnasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeNetworkAcls where
        toQuery DescribeNetworkAcls{..}
          = Core.toQueryPair "Action" ("DescribeNetworkAcls" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "NetworkAclId")
                networkAclIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeNetworkAcls where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeNetworkAcls where
        type Rs DescribeNetworkAcls = DescribeNetworkAclsResponse
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
                 DescribeNetworkAclsResponse' Core.<$>
                   (x Core..@? "networkAclSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeNetworkAcls where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"networkAcls" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeNetworkAclsResponse' smart constructor.
data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse'
  { networkAcls :: Core.Maybe [Types.NetworkAcl]
    -- ^ Information about one or more network ACLs.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNetworkAclsResponse' value with any optional fields omitted.
mkDescribeNetworkAclsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNetworkAclsResponse
mkDescribeNetworkAclsResponse responseStatus
  = DescribeNetworkAclsResponse'{networkAcls = Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | Information about one or more network ACLs.
--
-- /Note:/ Consider using 'networkAcls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnarrsNetworkAcls :: Lens.Lens' DescribeNetworkAclsResponse (Core.Maybe [Types.NetworkAcl])
dnarrsNetworkAcls = Lens.field @"networkAcls"
{-# INLINEABLE dnarrsNetworkAcls #-}
{-# DEPRECATED networkAcls "Use generic-lens or generic-optics with 'networkAcls' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnarrsNextToken :: Lens.Lens' DescribeNetworkAclsResponse (Core.Maybe Core.Text)
dnarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dnarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnarrsResponseStatus :: Lens.Lens' DescribeNetworkAclsResponse Core.Int
dnarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
