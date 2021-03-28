{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcPeeringConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC peering connections.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcPeeringConnections
    (
    -- * Creating a request
      DescribeVpcPeeringConnections (..)
    , mkDescribeVpcPeeringConnections
    -- ** Request lenses
    , dvpcsDryRun
    , dvpcsFilters
    , dvpcsMaxResults
    , dvpcsNextToken
    , dvpcsVpcPeeringConnectionIds

    -- * Destructuring the response
    , DescribeVpcPeeringConnectionsResponse (..)
    , mkDescribeVpcPeeringConnectionsResponse
    -- ** Response lenses
    , dvpcrfrsNextToken
    , dvpcrfrsVpcPeeringConnections
    , dvpcrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcPeeringConnections' smart constructor.
data DescribeVpcPeeringConnections = DescribeVpcPeeringConnections'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  , vpcPeeringConnectionIds :: Core.Maybe [Types.VpcPeeringConnectionId]
    -- ^ One or more VPC peering connection IDs.
--
-- Default: Describes all your VPC peering connections.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcPeeringConnections' value with any optional fields omitted.
mkDescribeVpcPeeringConnections
    :: DescribeVpcPeeringConnections
mkDescribeVpcPeeringConnections
  = DescribeVpcPeeringConnections'{dryRun = Core.Nothing,
                                   filters = Core.Nothing, maxResults = Core.Nothing,
                                   nextToken = Core.Nothing, vpcPeeringConnectionIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcsDryRun :: Lens.Lens' DescribeVpcPeeringConnections (Core.Maybe Core.Bool)
dvpcsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvpcsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
dvpcsFilters :: Lens.Lens' DescribeVpcPeeringConnections (Core.Maybe [Types.Filter])
dvpcsFilters = Lens.field @"filters"
{-# INLINEABLE dvpcsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcsMaxResults :: Lens.Lens' DescribeVpcPeeringConnections (Core.Maybe Core.Natural)
dvpcsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvpcsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcsNextToken :: Lens.Lens' DescribeVpcPeeringConnections (Core.Maybe Core.Text)
dvpcsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvpcsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more VPC peering connection IDs.
--
-- Default: Describes all your VPC peering connections.
--
-- /Note:/ Consider using 'vpcPeeringConnectionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcsVpcPeeringConnectionIds :: Lens.Lens' DescribeVpcPeeringConnections (Core.Maybe [Types.VpcPeeringConnectionId])
dvpcsVpcPeeringConnectionIds = Lens.field @"vpcPeeringConnectionIds"
{-# INLINEABLE dvpcsVpcPeeringConnectionIds #-}
{-# DEPRECATED vpcPeeringConnectionIds "Use generic-lens or generic-optics with 'vpcPeeringConnectionIds' instead"  #-}

instance Core.ToQuery DescribeVpcPeeringConnections where
        toQuery DescribeVpcPeeringConnections{..}
          = Core.toQueryPair "Action"
              ("DescribeVpcPeeringConnections" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "VpcPeeringConnectionId")
                vpcPeeringConnectionIds

instance Core.ToHeaders DescribeVpcPeeringConnections where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcPeeringConnections where
        type Rs DescribeVpcPeeringConnections =
             DescribeVpcPeeringConnectionsResponse
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
                 DescribeVpcPeeringConnectionsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "vpcPeeringConnectionSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcPeeringConnections where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"vpcPeeringConnections" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVpcPeeringConnectionsResponse' smart constructor.
data DescribeVpcPeeringConnectionsResponse = DescribeVpcPeeringConnectionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , vpcPeeringConnections :: Core.Maybe [Types.VpcPeeringConnection]
    -- ^ Information about the VPC peering connections.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeVpcPeeringConnectionsResponse' value with any optional fields omitted.
mkDescribeVpcPeeringConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcPeeringConnectionsResponse
mkDescribeVpcPeeringConnectionsResponse responseStatus
  = DescribeVpcPeeringConnectionsResponse'{nextToken = Core.Nothing,
                                           vpcPeeringConnections = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrfrsNextToken :: Lens.Lens' DescribeVpcPeeringConnectionsResponse (Core.Maybe Core.Text)
dvpcrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvpcrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the VPC peering connections.
--
-- /Note:/ Consider using 'vpcPeeringConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrfrsVpcPeeringConnections :: Lens.Lens' DescribeVpcPeeringConnectionsResponse (Core.Maybe [Types.VpcPeeringConnection])
dvpcrfrsVpcPeeringConnections = Lens.field @"vpcPeeringConnections"
{-# INLINEABLE dvpcrfrsVpcPeeringConnections #-}
{-# DEPRECATED vpcPeeringConnections "Use generic-lens or generic-optics with 'vpcPeeringConnections' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrfrsResponseStatus :: Lens.Lens' DescribeVpcPeeringConnectionsResponse Core.Int
dvpcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvpcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
