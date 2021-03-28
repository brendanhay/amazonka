{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC endpoints.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpoints
    (
    -- * Creating a request
      DescribeVpcEndpoints (..)
    , mkDescribeVpcEndpoints
    -- ** Request lenses
    , dvefDryRun
    , dvefFilters
    , dvefMaxResults
    , dvefNextToken
    , dvefVpcEndpointIds

    -- * Destructuring the response
    , DescribeVpcEndpointsResponse (..)
    , mkDescribeVpcEndpointsResponse
    -- ** Response lenses
    , dverfrsNextToken
    , dverfrsVpcEndpoints
    , dverfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeVpcEndpoints.
--
-- /See:/ 'mkDescribeVpcEndpoints' smart constructor.
data DescribeVpcEndpoints = DescribeVpcEndpoints'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @vpc-id@ - The ID of the VPC in which the endpoint resides.
--
--
--     * @vpc-endpoint-id@ - The ID of the endpoint.
--
--
--     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
--
--
--     * @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ | @Gateway@ | @GatewayLoadBalancer@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000 items.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of items to return. (You received this token from a prior call.)
  , vpcEndpointIds :: Core.Maybe [Types.VpcEndpointId]
    -- ^ One or more endpoint IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpoints' value with any optional fields omitted.
mkDescribeVpcEndpoints
    :: DescribeVpcEndpoints
mkDescribeVpcEndpoints
  = DescribeVpcEndpoints'{dryRun = Core.Nothing,
                          filters = Core.Nothing, maxResults = Core.Nothing,
                          nextToken = Core.Nothing, vpcEndpointIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvefDryRun :: Lens.Lens' DescribeVpcEndpoints (Core.Maybe Core.Bool)
dvefDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvefDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @vpc-id@ - The ID of the VPC in which the endpoint resides.
--
--
--     * @vpc-endpoint-id@ - The ID of the endpoint.
--
--
--     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
--
--
--     * @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ | @Gateway@ | @GatewayLoadBalancer@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvefFilters :: Lens.Lens' DescribeVpcEndpoints (Core.Maybe [Types.Filter])
dvefFilters = Lens.field @"filters"
{-# INLINEABLE dvefFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000 items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvefMaxResults :: Lens.Lens' DescribeVpcEndpoints (Core.Maybe Core.Int)
dvefMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvefMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a prior call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvefNextToken :: Lens.Lens' DescribeVpcEndpoints (Core.Maybe Core.Text)
dvefNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvefNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | One or more endpoint IDs.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvefVpcEndpointIds :: Lens.Lens' DescribeVpcEndpoints (Core.Maybe [Types.VpcEndpointId])
dvefVpcEndpointIds = Lens.field @"vpcEndpointIds"
{-# INLINEABLE dvefVpcEndpointIds #-}
{-# DEPRECATED vpcEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead"  #-}

instance Core.ToQuery DescribeVpcEndpoints where
        toQuery DescribeVpcEndpoints{..}
          = Core.toQueryPair "Action" ("DescribeVpcEndpoints" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "VpcEndpointId")
                vpcEndpointIds

instance Core.ToHeaders DescribeVpcEndpoints where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcEndpoints where
        type Rs DescribeVpcEndpoints = DescribeVpcEndpointsResponse
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
                 DescribeVpcEndpointsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "vpcEndpointSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcEndpoints where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"vpcEndpoints" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output of DescribeVpcEndpoints.
--
-- /See:/ 'mkDescribeVpcEndpointsResponse' smart constructor.
data DescribeVpcEndpointsResponse = DescribeVpcEndpointsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , vpcEndpoints :: Core.Maybe [Types.VpcEndpoint]
    -- ^ Information about the endpoints.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeVpcEndpointsResponse' value with any optional fields omitted.
mkDescribeVpcEndpointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcEndpointsResponse
mkDescribeVpcEndpointsResponse responseStatus
  = DescribeVpcEndpointsResponse'{nextToken = Core.Nothing,
                                  vpcEndpoints = Core.Nothing, responseStatus}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dverfrsNextToken :: Lens.Lens' DescribeVpcEndpointsResponse (Core.Maybe Core.Text)
dverfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dverfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the endpoints.
--
-- /Note:/ Consider using 'vpcEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dverfrsVpcEndpoints :: Lens.Lens' DescribeVpcEndpointsResponse (Core.Maybe [Types.VpcEndpoint])
dverfrsVpcEndpoints = Lens.field @"vpcEndpoints"
{-# INLINEABLE dverfrsVpcEndpoints #-}
{-# DEPRECATED vpcEndpoints "Use generic-lens or generic-optics with 'vpcEndpoints' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dverfrsResponseStatus :: Lens.Lens' DescribeVpcEndpointsResponse Core.Int
dverfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dverfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
