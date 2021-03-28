{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcEndpointConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint connections to your VPC endpoint services, including any endpoints that are pending your acceptance.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointConnections
    (
    -- * Creating a request
      DescribeVpcEndpointConnections (..)
    , mkDescribeVpcEndpointConnections
    -- ** Request lenses
    , dvecDryRun
    , dvecFilters
    , dvecMaxResults
    , dvecNextToken

    -- * Destructuring the response
    , DescribeVpcEndpointConnectionsResponse (..)
    , mkDescribeVpcEndpointConnectionsResponse
    -- ** Response lenses
    , dvecrrsNextToken
    , dvecrrsVpcEndpointConnections
    , dvecrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcEndpointConnections' smart constructor.
data DescribeVpcEndpointConnections = DescribeVpcEndpointConnections'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @service-id@ - The ID of the service.
--
--
--     * @vpc-endpoint-owner@ - The AWS account number of the owner of the endpoint.
--
--
--     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
--
--
--     * @vpc-endpoint-id@ - The ID of the endpoint.
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointConnections' value with any optional fields omitted.
mkDescribeVpcEndpointConnections
    :: DescribeVpcEndpointConnections
mkDescribeVpcEndpointConnections
  = DescribeVpcEndpointConnections'{dryRun = Core.Nothing,
                                    filters = Core.Nothing, maxResults = Core.Nothing,
                                    nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecDryRun :: Lens.Lens' DescribeVpcEndpointConnections (Core.Maybe Core.Bool)
dvecDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvecDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @service-id@ - The ID of the service.
--
--
--     * @vpc-endpoint-owner@ - The AWS account number of the owner of the endpoint.
--
--
--     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
--
--
--     * @vpc-endpoint-id@ - The ID of the endpoint.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecFilters :: Lens.Lens' DescribeVpcEndpointConnections (Core.Maybe [Types.Filter])
dvecFilters = Lens.field @"filters"
{-# INLINEABLE dvecFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecMaxResults :: Lens.Lens' DescribeVpcEndpointConnections (Core.Maybe Core.Int)
dvecMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvecMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecNextToken :: Lens.Lens' DescribeVpcEndpointConnections (Core.Maybe Core.Text)
dvecNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvecNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeVpcEndpointConnections where
        toQuery DescribeVpcEndpointConnections{..}
          = Core.toQueryPair "Action"
              ("DescribeVpcEndpointConnections" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeVpcEndpointConnections where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcEndpointConnections where
        type Rs DescribeVpcEndpointConnections =
             DescribeVpcEndpointConnectionsResponse
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
                 DescribeVpcEndpointConnectionsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "vpcEndpointConnectionSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcEndpointConnections where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"vpcEndpointConnections" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVpcEndpointConnectionsResponse' smart constructor.
data DescribeVpcEndpointConnectionsResponse = DescribeVpcEndpointConnectionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , vpcEndpointConnections :: Core.Maybe [Types.VpcEndpointConnection]
    -- ^ Information about one or more VPC endpoint connections.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeVpcEndpointConnectionsResponse' value with any optional fields omitted.
mkDescribeVpcEndpointConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcEndpointConnectionsResponse
mkDescribeVpcEndpointConnectionsResponse responseStatus
  = DescribeVpcEndpointConnectionsResponse'{nextToken = Core.Nothing,
                                            vpcEndpointConnections = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecrrsNextToken :: Lens.Lens' DescribeVpcEndpointConnectionsResponse (Core.Maybe Core.Text)
dvecrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvecrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about one or more VPC endpoint connections.
--
-- /Note:/ Consider using 'vpcEndpointConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecrrsVpcEndpointConnections :: Lens.Lens' DescribeVpcEndpointConnectionsResponse (Core.Maybe [Types.VpcEndpointConnection])
dvecrrsVpcEndpointConnections = Lens.field @"vpcEndpointConnections"
{-# INLINEABLE dvecrrsVpcEndpointConnections #-}
{-# DEPRECATED vpcEndpointConnections "Use generic-lens or generic-optics with 'vpcEndpointConnections' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecrrsResponseStatus :: Lens.Lens' DescribeVpcEndpointConnectionsResponse Core.Int
dvecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
