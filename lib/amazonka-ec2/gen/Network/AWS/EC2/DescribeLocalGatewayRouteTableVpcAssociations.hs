{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTableVpcAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified associations between VPCs and local gateway route tables.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTableVpcAssociations
    (
    -- * Creating a request
      DescribeLocalGatewayRouteTableVpcAssociations (..)
    , mkDescribeLocalGatewayRouteTableVpcAssociations
    -- ** Request lenses
    , dlgrtvasDryRun
    , dlgrtvasFilters
    , dlgrtvasLocalGatewayRouteTableVpcAssociationIds
    , dlgrtvasMaxResults
    , dlgrtvasNextToken

    -- * Destructuring the response
    , DescribeLocalGatewayRouteTableVpcAssociationsResponse (..)
    , mkDescribeLocalGatewayRouteTableVpcAssociationsResponse
    -- ** Response lenses
    , dlgrtvarfrsLocalGatewayRouteTableVpcAssociations
    , dlgrtvarfrsNextToken
    , dlgrtvarfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVpcAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVpcAssociations = DescribeLocalGatewayRouteTableVpcAssociations'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-vpc-association-id@ - The ID of the association.
--
--
--     * @state@ - The state of the association.
--
--
--     * @vpc-id@ - The ID of the VPC.
--
--
  , localGatewayRouteTableVpcAssociationIds :: Core.Maybe [Types.LocalGatewayRouteTableVpcAssociationId]
    -- ^ The IDs of the associations.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayRouteTableVpcAssociations' value with any optional fields omitted.
mkDescribeLocalGatewayRouteTableVpcAssociations
    :: DescribeLocalGatewayRouteTableVpcAssociations
mkDescribeLocalGatewayRouteTableVpcAssociations
  = DescribeLocalGatewayRouteTableVpcAssociations'{dryRun =
                                                     Core.Nothing,
                                                   filters = Core.Nothing,
                                                   localGatewayRouteTableVpcAssociationIds =
                                                     Core.Nothing,
                                                   maxResults = Core.Nothing,
                                                   nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvasDryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Core.Maybe Core.Bool)
dlgrtvasDryRun = Lens.field @"dryRun"
{-# INLINEABLE dlgrtvasDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-vpc-association-id@ - The ID of the association.
--
--
--     * @state@ - The state of the association.
--
--
--     * @vpc-id@ - The ID of the VPC.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvasFilters :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Core.Maybe [Types.Filter])
dlgrtvasFilters = Lens.field @"filters"
{-# INLINEABLE dlgrtvasFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The IDs of the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVpcAssociationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvasLocalGatewayRouteTableVpcAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Core.Maybe [Types.LocalGatewayRouteTableVpcAssociationId])
dlgrtvasLocalGatewayRouteTableVpcAssociationIds = Lens.field @"localGatewayRouteTableVpcAssociationIds"
{-# INLINEABLE dlgrtvasLocalGatewayRouteTableVpcAssociationIds #-}
{-# DEPRECATED localGatewayRouteTableVpcAssociationIds "Use generic-lens or generic-optics with 'localGatewayRouteTableVpcAssociationIds' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvasMaxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Core.Maybe Core.Natural)
dlgrtvasMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dlgrtvasMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvasNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Core.Maybe Core.Text)
dlgrtvasNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgrtvasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeLocalGatewayRouteTableVpcAssociations
         where
        toQuery DescribeLocalGatewayRouteTableVpcAssociations{..}
          = Core.toQueryPair "Action"
              ("DescribeLocalGatewayRouteTableVpcAssociations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList "LocalGatewayRouteTableVpcAssociationId")
                localGatewayRouteTableVpcAssociationIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders
           DescribeLocalGatewayRouteTableVpcAssociations
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           DescribeLocalGatewayRouteTableVpcAssociations
         where
        type Rs DescribeLocalGatewayRouteTableVpcAssociations =
             DescribeLocalGatewayRouteTableVpcAssociationsResponse
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
                 DescribeLocalGatewayRouteTableVpcAssociationsResponse' Core.<$>
                   (x Core..@? "localGatewayRouteTableVpcAssociationSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager
           DescribeLocalGatewayRouteTableVpcAssociations
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"localGatewayRouteTableVpcAssociations" Core..
                   Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVpcAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVpcAssociationsResponse = DescribeLocalGatewayRouteTableVpcAssociationsResponse'
  { localGatewayRouteTableVpcAssociations :: Core.Maybe [Types.LocalGatewayRouteTableVpcAssociation]
    -- ^ Information about the associations.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayRouteTableVpcAssociationsResponse' value with any optional fields omitted.
mkDescribeLocalGatewayRouteTableVpcAssociationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLocalGatewayRouteTableVpcAssociationsResponse
mkDescribeLocalGatewayRouteTableVpcAssociationsResponse
  responseStatus
  = DescribeLocalGatewayRouteTableVpcAssociationsResponse'{localGatewayRouteTableVpcAssociations
                                                             = Core.Nothing,
                                                           nextToken = Core.Nothing, responseStatus}

-- | Information about the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVpcAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvarfrsLocalGatewayRouteTableVpcAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse (Core.Maybe [Types.LocalGatewayRouteTableVpcAssociation])
dlgrtvarfrsLocalGatewayRouteTableVpcAssociations = Lens.field @"localGatewayRouteTableVpcAssociations"
{-# INLINEABLE dlgrtvarfrsLocalGatewayRouteTableVpcAssociations #-}
{-# DEPRECATED localGatewayRouteTableVpcAssociations "Use generic-lens or generic-optics with 'localGatewayRouteTableVpcAssociations' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvarfrsNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse (Core.Maybe Core.Text)
dlgrtvarfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgrtvarfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvarfrsResponseStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse Core.Int
dlgrtvarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlgrtvarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
