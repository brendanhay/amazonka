{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the associations between virtual interface groups and local gateway route tables.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
    (
    -- * Creating a request
      DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (..)
    , mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
    -- ** Request lenses
    , dlgrtvigaDryRun
    , dlgrtvigaFilters
    , dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds
    , dlgrtvigaMaxResults
    , dlgrtvigaNextToken

    -- * Destructuring the response
    , DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (..)
    , mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
    -- ** Response lenses
    , dlgrtvigarrsLocalGatewayRouteTableVirtualInterfaceGroupAssociations
    , dlgrtvigarrsNextToken
    , dlgrtvigarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
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
--     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
--
--
--     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--     * @state@ - The state of the association.
--
--
  , localGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Core.Maybe [Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociationId]
    -- ^ The IDs of the associations.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' value with any optional fields omitted.
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
    :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'{dryRun
                                                                       = Core.Nothing,
                                                                     filters = Core.Nothing,
                                                                     localGatewayRouteTableVirtualInterfaceGroupAssociationIds
                                                                       = Core.Nothing,
                                                                     maxResults = Core.Nothing,
                                                                     nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaDryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe Core.Bool)
dlgrtvigaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dlgrtvigaDryRun #-}
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
--     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
--
--
--     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--     * @state@ - The state of the association.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaFilters :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe [Types.Filter])
dlgrtvigaFilters = Lens.field @"filters"
{-# INLINEABLE dlgrtvigaFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The IDs of the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe [Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociationId])
dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds = Lens.field @"localGatewayRouteTableVirtualInterfaceGroupAssociationIds"
{-# INLINEABLE dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds #-}
{-# DEPRECATED localGatewayRouteTableVirtualInterfaceGroupAssociationIds "Use generic-lens or generic-optics with 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaMaxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe Core.Natural)
dlgrtvigaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dlgrtvigaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Core.Maybe Core.Text)
dlgrtvigaNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgrtvigaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery
           DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
         where
        toQuery
          DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations{..}
          = Core.toQueryPair "Action"
              ("DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations"
                 :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryList
                   "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId")
                localGatewayRouteTableVirtualInterfaceGroupAssociationIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders
           DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
         where
        type Rs
               DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
             =
             DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
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
                 DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
                   Core.<$>
                   (x Core..@?
                      "localGatewayRouteTableVirtualInterfaceGroupAssociationSet"
                      Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager
           DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field
                   @"localGatewayRouteTableVirtualInterfaceGroupAssociations"
                   Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
  { localGatewayRouteTableVirtualInterfaceGroupAssociations :: Core.Maybe [Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation]
    -- ^ Information about the associations.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' value with any optional fields omitted.
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  responseStatus
  = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'{localGatewayRouteTableVirtualInterfaceGroupAssociations
                                                                               = Core.Nothing,
                                                                             nextToken =
                                                                               Core.Nothing,
                                                                             responseStatus}

-- | Information about the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVirtualInterfaceGroupAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarrsLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Core.Maybe [Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation])
dlgrtvigarrsLocalGatewayRouteTableVirtualInterfaceGroupAssociations = Lens.field @"localGatewayRouteTableVirtualInterfaceGroupAssociations"
{-# INLINEABLE dlgrtvigarrsLocalGatewayRouteTableVirtualInterfaceGroupAssociations #-}
{-# DEPRECATED localGatewayRouteTableVirtualInterfaceGroupAssociations "Use generic-lens or generic-optics with 'localGatewayRouteTableVirtualInterfaceGroupAssociations' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarrsNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Core.Maybe Core.Text)
dlgrtvigarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgrtvigarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarrsResponseStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse Core.Int
dlgrtvigarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlgrtvigarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
