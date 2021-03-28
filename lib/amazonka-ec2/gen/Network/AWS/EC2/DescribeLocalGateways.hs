{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more local gateways. By default, all local gateways are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGateways
    (
    -- * Creating a request
      DescribeLocalGateways (..)
    , mkDescribeLocalGateways
    -- ** Request lenses
    , dlgDryRun
    , dlgFilters
    , dlgLocalGatewayIds
    , dlgMaxResults
    , dlgNextToken

    -- * Destructuring the response
    , DescribeLocalGatewaysResponse (..)
    , mkDescribeLocalGatewaysResponse
    -- ** Response lenses
    , dlgrrsLocalGateways
    , dlgrrsNextToken
    , dlgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLocalGateways' smart constructor.
data DescribeLocalGateways = DescribeLocalGateways'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
  , localGatewayIds :: Core.Maybe [Types.LocalGatewayId]
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
--     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
--
--     * @state@ - The state of the association.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGateways' value with any optional fields omitted.
mkDescribeLocalGateways
    :: DescribeLocalGateways
mkDescribeLocalGateways
  = DescribeLocalGateways'{dryRun = Core.Nothing,
                           filters = Core.Nothing, localGatewayIds = Core.Nothing,
                           maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgDryRun :: Lens.Lens' DescribeLocalGateways (Core.Maybe Core.Bool)
dlgDryRun = Lens.field @"dryRun"
{-# INLINEABLE dlgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgFilters :: Lens.Lens' DescribeLocalGateways (Core.Maybe [Types.Filter])
dlgFilters = Lens.field @"filters"
{-# INLINEABLE dlgFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

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
--     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
--
--     * @state@ - The state of the association.
--
--
--
-- /Note:/ Consider using 'localGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgLocalGatewayIds :: Lens.Lens' DescribeLocalGateways (Core.Maybe [Types.LocalGatewayId])
dlgLocalGatewayIds = Lens.field @"localGatewayIds"
{-# INLINEABLE dlgLocalGatewayIds #-}
{-# DEPRECATED localGatewayIds "Use generic-lens or generic-optics with 'localGatewayIds' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgMaxResults :: Lens.Lens' DescribeLocalGateways (Core.Maybe Core.Natural)
dlgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dlgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgNextToken :: Lens.Lens' DescribeLocalGateways (Core.Maybe Core.Text)
dlgNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeLocalGateways where
        toQuery DescribeLocalGateways{..}
          = Core.toQueryPair "Action" ("DescribeLocalGateways" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "LocalGatewayId")
                localGatewayIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeLocalGateways where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeLocalGateways where
        type Rs DescribeLocalGateways = DescribeLocalGatewaysResponse
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
                 DescribeLocalGatewaysResponse' Core.<$>
                   (x Core..@? "localGatewaySet" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeLocalGateways where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"localGateways" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeLocalGatewaysResponse' smart constructor.
data DescribeLocalGatewaysResponse = DescribeLocalGatewaysResponse'
  { localGateways :: Core.Maybe [Types.LocalGateway]
    -- ^ Information about the local gateways.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLocalGatewaysResponse' value with any optional fields omitted.
mkDescribeLocalGatewaysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLocalGatewaysResponse
mkDescribeLocalGatewaysResponse responseStatus
  = DescribeLocalGatewaysResponse'{localGateways = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | Information about the local gateways.
--
-- /Note:/ Consider using 'localGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrsLocalGateways :: Lens.Lens' DescribeLocalGatewaysResponse (Core.Maybe [Types.LocalGateway])
dlgrrsLocalGateways = Lens.field @"localGateways"
{-# INLINEABLE dlgrrsLocalGateways #-}
{-# DEPRECATED localGateways "Use generic-lens or generic-optics with 'localGateways' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrsNextToken :: Lens.Lens' DescribeLocalGatewaysResponse (Core.Maybe Core.Text)
dlgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrrsResponseStatus :: Lens.Lens' DescribeLocalGatewaysResponse Core.Int
dlgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
