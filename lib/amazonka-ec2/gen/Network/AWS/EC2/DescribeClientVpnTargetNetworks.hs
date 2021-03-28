{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVpnTargetNetworks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the target networks associated with the specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnTargetNetworks
    (
    -- * Creating a request
      DescribeClientVpnTargetNetworks (..)
    , mkDescribeClientVpnTargetNetworks
    -- ** Request lenses
    , dcvtnClientVpnEndpointId
    , dcvtnAssociationIds
    , dcvtnDryRun
    , dcvtnFilters
    , dcvtnMaxResults
    , dcvtnNextToken

    -- * Destructuring the response
    , DescribeClientVpnTargetNetworksResponse (..)
    , mkDescribeClientVpnTargetNetworksResponse
    -- ** Response lenses
    , dcvtnrrsClientVpnTargetNetworks
    , dcvtnrrsNextToken
    , dcvtnrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClientVpnTargetNetworks' smart constructor.
data DescribeClientVpnTargetNetworks = DescribeClientVpnTargetNetworks'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , associationIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of the target network associations.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters. Filter names and values are case-sensitive.
--
--
--     * @association-id@ - The ID of the association.
--
--
--     * @target-network-id@ - The ID of the subnet specified as the target network.
--
--
--     * @vpc-id@ - The ID of the VPC in which the target network is located.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnTargetNetworks' value with any optional fields omitted.
mkDescribeClientVpnTargetNetworks
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> DescribeClientVpnTargetNetworks
mkDescribeClientVpnTargetNetworks clientVpnEndpointId
  = DescribeClientVpnTargetNetworks'{clientVpnEndpointId,
                                     associationIds = Core.Nothing, dryRun = Core.Nothing,
                                     filters = Core.Nothing, maxResults = Core.Nothing,
                                     nextToken = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnClientVpnEndpointId :: Lens.Lens' DescribeClientVpnTargetNetworks Types.ClientVpnEndpointId
dcvtnClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE dcvtnClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The IDs of the target network associations.
--
-- /Note:/ Consider using 'associationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnAssociationIds :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe [Core.Text])
dcvtnAssociationIds = Lens.field @"associationIds"
{-# INLINEABLE dcvtnAssociationIds #-}
{-# DEPRECATED associationIds "Use generic-lens or generic-optics with 'associationIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnDryRun :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe Core.Bool)
dcvtnDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcvtnDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @association-id@ - The ID of the association.
--
--
--     * @target-network-id@ - The ID of the subnet specified as the target network.
--
--
--     * @vpc-id@ - The ID of the VPC in which the target network is located.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnFilters :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe [Types.Filter])
dcvtnFilters = Lens.field @"filters"
{-# INLINEABLE dcvtnFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnMaxResults :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe Core.Natural)
dcvtnMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dcvtnMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnNextToken :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe Types.NextToken)
dcvtnNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcvtnNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeClientVpnTargetNetworks where
        toQuery DescribeClientVpnTargetNetworks{..}
          = Core.toQueryPair "Action"
              ("DescribeClientVpnTargetNetworks" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AssociationIds")
                associationIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeClientVpnTargetNetworks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeClientVpnTargetNetworks where
        type Rs DescribeClientVpnTargetNetworks =
             DescribeClientVpnTargetNetworksResponse
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
                 DescribeClientVpnTargetNetworksResponse' Core.<$>
                   (x Core..@? "clientVpnTargetNetworks" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeClientVpnTargetNetworks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"clientVpnTargetNetworks" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeClientVpnTargetNetworksResponse' smart constructor.
data DescribeClientVpnTargetNetworksResponse = DescribeClientVpnTargetNetworksResponse'
  { clientVpnTargetNetworks :: Core.Maybe [Types.TargetNetwork]
    -- ^ Information about the associated target networks.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientVpnTargetNetworksResponse' value with any optional fields omitted.
mkDescribeClientVpnTargetNetworksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClientVpnTargetNetworksResponse
mkDescribeClientVpnTargetNetworksResponse responseStatus
  = DescribeClientVpnTargetNetworksResponse'{clientVpnTargetNetworks
                                               = Core.Nothing,
                                             nextToken = Core.Nothing, responseStatus}

-- | Information about the associated target networks.
--
-- /Note:/ Consider using 'clientVpnTargetNetworks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrrsClientVpnTargetNetworks :: Lens.Lens' DescribeClientVpnTargetNetworksResponse (Core.Maybe [Types.TargetNetwork])
dcvtnrrsClientVpnTargetNetworks = Lens.field @"clientVpnTargetNetworks"
{-# INLINEABLE dcvtnrrsClientVpnTargetNetworks #-}
{-# DEPRECATED clientVpnTargetNetworks "Use generic-lens or generic-optics with 'clientVpnTargetNetworks' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrrsNextToken :: Lens.Lens' DescribeClientVpnTargetNetworksResponse (Core.Maybe Types.NextToken)
dcvtnrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcvtnrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrrsResponseStatus :: Lens.Lens' DescribeClientVpnTargetNetworksResponse Core.Int
dcvtnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcvtnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
