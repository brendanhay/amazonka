{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetCoipPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the allocations from the specified customer-owned address pool.
module Network.AWS.EC2.GetCoipPoolUsage
    (
    -- * Creating a request
      GetCoipPoolUsage (..)
    , mkGetCoipPoolUsage
    -- ** Request lenses
    , gcpuPoolId
    , gcpuDryRun
    , gcpuFilters
    , gcpuMaxResults
    , gcpuNextToken

    -- * Destructuring the response
    , GetCoipPoolUsageResponse (..)
    , mkGetCoipPoolUsageResponse
    -- ** Response lenses
    , gcpurrsCoipAddressUsages
    , gcpurrsCoipPoolId
    , gcpurrsLocalGatewayRouteTableId
    , gcpurrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCoipPoolUsage' smart constructor.
data GetCoipPoolUsage = GetCoipPoolUsage'
  { poolId :: Types.PoolId
    -- ^ The ID of the address pool.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ The filters. The following are the possible values:
--
--
--     * @coip-address-usage.allocation-id@ 
--
--
--
--     * @coip-address-usage.aws-account-id@ 
--
--
--
--     * @coip-address-usage.aws-service@ 
--
--
--
--     * @coip-address-usage.co-ip@ 
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCoipPoolUsage' value with any optional fields omitted.
mkGetCoipPoolUsage
    :: Types.PoolId -- ^ 'poolId'
    -> GetCoipPoolUsage
mkGetCoipPoolUsage poolId
  = GetCoipPoolUsage'{poolId, dryRun = Core.Nothing,
                      filters = Core.Nothing, maxResults = Core.Nothing,
                      nextToken = Core.Nothing}

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuPoolId :: Lens.Lens' GetCoipPoolUsage Types.PoolId
gcpuPoolId = Lens.field @"poolId"
{-# INLINEABLE gcpuPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuDryRun :: Lens.Lens' GetCoipPoolUsage (Core.Maybe Core.Bool)
gcpuDryRun = Lens.field @"dryRun"
{-# INLINEABLE gcpuDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The filters. The following are the possible values:
--
--
--     * @coip-address-usage.allocation-id@ 
--
--
--
--     * @coip-address-usage.aws-account-id@ 
--
--
--
--     * @coip-address-usage.aws-service@ 
--
--
--
--     * @coip-address-usage.co-ip@ 
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuFilters :: Lens.Lens' GetCoipPoolUsage (Core.Maybe [Types.Filter])
gcpuFilters = Lens.field @"filters"
{-# INLINEABLE gcpuFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuMaxResults :: Lens.Lens' GetCoipPoolUsage (Core.Maybe Core.Natural)
gcpuMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gcpuMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuNextToken :: Lens.Lens' GetCoipPoolUsage (Core.Maybe Core.Text)
gcpuNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcpuNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetCoipPoolUsage where
        toQuery GetCoipPoolUsage{..}
          = Core.toQueryPair "Action" ("GetCoipPoolUsage" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PoolId" poolId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders GetCoipPoolUsage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetCoipPoolUsage where
        type Rs GetCoipPoolUsage = GetCoipPoolUsageResponse
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
                 GetCoipPoolUsageResponse' Core.<$>
                   (x Core..@? "coipAddressUsageSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "coipPoolId"
                     Core.<*> x Core..@? "localGatewayRouteTableId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCoipPoolUsageResponse' smart constructor.
data GetCoipPoolUsageResponse = GetCoipPoolUsageResponse'
  { coipAddressUsages :: Core.Maybe [Types.CoipAddressUsage]
    -- ^ Information about the address usage.
  , coipPoolId :: Core.Maybe Core.Text
    -- ^ The ID of the customer-owned address pool.
  , localGatewayRouteTableId :: Core.Maybe Core.Text
    -- ^ The ID of the local gateway route table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCoipPoolUsageResponse' value with any optional fields omitted.
mkGetCoipPoolUsageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCoipPoolUsageResponse
mkGetCoipPoolUsageResponse responseStatus
  = GetCoipPoolUsageResponse'{coipAddressUsages = Core.Nothing,
                              coipPoolId = Core.Nothing, localGatewayRouteTableId = Core.Nothing,
                              responseStatus}

-- | Information about the address usage.
--
-- /Note:/ Consider using 'coipAddressUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpurrsCoipAddressUsages :: Lens.Lens' GetCoipPoolUsageResponse (Core.Maybe [Types.CoipAddressUsage])
gcpurrsCoipAddressUsages = Lens.field @"coipAddressUsages"
{-# INLINEABLE gcpurrsCoipAddressUsages #-}
{-# DEPRECATED coipAddressUsages "Use generic-lens or generic-optics with 'coipAddressUsages' instead"  #-}

-- | The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'coipPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpurrsCoipPoolId :: Lens.Lens' GetCoipPoolUsageResponse (Core.Maybe Core.Text)
gcpurrsCoipPoolId = Lens.field @"coipPoolId"
{-# INLINEABLE gcpurrsCoipPoolId #-}
{-# DEPRECATED coipPoolId "Use generic-lens or generic-optics with 'coipPoolId' instead"  #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpurrsLocalGatewayRouteTableId :: Lens.Lens' GetCoipPoolUsageResponse (Core.Maybe Core.Text)
gcpurrsLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# INLINEABLE gcpurrsLocalGatewayRouteTableId #-}
{-# DEPRECATED localGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpurrsResponseStatus :: Lens.Lens' GetCoipPoolUsageResponse Core.Int
gcpurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcpurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
