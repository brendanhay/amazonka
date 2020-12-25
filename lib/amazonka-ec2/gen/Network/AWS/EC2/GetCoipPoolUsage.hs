{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetCoipPoolUsage (..),
    mkGetCoipPoolUsage,

    -- ** Request lenses
    gcpuPoolId,
    gcpuDryRun,
    gcpuFilters,
    gcpuMaxResults,
    gcpuNextToken,

    -- * Destructuring the response
    GetCoipPoolUsageResponse (..),
    mkGetCoipPoolUsageResponse,

    -- ** Response lenses
    gcpurrsCoipAddressUsages,
    gcpurrsCoipPoolId,
    gcpurrsLocalGatewayRouteTableId,
    gcpurrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCoipPoolUsage' smart constructor.
data GetCoipPoolUsage = GetCoipPoolUsage'
  { -- | The ID of the address pool.
    poolId :: Types.PoolId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCoipPoolUsage' value with any optional fields omitted.
mkGetCoipPoolUsage ::
  -- | 'poolId'
  Types.PoolId ->
  GetCoipPoolUsage
mkGetCoipPoolUsage poolId =
  GetCoipPoolUsage'
    { poolId,
      dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuPoolId :: Lens.Lens' GetCoipPoolUsage Types.PoolId
gcpuPoolId = Lens.field @"poolId"
{-# DEPRECATED gcpuPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuDryRun :: Lens.Lens' GetCoipPoolUsage (Core.Maybe Core.Bool)
gcpuDryRun = Lens.field @"dryRun"
{-# DEPRECATED gcpuDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
{-# DEPRECATED gcpuFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuMaxResults :: Lens.Lens' GetCoipPoolUsage (Core.Maybe Core.Natural)
gcpuMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gcpuMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuNextToken :: Lens.Lens' GetCoipPoolUsage (Core.Maybe Types.String)
gcpuNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcpuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetCoipPoolUsage where
  type Rs GetCoipPoolUsage = GetCoipPoolUsageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetCoipPoolUsage")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "PoolId" poolId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetCoipPoolUsageResponse'
            Core.<$> ( x Core..@? "coipAddressUsageSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "coipPoolId")
            Core.<*> (x Core..@? "localGatewayRouteTableId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCoipPoolUsageResponse' smart constructor.
data GetCoipPoolUsageResponse = GetCoipPoolUsageResponse'
  { -- | Information about the address usage.
    coipAddressUsages :: Core.Maybe [Types.CoipAddressUsage],
    -- | The ID of the customer-owned address pool.
    coipPoolId :: Core.Maybe Types.String,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCoipPoolUsageResponse' value with any optional fields omitted.
mkGetCoipPoolUsageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCoipPoolUsageResponse
mkGetCoipPoolUsageResponse responseStatus =
  GetCoipPoolUsageResponse'
    { coipAddressUsages = Core.Nothing,
      coipPoolId = Core.Nothing,
      localGatewayRouteTableId = Core.Nothing,
      responseStatus
    }

-- | Information about the address usage.
--
-- /Note:/ Consider using 'coipAddressUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpurrsCoipAddressUsages :: Lens.Lens' GetCoipPoolUsageResponse (Core.Maybe [Types.CoipAddressUsage])
gcpurrsCoipAddressUsages = Lens.field @"coipAddressUsages"
{-# DEPRECATED gcpurrsCoipAddressUsages "Use generic-lens or generic-optics with 'coipAddressUsages' instead." #-}

-- | The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'coipPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpurrsCoipPoolId :: Lens.Lens' GetCoipPoolUsageResponse (Core.Maybe Types.String)
gcpurrsCoipPoolId = Lens.field @"coipPoolId"
{-# DEPRECATED gcpurrsCoipPoolId "Use generic-lens or generic-optics with 'coipPoolId' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpurrsLocalGatewayRouteTableId :: Lens.Lens' GetCoipPoolUsageResponse (Core.Maybe Types.String)
gcpurrsLocalGatewayRouteTableId = Lens.field @"localGatewayRouteTableId"
{-# DEPRECATED gcpurrsLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpurrsResponseStatus :: Lens.Lens' GetCoipPoolUsageResponse Core.Int
gcpurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcpurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
