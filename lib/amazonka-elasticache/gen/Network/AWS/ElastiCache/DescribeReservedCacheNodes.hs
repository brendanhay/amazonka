{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved cache nodes for this account, or about a specified reserved cache node.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeReservedCacheNodes
  ( -- * Creating a request
    DescribeReservedCacheNodes (..),
    mkDescribeReservedCacheNodes,

    -- ** Request lenses
    drcnCacheNodeType,
    drcnDuration,
    drcnMarker,
    drcnMaxRecords,
    drcnOfferingType,
    drcnProductDescription,
    drcnReservedCacheNodeId,
    drcnReservedCacheNodesOfferingId,

    -- * Destructuring the response
    DescribeReservedCacheNodesResponse (..),
    mkDescribeReservedCacheNodesResponse,

    -- ** Response lenses
    drcnrrsMarker,
    drcnrrsReservedCacheNodes,
    drcnrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeReservedCacheNodes@ operation.
--
-- /See:/ 'mkDescribeReservedCacheNodes' smart constructor.
data DescribeReservedCacheNodes = DescribeReservedCacheNodes'
  { -- | The cache node type filter value. Use this parameter to show only those reservations matching the specified cache node type.
    --
    -- The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.
    --
    --     * General purpose:
    --
    --     * Current generation:
    -- __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
    -- @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@
    -- __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@
    -- __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@
    -- __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@
    -- __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@
    --
    --
    --     * Previous generation: (not recommended)
    -- __T1 node types:__ @cache.t1.micro@
    -- __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@
    -- __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@
    --
    --
    --
    --
    --     * Compute optimized:
    --
    --     * Previous generation: (not recommended)
    -- __C1 node types:__ @cache.c1.xlarge@
    --
    --
    --
    --
    --     * Memory optimized:
    --
    --     * Current generation:
    -- __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
    -- @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@
    -- __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@
    -- __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@
    --
    --
    --     * Previous generation: (not recommended)
    -- __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@
    -- __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@
    --
    --
    --
    --
    -- __Additional node type info__
    --
    --     * All current generation instance types are created in Amazon VPC by default.
    --
    --
    --     * Redis append-only files (AOF) are not supported for T1 or T2 instances.
    --
    --
    --     * Redis Multi-AZ with automatic failover is not supported on T1 instances.
    --
    --
    --     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
    cacheNodeType :: Core.Maybe Types.String,
    -- | The duration filter value, specified in years or seconds. Use this parameter to show only reservations for this duration.
    --
    -- Valid Values: @1 | 3 | 31536000 | 94608000@
    duration :: Core.Maybe Types.String,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type.
    --
    -- Valid values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization"|"All Upfront"|"Partial Upfront"| "No Upfront"@
    offeringType :: Core.Maybe Types.String,
    -- | The product description filter value. Use this parameter to show only those reservations matching the specified product description.
    productDescription :: Core.Maybe Types.String,
    -- | The reserved cache node identifier filter value. Use this parameter to show only the reservation that matches the specified reservation ID.
    reservedCacheNodeId :: Core.Maybe Types.String,
    -- | The offering identifier filter value. Use this parameter to show only purchased reservations matching the specified offering identifier.
    reservedCacheNodesOfferingId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedCacheNodes' value with any optional fields omitted.
mkDescribeReservedCacheNodes ::
  DescribeReservedCacheNodes
mkDescribeReservedCacheNodes =
  DescribeReservedCacheNodes'
    { cacheNodeType = Core.Nothing,
      duration = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      offeringType = Core.Nothing,
      productDescription = Core.Nothing,
      reservedCacheNodeId = Core.Nothing,
      reservedCacheNodesOfferingId = Core.Nothing
    }

-- | The cache node type filter value. Use this parameter to show only those reservations matching the specified cache node type.
--
-- The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.
--
--     * General purpose:
--
--     * Current generation:
-- __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@
-- __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@
-- __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@
-- __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@
-- __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@
--
--
--     * Previous generation: (not recommended)
-- __T1 node types:__ @cache.t1.micro@
-- __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@
-- __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@
--
--
--
--
--     * Compute optimized:
--
--     * Previous generation: (not recommended)
-- __C1 node types:__ @cache.c1.xlarge@
--
--
--
--
--     * Memory optimized:
--
--     * Current generation:
-- __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@
-- __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@
-- __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@
--
--
--     * Previous generation: (not recommended)
-- __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@
-- __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@
--
--
--
--
-- __Additional node type info__
--
--     * All current generation instance types are created in Amazon VPC by default.
--
--
--     * Redis append-only files (AOF) are not supported for T1 or T2 instances.
--
--
--     * Redis Multi-AZ with automatic failover is not supported on T1 instances.
--
--
--     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
--
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnCacheNodeType :: Lens.Lens' DescribeReservedCacheNodes (Core.Maybe Types.String)
drcnCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED drcnCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The duration filter value, specified in years or seconds. Use this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnDuration :: Lens.Lens' DescribeReservedCacheNodes (Core.Maybe Types.String)
drcnDuration = Lens.field @"duration"
{-# DEPRECATED drcnDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnMarker :: Lens.Lens' DescribeReservedCacheNodes (Core.Maybe Types.String)
drcnMarker = Lens.field @"marker"
{-# DEPRECATED drcnMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnMaxRecords :: Lens.Lens' DescribeReservedCacheNodes (Core.Maybe Core.Int)
drcnMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED drcnMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type.
--
-- Valid values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization"|"All Upfront"|"Partial Upfront"| "No Upfront"@
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnOfferingType :: Lens.Lens' DescribeReservedCacheNodes (Core.Maybe Types.String)
drcnOfferingType = Lens.field @"offeringType"
{-# DEPRECATED drcnOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The product description filter value. Use this parameter to show only those reservations matching the specified product description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnProductDescription :: Lens.Lens' DescribeReservedCacheNodes (Core.Maybe Types.String)
drcnProductDescription = Lens.field @"productDescription"
{-# DEPRECATED drcnProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The reserved cache node identifier filter value. Use this parameter to show only the reservation that matches the specified reservation ID.
--
-- /Note:/ Consider using 'reservedCacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnReservedCacheNodeId :: Lens.Lens' DescribeReservedCacheNodes (Core.Maybe Types.String)
drcnReservedCacheNodeId = Lens.field @"reservedCacheNodeId"
{-# DEPRECATED drcnReservedCacheNodeId "Use generic-lens or generic-optics with 'reservedCacheNodeId' instead." #-}

-- | The offering identifier filter value. Use this parameter to show only purchased reservations matching the specified offering identifier.
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnReservedCacheNodesOfferingId :: Lens.Lens' DescribeReservedCacheNodes (Core.Maybe Types.String)
drcnReservedCacheNodesOfferingId = Lens.field @"reservedCacheNodesOfferingId"
{-# DEPRECATED drcnReservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead." #-}

instance Core.AWSRequest DescribeReservedCacheNodes where
  type
    Rs DescribeReservedCacheNodes =
      DescribeReservedCacheNodesResponse
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
            ( Core.pure ("Action", "DescribeReservedCacheNodes")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "CacheNodeType" Core.<$> cacheNodeType)
                Core.<> (Core.toQueryValue "Duration" Core.<$> duration)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "OfferingType" Core.<$> offeringType)
                Core.<> ( Core.toQueryValue "ProductDescription"
                            Core.<$> productDescription
                        )
                Core.<> ( Core.toQueryValue "ReservedCacheNodeId"
                            Core.<$> reservedCacheNodeId
                        )
                Core.<> ( Core.toQueryValue "ReservedCacheNodesOfferingId"
                            Core.<$> reservedCacheNodesOfferingId
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeReservedCacheNodesResult"
      ( \s h x ->
          DescribeReservedCacheNodesResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "ReservedCacheNodes"
                         Core..<@> Core.parseXMLList "ReservedCacheNode"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeReservedCacheNodes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"reservedCacheNodes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Represents the output of a @DescribeReservedCacheNodes@ operation.
--
-- /See:/ 'mkDescribeReservedCacheNodesResponse' smart constructor.
data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Types.String,
    -- | A list of reserved cache nodes. Each element in the list contains detailed information about one node.
    reservedCacheNodes :: Core.Maybe [Types.ReservedCacheNode],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeReservedCacheNodesResponse' value with any optional fields omitted.
mkDescribeReservedCacheNodesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReservedCacheNodesResponse
mkDescribeReservedCacheNodesResponse responseStatus =
  DescribeReservedCacheNodesResponse'
    { marker = Core.Nothing,
      reservedCacheNodes = Core.Nothing,
      responseStatus
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnrrsMarker :: Lens.Lens' DescribeReservedCacheNodesResponse (Core.Maybe Types.String)
drcnrrsMarker = Lens.field @"marker"
{-# DEPRECATED drcnrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of reserved cache nodes. Each element in the list contains detailed information about one node.
--
-- /Note:/ Consider using 'reservedCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnrrsReservedCacheNodes :: Lens.Lens' DescribeReservedCacheNodesResponse (Core.Maybe [Types.ReservedCacheNode])
drcnrrsReservedCacheNodes = Lens.field @"reservedCacheNodes"
{-# DEPRECATED drcnrrsReservedCacheNodes "Use generic-lens or generic-optics with 'reservedCacheNodes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnrrsResponseStatus :: Lens.Lens' DescribeReservedCacheNodesResponse Core.Int
drcnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drcnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
