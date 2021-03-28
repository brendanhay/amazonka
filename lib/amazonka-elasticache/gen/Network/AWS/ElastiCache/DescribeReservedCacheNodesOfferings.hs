{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved cache node offerings.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
    (
    -- * Creating a request
      DescribeReservedCacheNodesOfferings (..)
    , mkDescribeReservedCacheNodesOfferings
    -- ** Request lenses
    , drcnoCacheNodeType
    , drcnoDuration
    , drcnoMarker
    , drcnoMaxRecords
    , drcnoOfferingType
    , drcnoProductDescription
    , drcnoReservedCacheNodesOfferingId

    -- * Destructuring the response
    , DescribeReservedCacheNodesOfferingsResponse (..)
    , mkDescribeReservedCacheNodesOfferingsResponse
    -- ** Response lenses
    , drcnorrsMarker
    , drcnorrsReservedCacheNodesOfferings
    , drcnorrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeReservedCacheNodesOfferings@ operation.
--
-- /See:/ 'mkDescribeReservedCacheNodesOfferings' smart constructor.
data DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings'
  { cacheNodeType :: Core.Maybe Core.Text
    -- ^ The cache node type filter value. Use this parameter to show only the available offerings matching the specified cache node type.
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
  , duration :: Core.Maybe Core.Text
    -- ^ Duration filter value, specified in years or seconds. Use this parameter to show only reservations for a given duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@ 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
  , offeringType :: Core.Maybe Core.Text
    -- ^ The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization" |"All Upfront"|"Partial Upfront"| "No Upfront"@ 
  , productDescription :: Core.Maybe Core.Text
    -- ^ The product description filter value. Use this parameter to show only the available offerings matching the specified product description.
  , reservedCacheNodesOfferingId :: Core.Maybe Core.Text
    -- ^ The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedCacheNodesOfferings' value with any optional fields omitted.
mkDescribeReservedCacheNodesOfferings
    :: DescribeReservedCacheNodesOfferings
mkDescribeReservedCacheNodesOfferings
  = DescribeReservedCacheNodesOfferings'{cacheNodeType =
                                           Core.Nothing,
                                         duration = Core.Nothing, marker = Core.Nothing,
                                         maxRecords = Core.Nothing, offeringType = Core.Nothing,
                                         productDescription = Core.Nothing,
                                         reservedCacheNodesOfferingId = Core.Nothing}

-- | The cache node type filter value. Use this parameter to show only the available offerings matching the specified cache node type.
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
drcnoCacheNodeType :: Lens.Lens' DescribeReservedCacheNodesOfferings (Core.Maybe Core.Text)
drcnoCacheNodeType = Lens.field @"cacheNodeType"
{-# INLINEABLE drcnoCacheNodeType #-}
{-# DEPRECATED cacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead"  #-}

-- | Duration filter value, specified in years or seconds. Use this parameter to show only reservations for a given duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@ 
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoDuration :: Lens.Lens' DescribeReservedCacheNodesOfferings (Core.Maybe Core.Text)
drcnoDuration = Lens.field @"duration"
{-# INLINEABLE drcnoDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoMarker :: Lens.Lens' DescribeReservedCacheNodesOfferings (Core.Maybe Core.Text)
drcnoMarker = Lens.field @"marker"
{-# INLINEABLE drcnoMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoMaxRecords :: Lens.Lens' DescribeReservedCacheNodesOfferings (Core.Maybe Core.Int)
drcnoMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE drcnoMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization" |"All Upfront"|"Partial Upfront"| "No Upfront"@ 
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoOfferingType :: Lens.Lens' DescribeReservedCacheNodesOfferings (Core.Maybe Core.Text)
drcnoOfferingType = Lens.field @"offeringType"
{-# INLINEABLE drcnoOfferingType #-}
{-# DEPRECATED offeringType "Use generic-lens or generic-optics with 'offeringType' instead"  #-}

-- | The product description filter value. Use this parameter to show only the available offerings matching the specified product description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoProductDescription :: Lens.Lens' DescribeReservedCacheNodesOfferings (Core.Maybe Core.Text)
drcnoProductDescription = Lens.field @"productDescription"
{-# INLINEABLE drcnoProductDescription #-}
{-# DEPRECATED productDescription "Use generic-lens or generic-optics with 'productDescription' instead"  #-}

-- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@ 
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoReservedCacheNodesOfferingId :: Lens.Lens' DescribeReservedCacheNodesOfferings (Core.Maybe Core.Text)
drcnoReservedCacheNodesOfferingId = Lens.field @"reservedCacheNodesOfferingId"
{-# INLINEABLE drcnoReservedCacheNodesOfferingId #-}
{-# DEPRECATED reservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead"  #-}

instance Core.ToQuery DescribeReservedCacheNodesOfferings where
        toQuery DescribeReservedCacheNodesOfferings{..}
          = Core.toQueryPair "Action"
              ("DescribeReservedCacheNodesOfferings" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheNodeType")
                cacheNodeType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Duration") duration
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OfferingType")
                offeringType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ProductDescription")
                productDescription
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ReservedCacheNodesOfferingId")
                reservedCacheNodesOfferingId

instance Core.ToHeaders DescribeReservedCacheNodesOfferings where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeReservedCacheNodesOfferings where
        type Rs DescribeReservedCacheNodesOfferings =
             DescribeReservedCacheNodesOfferingsResponse
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
          = Response.receiveXMLWrapper
              "DescribeReservedCacheNodesOfferingsResult"
              (\ s h x ->
                 DescribeReservedCacheNodesOfferingsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "ReservedCacheNodesOfferings" Core..<@>
                       Core.parseXMLList "ReservedCacheNodesOffering"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeReservedCacheNodesOfferings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"reservedCacheNodesOfferings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Represents the output of a @DescribeReservedCacheNodesOfferings@ operation.
--
-- /See:/ 'mkDescribeReservedCacheNodesOfferingsResponse' smart constructor.
data DescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , reservedCacheNodesOfferings :: Core.Maybe [Types.ReservedCacheNodesOffering]
    -- ^ A list of reserved cache node offerings. Each element in the list contains detailed information about one offering.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservedCacheNodesOfferingsResponse' value with any optional fields omitted.
mkDescribeReservedCacheNodesOfferingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReservedCacheNodesOfferingsResponse
mkDescribeReservedCacheNodesOfferingsResponse responseStatus
  = DescribeReservedCacheNodesOfferingsResponse'{marker =
                                                   Core.Nothing,
                                                 reservedCacheNodesOfferings = Core.Nothing,
                                                 responseStatus}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnorrsMarker :: Lens.Lens' DescribeReservedCacheNodesOfferingsResponse (Core.Maybe Core.Text)
drcnorrsMarker = Lens.field @"marker"
{-# INLINEABLE drcnorrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of reserved cache node offerings. Each element in the list contains detailed information about one offering.
--
-- /Note:/ Consider using 'reservedCacheNodesOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnorrsReservedCacheNodesOfferings :: Lens.Lens' DescribeReservedCacheNodesOfferingsResponse (Core.Maybe [Types.ReservedCacheNodesOffering])
drcnorrsReservedCacheNodesOfferings = Lens.field @"reservedCacheNodesOfferings"
{-# INLINEABLE drcnorrsReservedCacheNodesOfferings #-}
{-# DEPRECATED reservedCacheNodesOfferings "Use generic-lens or generic-optics with 'reservedCacheNodesOfferings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnorrsResponseStatus :: Lens.Lens' DescribeReservedCacheNodesOfferingsResponse Core.Int
drcnorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drcnorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
