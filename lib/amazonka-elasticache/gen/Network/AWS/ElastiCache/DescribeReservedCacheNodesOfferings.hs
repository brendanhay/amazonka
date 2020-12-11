{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeReservedCacheNodesOfferings (..),
    mkDescribeReservedCacheNodesOfferings,

    -- ** Request lenses
    drcnoCacheNodeType,
    drcnoProductDescription,
    drcnoMarker,
    drcnoMaxRecords,
    drcnoOfferingType,
    drcnoDuration,
    drcnoReservedCacheNodesOfferingId,

    -- * Destructuring the response
    DescribeReservedCacheNodesOfferingsResponse (..),
    mkDescribeReservedCacheNodesOfferingsResponse,

    -- ** Response lenses
    drcnorsMarker,
    drcnorsReservedCacheNodesOfferings,
    drcnorsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeReservedCacheNodesOfferings@ operation.
--
-- /See:/ 'mkDescribeReservedCacheNodesOfferings' smart constructor.
data DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings'
  { cacheNodeType ::
      Lude.Maybe
        Lude.Text,
    productDescription ::
      Lude.Maybe
        Lude.Text,
    marker ::
      Lude.Maybe
        Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    offeringType ::
      Lude.Maybe
        Lude.Text,
    duration ::
      Lude.Maybe
        Lude.Text,
    reservedCacheNodesOfferingId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedCacheNodesOfferings' with the minimum fields required to make a request.
--
-- * 'cacheNodeType' - The cache node type filter value. Use this parameter to show only the available offerings matching the specified cache node type.
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
-- * 'duration' - Duration filter value, specified in years or seconds. Use this parameter to show only reservations for a given duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
-- * 'offeringType' - The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization" |"All Upfront"|"Partial Upfront"| "No Upfront"@
-- * 'productDescription' - The product description filter value. Use this parameter to show only the available offerings matching the specified product description.
-- * 'reservedCacheNodesOfferingId' - The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
mkDescribeReservedCacheNodesOfferings ::
  DescribeReservedCacheNodesOfferings
mkDescribeReservedCacheNodesOfferings =
  DescribeReservedCacheNodesOfferings'
    { cacheNodeType =
        Lude.Nothing,
      productDescription = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      offeringType = Lude.Nothing,
      duration = Lude.Nothing,
      reservedCacheNodesOfferingId = Lude.Nothing
    }

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
drcnoCacheNodeType :: Lens.Lens' DescribeReservedCacheNodesOfferings (Lude.Maybe Lude.Text)
drcnoCacheNodeType = Lens.lens (cacheNodeType :: DescribeReservedCacheNodesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: DescribeReservedCacheNodesOfferings)
{-# DEPRECATED drcnoCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The product description filter value. Use this parameter to show only the available offerings matching the specified product description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoProductDescription :: Lens.Lens' DescribeReservedCacheNodesOfferings (Lude.Maybe Lude.Text)
drcnoProductDescription = Lens.lens (productDescription :: DescribeReservedCacheNodesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: DescribeReservedCacheNodesOfferings)
{-# DEPRECATED drcnoProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoMarker :: Lens.Lens' DescribeReservedCacheNodesOfferings (Lude.Maybe Lude.Text)
drcnoMarker = Lens.lens (marker :: DescribeReservedCacheNodesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedCacheNodesOfferings)
{-# DEPRECATED drcnoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoMaxRecords :: Lens.Lens' DescribeReservedCacheNodesOfferings (Lude.Maybe Lude.Int)
drcnoMaxRecords = Lens.lens (maxRecords :: DescribeReservedCacheNodesOfferings -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReservedCacheNodesOfferings)
{-# DEPRECATED drcnoMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type.
--
-- Valid Values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization" |"All Upfront"|"Partial Upfront"| "No Upfront"@
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoOfferingType :: Lens.Lens' DescribeReservedCacheNodesOfferings (Lude.Maybe Lude.Text)
drcnoOfferingType = Lens.lens (offeringType :: DescribeReservedCacheNodesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: DescribeReservedCacheNodesOfferings)
{-# DEPRECATED drcnoOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Duration filter value, specified in years or seconds. Use this parameter to show only reservations for a given duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoDuration :: Lens.Lens' DescribeReservedCacheNodesOfferings (Lude.Maybe Lude.Text)
drcnoDuration = Lens.lens (duration :: DescribeReservedCacheNodesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {duration = a} :: DescribeReservedCacheNodesOfferings)
{-# DEPRECATED drcnoDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnoReservedCacheNodesOfferingId :: Lens.Lens' DescribeReservedCacheNodesOfferings (Lude.Maybe Lude.Text)
drcnoReservedCacheNodesOfferingId = Lens.lens (reservedCacheNodesOfferingId :: DescribeReservedCacheNodesOfferings -> Lude.Maybe Lude.Text) (\s a -> s {reservedCacheNodesOfferingId = a} :: DescribeReservedCacheNodesOfferings)
{-# DEPRECATED drcnoReservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead." #-}

instance Page.AWSPager DescribeReservedCacheNodesOfferings where
  page rq rs
    | Page.stop (rs Lens.^. drcnorsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drcnorsReservedCacheNodesOfferings) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drcnoMarker Lens..~ rs Lens.^. drcnorsMarker

instance Lude.AWSRequest DescribeReservedCacheNodesOfferings where
  type
    Rs DescribeReservedCacheNodesOfferings =
      DescribeReservedCacheNodesOfferingsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeReservedCacheNodesOfferingsResult"
      ( \s h x ->
          DescribeReservedCacheNodesOfferingsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "ReservedCacheNodesOfferings" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ReservedCacheNodesOffering")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedCacheNodesOfferings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedCacheNodesOfferings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedCacheNodesOfferings where
  toQuery DescribeReservedCacheNodesOfferings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeReservedCacheNodesOfferings" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheNodeType" Lude.=: cacheNodeType,
        "ProductDescription" Lude.=: productDescription,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "OfferingType" Lude.=: offeringType,
        "Duration" Lude.=: duration,
        "ReservedCacheNodesOfferingId"
          Lude.=: reservedCacheNodesOfferingId
      ]

-- | Represents the output of a @DescribeReservedCacheNodesOfferings@ operation.
--
-- /See:/ 'mkDescribeReservedCacheNodesOfferingsResponse' smart constructor.
data DescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse'
  { marker ::
      Lude.Maybe
        Lude.Text,
    reservedCacheNodesOfferings ::
      Lude.Maybe
        [ReservedCacheNodesOffering],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedCacheNodesOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'reservedCacheNodesOfferings' - A list of reserved cache node offerings. Each element in the list contains detailed information about one offering.
-- * 'responseStatus' - The response status code.
mkDescribeReservedCacheNodesOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedCacheNodesOfferingsResponse
mkDescribeReservedCacheNodesOfferingsResponse pResponseStatus_ =
  DescribeReservedCacheNodesOfferingsResponse'
    { marker =
        Lude.Nothing,
      reservedCacheNodesOfferings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnorsMarker :: Lens.Lens' DescribeReservedCacheNodesOfferingsResponse (Lude.Maybe Lude.Text)
drcnorsMarker = Lens.lens (marker :: DescribeReservedCacheNodesOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedCacheNodesOfferingsResponse)
{-# DEPRECATED drcnorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of reserved cache node offerings. Each element in the list contains detailed information about one offering.
--
-- /Note:/ Consider using 'reservedCacheNodesOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnorsReservedCacheNodesOfferings :: Lens.Lens' DescribeReservedCacheNodesOfferingsResponse (Lude.Maybe [ReservedCacheNodesOffering])
drcnorsReservedCacheNodesOfferings = Lens.lens (reservedCacheNodesOfferings :: DescribeReservedCacheNodesOfferingsResponse -> Lude.Maybe [ReservedCacheNodesOffering]) (\s a -> s {reservedCacheNodesOfferings = a} :: DescribeReservedCacheNodesOfferingsResponse)
{-# DEPRECATED drcnorsReservedCacheNodesOfferings "Use generic-lens or generic-optics with 'reservedCacheNodesOfferings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnorsResponseStatus :: Lens.Lens' DescribeReservedCacheNodesOfferingsResponse Lude.Int
drcnorsResponseStatus = Lens.lens (responseStatus :: DescribeReservedCacheNodesOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedCacheNodesOfferingsResponse)
{-# DEPRECATED drcnorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
