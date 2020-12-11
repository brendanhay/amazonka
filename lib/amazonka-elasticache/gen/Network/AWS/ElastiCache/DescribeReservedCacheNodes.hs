{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    drcnProductDescription,
    drcnMarker,
    drcnMaxRecords,
    drcnReservedCacheNodeId,
    drcnOfferingType,
    drcnDuration,
    drcnReservedCacheNodesOfferingId,

    -- * Destructuring the response
    DescribeReservedCacheNodesResponse (..),
    mkDescribeReservedCacheNodesResponse,

    -- ** Response lenses
    drcnrsMarker,
    drcnrsReservedCacheNodes,
    drcnrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeReservedCacheNodes@ operation.
--
-- /See:/ 'mkDescribeReservedCacheNodes' smart constructor.
data DescribeReservedCacheNodes = DescribeReservedCacheNodes'
  { cacheNodeType ::
      Lude.Maybe Lude.Text,
    productDescription ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    reservedCacheNodeId ::
      Lude.Maybe Lude.Text,
    offeringType :: Lude.Maybe Lude.Text,
    duration :: Lude.Maybe Lude.Text,
    reservedCacheNodesOfferingId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedCacheNodes' with the minimum fields required to make a request.
--
-- * 'cacheNodeType' - The cache node type filter value. Use this parameter to show only those reservations matching the specified cache node type.
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
-- * 'duration' - The duration filter value, specified in years or seconds. Use this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
-- * 'offeringType' - The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type.
--
-- Valid values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization"|"All Upfront"|"Partial Upfront"| "No Upfront"@
-- * 'productDescription' - The product description filter value. Use this parameter to show only those reservations matching the specified product description.
-- * 'reservedCacheNodeId' - The reserved cache node identifier filter value. Use this parameter to show only the reservation that matches the specified reservation ID.
-- * 'reservedCacheNodesOfferingId' - The offering identifier filter value. Use this parameter to show only purchased reservations matching the specified offering identifier.
mkDescribeReservedCacheNodes ::
  DescribeReservedCacheNodes
mkDescribeReservedCacheNodes =
  DescribeReservedCacheNodes'
    { cacheNodeType = Lude.Nothing,
      productDescription = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      reservedCacheNodeId = Lude.Nothing,
      offeringType = Lude.Nothing,
      duration = Lude.Nothing,
      reservedCacheNodesOfferingId = Lude.Nothing
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
drcnCacheNodeType :: Lens.Lens' DescribeReservedCacheNodes (Lude.Maybe Lude.Text)
drcnCacheNodeType = Lens.lens (cacheNodeType :: DescribeReservedCacheNodes -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: DescribeReservedCacheNodes)
{-# DEPRECATED drcnCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The product description filter value. Use this parameter to show only those reservations matching the specified product description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnProductDescription :: Lens.Lens' DescribeReservedCacheNodes (Lude.Maybe Lude.Text)
drcnProductDescription = Lens.lens (productDescription :: DescribeReservedCacheNodes -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: DescribeReservedCacheNodes)
{-# DEPRECATED drcnProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnMarker :: Lens.Lens' DescribeReservedCacheNodes (Lude.Maybe Lude.Text)
drcnMarker = Lens.lens (marker :: DescribeReservedCacheNodes -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedCacheNodes)
{-# DEPRECATED drcnMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnMaxRecords :: Lens.Lens' DescribeReservedCacheNodes (Lude.Maybe Lude.Int)
drcnMaxRecords = Lens.lens (maxRecords :: DescribeReservedCacheNodes -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReservedCacheNodes)
{-# DEPRECATED drcnMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The reserved cache node identifier filter value. Use this parameter to show only the reservation that matches the specified reservation ID.
--
-- /Note:/ Consider using 'reservedCacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnReservedCacheNodeId :: Lens.Lens' DescribeReservedCacheNodes (Lude.Maybe Lude.Text)
drcnReservedCacheNodeId = Lens.lens (reservedCacheNodeId :: DescribeReservedCacheNodes -> Lude.Maybe Lude.Text) (\s a -> s {reservedCacheNodeId = a} :: DescribeReservedCacheNodes)
{-# DEPRECATED drcnReservedCacheNodeId "Use generic-lens or generic-optics with 'reservedCacheNodeId' instead." #-}

-- | The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type.
--
-- Valid values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization"|"All Upfront"|"Partial Upfront"| "No Upfront"@
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnOfferingType :: Lens.Lens' DescribeReservedCacheNodes (Lude.Maybe Lude.Text)
drcnOfferingType = Lens.lens (offeringType :: DescribeReservedCacheNodes -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: DescribeReservedCacheNodes)
{-# DEPRECATED drcnOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The duration filter value, specified in years or seconds. Use this parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnDuration :: Lens.Lens' DescribeReservedCacheNodes (Lude.Maybe Lude.Text)
drcnDuration = Lens.lens (duration :: DescribeReservedCacheNodes -> Lude.Maybe Lude.Text) (\s a -> s {duration = a} :: DescribeReservedCacheNodes)
{-# DEPRECATED drcnDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The offering identifier filter value. Use this parameter to show only purchased reservations matching the specified offering identifier.
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnReservedCacheNodesOfferingId :: Lens.Lens' DescribeReservedCacheNodes (Lude.Maybe Lude.Text)
drcnReservedCacheNodesOfferingId = Lens.lens (reservedCacheNodesOfferingId :: DescribeReservedCacheNodes -> Lude.Maybe Lude.Text) (\s a -> s {reservedCacheNodesOfferingId = a} :: DescribeReservedCacheNodes)
{-# DEPRECATED drcnReservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead." #-}

instance Page.AWSPager DescribeReservedCacheNodes where
  page rq rs
    | Page.stop (rs Lens.^. drcnrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drcnrsReservedCacheNodes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drcnMarker Lens..~ rs Lens.^. drcnrsMarker

instance Lude.AWSRequest DescribeReservedCacheNodes where
  type
    Rs DescribeReservedCacheNodes =
      DescribeReservedCacheNodesResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeReservedCacheNodesResult"
      ( \s h x ->
          DescribeReservedCacheNodesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "ReservedCacheNodes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ReservedCacheNode")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedCacheNodes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedCacheNodes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedCacheNodes where
  toQuery DescribeReservedCacheNodes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeReservedCacheNodes" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheNodeType" Lude.=: cacheNodeType,
        "ProductDescription" Lude.=: productDescription,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ReservedCacheNodeId" Lude.=: reservedCacheNodeId,
        "OfferingType" Lude.=: offeringType,
        "Duration" Lude.=: duration,
        "ReservedCacheNodesOfferingId"
          Lude.=: reservedCacheNodesOfferingId
      ]

-- | Represents the output of a @DescribeReservedCacheNodes@ operation.
--
-- /See:/ 'mkDescribeReservedCacheNodesResponse' smart constructor.
data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    reservedCacheNodes ::
      Lude.Maybe
        [ReservedCacheNode],
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

-- | Creates a value of 'DescribeReservedCacheNodesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'reservedCacheNodes' - A list of reserved cache nodes. Each element in the list contains detailed information about one node.
-- * 'responseStatus' - The response status code.
mkDescribeReservedCacheNodesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedCacheNodesResponse
mkDescribeReservedCacheNodesResponse pResponseStatus_ =
  DescribeReservedCacheNodesResponse'
    { marker = Lude.Nothing,
      reservedCacheNodes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnrsMarker :: Lens.Lens' DescribeReservedCacheNodesResponse (Lude.Maybe Lude.Text)
drcnrsMarker = Lens.lens (marker :: DescribeReservedCacheNodesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedCacheNodesResponse)
{-# DEPRECATED drcnrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of reserved cache nodes. Each element in the list contains detailed information about one node.
--
-- /Note:/ Consider using 'reservedCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnrsReservedCacheNodes :: Lens.Lens' DescribeReservedCacheNodesResponse (Lude.Maybe [ReservedCacheNode])
drcnrsReservedCacheNodes = Lens.lens (reservedCacheNodes :: DescribeReservedCacheNodesResponse -> Lude.Maybe [ReservedCacheNode]) (\s a -> s {reservedCacheNodes = a} :: DescribeReservedCacheNodesResponse)
{-# DEPRECATED drcnrsReservedCacheNodes "Use generic-lens or generic-optics with 'reservedCacheNodes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcnrsResponseStatus :: Lens.Lens' DescribeReservedCacheNodesResponse Lude.Int
drcnrsResponseStatus = Lens.lens (responseStatus :: DescribeReservedCacheNodesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedCacheNodesResponse)
{-# DEPRECATED drcnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
