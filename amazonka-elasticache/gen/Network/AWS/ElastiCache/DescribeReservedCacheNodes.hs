{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved cache nodes for this account, or
-- about a specified reserved cache node.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeReservedCacheNodes
  ( -- * Creating a Request
    DescribeReservedCacheNodes (..),
    newDescribeReservedCacheNodes,

    -- * Request Lenses
    describeReservedCacheNodes_reservedCacheNodesOfferingId,
    describeReservedCacheNodes_duration,
    describeReservedCacheNodes_cacheNodeType,
    describeReservedCacheNodes_offeringType,
    describeReservedCacheNodes_productDescription,
    describeReservedCacheNodes_reservedCacheNodeId,
    describeReservedCacheNodes_marker,
    describeReservedCacheNodes_maxRecords,

    -- * Destructuring the Response
    DescribeReservedCacheNodesResponse (..),
    newDescribeReservedCacheNodesResponse,

    -- * Response Lenses
    describeReservedCacheNodesResponse_reservedCacheNodes,
    describeReservedCacheNodesResponse_marker,
    describeReservedCacheNodesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeReservedCacheNodes@ operation.
--
-- /See:/ 'newDescribeReservedCacheNodes' smart constructor.
data DescribeReservedCacheNodes = DescribeReservedCacheNodes'
  { -- | The offering identifier filter value. Use this parameter to show only
    -- purchased reservations matching the specified offering identifier.
    reservedCacheNodesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The duration filter value, specified in years or seconds. Use this
    -- parameter to show only reservations for this duration.
    --
    -- Valid Values: @1 | 3 | 31536000 | 94608000@
    duration :: Prelude.Maybe Prelude.Text,
    -- | The cache node type filter value. Use this parameter to show only those
    -- reservations matching the specified cache node type.
    --
    -- The following node types are supported by ElastiCache. Generally
    -- speaking, the current generation types provide more memory and
    -- computational power at lower cost when compared to their equivalent
    -- previous generation counterparts.
    --
    -- -   General purpose:
    --
    --     -   Current generation:
    --
    --         __M6g node types__ (available only for Redis engine version
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward).
    --
    --         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
    --         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
    --         @cache.m6g.16xlarge@
    --
    --         For region availability, see
    --         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
    --
    --         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
    --         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
    --         @cache.m5.24xlarge@
    --
    --         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
    --         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
    --
    --         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
    --         @cache.t3.medium@
    --
    --         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
    --         @cache.t2.medium@
    --
    --     -   Previous generation: (not recommended)
    --
    --         __T1 node types:__ @cache.t1.micro@
    --
    --         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
    --         @cache.m1.large@, @cache.m1.xlarge@
    --
    --         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
    --         @cache.m3.xlarge@, @cache.m3.2xlarge@
    --
    -- -   Compute optimized:
    --
    --     -   Previous generation: (not recommended)
    --
    --         __C1 node types:__ @cache.c1.xlarge@
    --
    -- -   Memory optimized:
    --
    --     -   Current generation:
    --
    --         __R6g node types__ (available only for Redis engine version
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward).
    --
    --         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
    --         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
    --         @cache.r6g.16xlarge@
    --
    --         For region availability, see
    --         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
    --
    --         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
    --         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
    --         @cache.r5.24xlarge@
    --
    --         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
    --         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
    --         @cache.r4.16xlarge@
    --
    --     -   Previous generation: (not recommended)
    --
    --         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
    --         @cache.m2.4xlarge@
    --
    --         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
    --         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
    --
    -- __Additional node type info__
    --
    -- -   All current generation instance types are created in Amazon VPC by
    --     default.
    --
    -- -   Redis append-only files (AOF) are not supported for T1 or T2
    --     instances.
    --
    -- -   Redis Multi-AZ with automatic failover is not supported on T1
    --     instances.
    --
    -- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
    --     supported on Redis version 2.8.22 and later.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The offering type filter value. Use this parameter to show only the
    -- available offerings matching the specified offering type.
    --
    -- Valid values:
    -- @\"Light Utilization\"|\"Medium Utilization\"|\"Heavy Utilization\"|\"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"@
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The product description filter value. Use this parameter to show only
    -- those reservations matching the specified product description.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The reserved cache node identifier filter value. Use this parameter to
    -- show only the reservation that matches the specified reservation ID.
    reservedCacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedCacheNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedCacheNodesOfferingId', 'describeReservedCacheNodes_reservedCacheNodesOfferingId' - The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
--
-- 'duration', 'describeReservedCacheNodes_duration' - The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- 'cacheNodeType', 'describeReservedCacheNodes_cacheNodeType' - The cache node type filter value. Use this parameter to show only those
-- reservations matching the specified cache node type.
--
-- The following node types are supported by ElastiCache. Generally
-- speaking, the current generation types provide more memory and
-- computational power at lower cost when compared to their equivalent
-- previous generation counterparts.
--
-- -   General purpose:
--
--     -   Current generation:
--
--         __M6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
--         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
--         @cache.m6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
--         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
--         @cache.m5.24xlarge@
--
--         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
--         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended)
--
--         __T1 node types:__ @cache.t1.micro@
--
--         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
--         @cache.m1.large@, @cache.m1.xlarge@
--
--         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--
-- -   Compute optimized:
--
--     -   Previous generation: (not recommended)
--
--         __C1 node types:__ @cache.c1.xlarge@
--
-- -   Memory optimized:
--
--     -   Current generation:
--
--         __R6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
--         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
--         @cache.r6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
--         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
--         @cache.r5.24xlarge@
--
--         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
--         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
--         @cache.r4.16xlarge@
--
--     -   Previous generation: (not recommended)
--
--         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
--         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--
-- __Additional node type info__
--
-- -   All current generation instance types are created in Amazon VPC by
--     default.
--
-- -   Redis append-only files (AOF) are not supported for T1 or T2
--     instances.
--
-- -   Redis Multi-AZ with automatic failover is not supported on T1
--     instances.
--
-- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
--     supported on Redis version 2.8.22 and later.
--
-- 'offeringType', 'describeReservedCacheNodes_offeringType' - The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid values:
-- @\"Light Utilization\"|\"Medium Utilization\"|\"Heavy Utilization\"|\"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"@
--
-- 'productDescription', 'describeReservedCacheNodes_productDescription' - The product description filter value. Use this parameter to show only
-- those reservations matching the specified product description.
--
-- 'reservedCacheNodeId', 'describeReservedCacheNodes_reservedCacheNodeId' - The reserved cache node identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reservation ID.
--
-- 'marker', 'describeReservedCacheNodes_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReservedCacheNodes_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
newDescribeReservedCacheNodes ::
  DescribeReservedCacheNodes
newDescribeReservedCacheNodes =
  DescribeReservedCacheNodes'
    { reservedCacheNodesOfferingId =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      reservedCacheNodeId = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
describeReservedCacheNodes_reservedCacheNodesOfferingId :: Lens.Lens' DescribeReservedCacheNodes (Prelude.Maybe Prelude.Text)
describeReservedCacheNodes_reservedCacheNodesOfferingId = Lens.lens (\DescribeReservedCacheNodes' {reservedCacheNodesOfferingId} -> reservedCacheNodesOfferingId) (\s@DescribeReservedCacheNodes' {} a -> s {reservedCacheNodesOfferingId = a} :: DescribeReservedCacheNodes)

-- | The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration.
--
-- Valid Values: @1 | 3 | 31536000 | 94608000@
describeReservedCacheNodes_duration :: Lens.Lens' DescribeReservedCacheNodes (Prelude.Maybe Prelude.Text)
describeReservedCacheNodes_duration = Lens.lens (\DescribeReservedCacheNodes' {duration} -> duration) (\s@DescribeReservedCacheNodes' {} a -> s {duration = a} :: DescribeReservedCacheNodes)

-- | The cache node type filter value. Use this parameter to show only those
-- reservations matching the specified cache node type.
--
-- The following node types are supported by ElastiCache. Generally
-- speaking, the current generation types provide more memory and
-- computational power at lower cost when compared to their equivalent
-- previous generation counterparts.
--
-- -   General purpose:
--
--     -   Current generation:
--
--         __M6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
--         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
--         @cache.m6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
--         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
--         @cache.m5.24xlarge@
--
--         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
--         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended)
--
--         __T1 node types:__ @cache.t1.micro@
--
--         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
--         @cache.m1.large@, @cache.m1.xlarge@
--
--         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--
-- -   Compute optimized:
--
--     -   Previous generation: (not recommended)
--
--         __C1 node types:__ @cache.c1.xlarge@
--
-- -   Memory optimized:
--
--     -   Current generation:
--
--         __R6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
--         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
--         @cache.r6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
--         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
--         @cache.r5.24xlarge@
--
--         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
--         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
--         @cache.r4.16xlarge@
--
--     -   Previous generation: (not recommended)
--
--         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
--         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--
-- __Additional node type info__
--
-- -   All current generation instance types are created in Amazon VPC by
--     default.
--
-- -   Redis append-only files (AOF) are not supported for T1 or T2
--     instances.
--
-- -   Redis Multi-AZ with automatic failover is not supported on T1
--     instances.
--
-- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
--     supported on Redis version 2.8.22 and later.
describeReservedCacheNodes_cacheNodeType :: Lens.Lens' DescribeReservedCacheNodes (Prelude.Maybe Prelude.Text)
describeReservedCacheNodes_cacheNodeType = Lens.lens (\DescribeReservedCacheNodes' {cacheNodeType} -> cacheNodeType) (\s@DescribeReservedCacheNodes' {} a -> s {cacheNodeType = a} :: DescribeReservedCacheNodes)

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type.
--
-- Valid values:
-- @\"Light Utilization\"|\"Medium Utilization\"|\"Heavy Utilization\"|\"All Upfront\"|\"Partial Upfront\"| \"No Upfront\"@
describeReservedCacheNodes_offeringType :: Lens.Lens' DescribeReservedCacheNodes (Prelude.Maybe Prelude.Text)
describeReservedCacheNodes_offeringType = Lens.lens (\DescribeReservedCacheNodes' {offeringType} -> offeringType) (\s@DescribeReservedCacheNodes' {} a -> s {offeringType = a} :: DescribeReservedCacheNodes)

-- | The product description filter value. Use this parameter to show only
-- those reservations matching the specified product description.
describeReservedCacheNodes_productDescription :: Lens.Lens' DescribeReservedCacheNodes (Prelude.Maybe Prelude.Text)
describeReservedCacheNodes_productDescription = Lens.lens (\DescribeReservedCacheNodes' {productDescription} -> productDescription) (\s@DescribeReservedCacheNodes' {} a -> s {productDescription = a} :: DescribeReservedCacheNodes)

-- | The reserved cache node identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reservation ID.
describeReservedCacheNodes_reservedCacheNodeId :: Lens.Lens' DescribeReservedCacheNodes (Prelude.Maybe Prelude.Text)
describeReservedCacheNodes_reservedCacheNodeId = Lens.lens (\DescribeReservedCacheNodes' {reservedCacheNodeId} -> reservedCacheNodeId) (\s@DescribeReservedCacheNodes' {} a -> s {reservedCacheNodeId = a} :: DescribeReservedCacheNodes)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeReservedCacheNodes_marker :: Lens.Lens' DescribeReservedCacheNodes (Prelude.Maybe Prelude.Text)
describeReservedCacheNodes_marker = Lens.lens (\DescribeReservedCacheNodes' {marker} -> marker) (\s@DescribeReservedCacheNodes' {} a -> s {marker = a} :: DescribeReservedCacheNodes)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeReservedCacheNodes_maxRecords :: Lens.Lens' DescribeReservedCacheNodes (Prelude.Maybe Prelude.Int)
describeReservedCacheNodes_maxRecords = Lens.lens (\DescribeReservedCacheNodes' {maxRecords} -> maxRecords) (\s@DescribeReservedCacheNodes' {} a -> s {maxRecords = a} :: DescribeReservedCacheNodes)

instance Core.AWSPager DescribeReservedCacheNodes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReservedCacheNodesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReservedCacheNodesResponse_reservedCacheNodes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReservedCacheNodes_marker
          Lens..~ rs
          Lens.^? describeReservedCacheNodesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeReservedCacheNodes where
  type
    AWSResponse DescribeReservedCacheNodes =
      DescribeReservedCacheNodesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeReservedCacheNodesResult"
      ( \s h x ->
          DescribeReservedCacheNodesResponse'
            Prelude.<$> ( x Core..@? "ReservedCacheNodes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "ReservedCacheNode")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReservedCacheNodes

instance Prelude.NFData DescribeReservedCacheNodes

instance Core.ToHeaders DescribeReservedCacheNodes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeReservedCacheNodes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReservedCacheNodes where
  toQuery DescribeReservedCacheNodes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeReservedCacheNodes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "ReservedCacheNodesOfferingId"
          Core.=: reservedCacheNodesOfferingId,
        "Duration" Core.=: duration,
        "CacheNodeType" Core.=: cacheNodeType,
        "OfferingType" Core.=: offeringType,
        "ProductDescription" Core.=: productDescription,
        "ReservedCacheNodeId" Core.=: reservedCacheNodeId,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Represents the output of a @DescribeReservedCacheNodes@ operation.
--
-- /See:/ 'newDescribeReservedCacheNodesResponse' smart constructor.
data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse'
  { -- | A list of reserved cache nodes. Each element in the list contains
    -- detailed information about one node.
    reservedCacheNodes :: Prelude.Maybe [ReservedCacheNode],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedCacheNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedCacheNodes', 'describeReservedCacheNodesResponse_reservedCacheNodes' - A list of reserved cache nodes. Each element in the list contains
-- detailed information about one node.
--
-- 'marker', 'describeReservedCacheNodesResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeReservedCacheNodesResponse_httpStatus' - The response's http status code.
newDescribeReservedCacheNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedCacheNodesResponse
newDescribeReservedCacheNodesResponse pHttpStatus_ =
  DescribeReservedCacheNodesResponse'
    { reservedCacheNodes =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of reserved cache nodes. Each element in the list contains
-- detailed information about one node.
describeReservedCacheNodesResponse_reservedCacheNodes :: Lens.Lens' DescribeReservedCacheNodesResponse (Prelude.Maybe [ReservedCacheNode])
describeReservedCacheNodesResponse_reservedCacheNodes = Lens.lens (\DescribeReservedCacheNodesResponse' {reservedCacheNodes} -> reservedCacheNodes) (\s@DescribeReservedCacheNodesResponse' {} a -> s {reservedCacheNodes = a} :: DescribeReservedCacheNodesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeReservedCacheNodesResponse_marker :: Lens.Lens' DescribeReservedCacheNodesResponse (Prelude.Maybe Prelude.Text)
describeReservedCacheNodesResponse_marker = Lens.lens (\DescribeReservedCacheNodesResponse' {marker} -> marker) (\s@DescribeReservedCacheNodesResponse' {} a -> s {marker = a} :: DescribeReservedCacheNodesResponse)

-- | The response's http status code.
describeReservedCacheNodesResponse_httpStatus :: Lens.Lens' DescribeReservedCacheNodesResponse Prelude.Int
describeReservedCacheNodesResponse_httpStatus = Lens.lens (\DescribeReservedCacheNodesResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedCacheNodesResponse' {} a -> s {httpStatus = a} :: DescribeReservedCacheNodesResponse)

instance
  Prelude.NFData
    DescribeReservedCacheNodesResponse
