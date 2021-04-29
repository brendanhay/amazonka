{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReservedCacheNode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReservedCacheNode where

import Network.AWS.ElastiCache.Types.RecurringCharge
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a @PurchaseReservedCacheNodesOffering@
-- operation.
--
-- /See:/ 'newReservedCacheNode' smart constructor.
data ReservedCacheNode = ReservedCacheNode'
  { -- | The offering identifier.
    reservedCacheNodesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The duration of the reservation in seconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The time the reservation started.
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The state of the reserved cache node.
    state :: Prelude.Maybe Prelude.Text,
    -- | The number of cache nodes that have been reserved.
    cacheNodeCount :: Prelude.Maybe Prelude.Int,
    -- | The cache node type for the reserved cache nodes.
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
    -- | The fixed price charged for this reserved cache node.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The hourly price charged for this reserved cache node.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The offering type of this reserved cache node.
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The recurring price charged to run this reserved cache node.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The description of the reserved cache node.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the reservation.
    reservedCacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the reserved cache node.
    --
    -- Example:
    -- @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
    reservationARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservedCacheNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedCacheNodesOfferingId', 'reservedCacheNode_reservedCacheNodesOfferingId' - The offering identifier.
--
-- 'duration', 'reservedCacheNode_duration' - The duration of the reservation in seconds.
--
-- 'startTime', 'reservedCacheNode_startTime' - The time the reservation started.
--
-- 'state', 'reservedCacheNode_state' - The state of the reserved cache node.
--
-- 'cacheNodeCount', 'reservedCacheNode_cacheNodeCount' - The number of cache nodes that have been reserved.
--
-- 'cacheNodeType', 'reservedCacheNode_cacheNodeType' - The cache node type for the reserved cache nodes.
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
-- 'fixedPrice', 'reservedCacheNode_fixedPrice' - The fixed price charged for this reserved cache node.
--
-- 'usagePrice', 'reservedCacheNode_usagePrice' - The hourly price charged for this reserved cache node.
--
-- 'offeringType', 'reservedCacheNode_offeringType' - The offering type of this reserved cache node.
--
-- 'recurringCharges', 'reservedCacheNode_recurringCharges' - The recurring price charged to run this reserved cache node.
--
-- 'productDescription', 'reservedCacheNode_productDescription' - The description of the reserved cache node.
--
-- 'reservedCacheNodeId', 'reservedCacheNode_reservedCacheNodeId' - The unique identifier for the reservation.
--
-- 'reservationARN', 'reservedCacheNode_reservationARN' - The Amazon Resource Name (ARN) of the reserved cache node.
--
-- Example:
-- @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
newReservedCacheNode ::
  ReservedCacheNode
newReservedCacheNode =
  ReservedCacheNode'
    { reservedCacheNodesOfferingId =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      cacheNodeCount = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      reservedCacheNodeId = Prelude.Nothing,
      reservationARN = Prelude.Nothing
    }

-- | The offering identifier.
reservedCacheNode_reservedCacheNodesOfferingId :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Text)
reservedCacheNode_reservedCacheNodesOfferingId = Lens.lens (\ReservedCacheNode' {reservedCacheNodesOfferingId} -> reservedCacheNodesOfferingId) (\s@ReservedCacheNode' {} a -> s {reservedCacheNodesOfferingId = a} :: ReservedCacheNode)

-- | The duration of the reservation in seconds.
reservedCacheNode_duration :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Int)
reservedCacheNode_duration = Lens.lens (\ReservedCacheNode' {duration} -> duration) (\s@ReservedCacheNode' {} a -> s {duration = a} :: ReservedCacheNode)

-- | The time the reservation started.
reservedCacheNode_startTime :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.UTCTime)
reservedCacheNode_startTime = Lens.lens (\ReservedCacheNode' {startTime} -> startTime) (\s@ReservedCacheNode' {} a -> s {startTime = a} :: ReservedCacheNode) Prelude.. Lens.mapping Prelude._Time

-- | The state of the reserved cache node.
reservedCacheNode_state :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Text)
reservedCacheNode_state = Lens.lens (\ReservedCacheNode' {state} -> state) (\s@ReservedCacheNode' {} a -> s {state = a} :: ReservedCacheNode)

-- | The number of cache nodes that have been reserved.
reservedCacheNode_cacheNodeCount :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Int)
reservedCacheNode_cacheNodeCount = Lens.lens (\ReservedCacheNode' {cacheNodeCount} -> cacheNodeCount) (\s@ReservedCacheNode' {} a -> s {cacheNodeCount = a} :: ReservedCacheNode)

-- | The cache node type for the reserved cache nodes.
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
reservedCacheNode_cacheNodeType :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Text)
reservedCacheNode_cacheNodeType = Lens.lens (\ReservedCacheNode' {cacheNodeType} -> cacheNodeType) (\s@ReservedCacheNode' {} a -> s {cacheNodeType = a} :: ReservedCacheNode)

-- | The fixed price charged for this reserved cache node.
reservedCacheNode_fixedPrice :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Double)
reservedCacheNode_fixedPrice = Lens.lens (\ReservedCacheNode' {fixedPrice} -> fixedPrice) (\s@ReservedCacheNode' {} a -> s {fixedPrice = a} :: ReservedCacheNode)

-- | The hourly price charged for this reserved cache node.
reservedCacheNode_usagePrice :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Double)
reservedCacheNode_usagePrice = Lens.lens (\ReservedCacheNode' {usagePrice} -> usagePrice) (\s@ReservedCacheNode' {} a -> s {usagePrice = a} :: ReservedCacheNode)

-- | The offering type of this reserved cache node.
reservedCacheNode_offeringType :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Text)
reservedCacheNode_offeringType = Lens.lens (\ReservedCacheNode' {offeringType} -> offeringType) (\s@ReservedCacheNode' {} a -> s {offeringType = a} :: ReservedCacheNode)

-- | The recurring price charged to run this reserved cache node.
reservedCacheNode_recurringCharges :: Lens.Lens' ReservedCacheNode (Prelude.Maybe [RecurringCharge])
reservedCacheNode_recurringCharges = Lens.lens (\ReservedCacheNode' {recurringCharges} -> recurringCharges) (\s@ReservedCacheNode' {} a -> s {recurringCharges = a} :: ReservedCacheNode) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the reserved cache node.
reservedCacheNode_productDescription :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Text)
reservedCacheNode_productDescription = Lens.lens (\ReservedCacheNode' {productDescription} -> productDescription) (\s@ReservedCacheNode' {} a -> s {productDescription = a} :: ReservedCacheNode)

-- | The unique identifier for the reservation.
reservedCacheNode_reservedCacheNodeId :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Text)
reservedCacheNode_reservedCacheNodeId = Lens.lens (\ReservedCacheNode' {reservedCacheNodeId} -> reservedCacheNodeId) (\s@ReservedCacheNode' {} a -> s {reservedCacheNodeId = a} :: ReservedCacheNode)

-- | The Amazon Resource Name (ARN) of the reserved cache node.
--
-- Example:
-- @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
reservedCacheNode_reservationARN :: Lens.Lens' ReservedCacheNode (Prelude.Maybe Prelude.Text)
reservedCacheNode_reservationARN = Lens.lens (\ReservedCacheNode' {reservationARN} -> reservationARN) (\s@ReservedCacheNode' {} a -> s {reservationARN = a} :: ReservedCacheNode)

instance Prelude.FromXML ReservedCacheNode where
  parseXML x =
    ReservedCacheNode'
      Prelude.<$> (x Prelude..@? "ReservedCacheNodesOfferingId")
      Prelude.<*> (x Prelude..@? "Duration")
      Prelude.<*> (x Prelude..@? "StartTime")
      Prelude.<*> (x Prelude..@? "State")
      Prelude.<*> (x Prelude..@? "CacheNodeCount")
      Prelude.<*> (x Prelude..@? "CacheNodeType")
      Prelude.<*> (x Prelude..@? "FixedPrice")
      Prelude.<*> (x Prelude..@? "UsagePrice")
      Prelude.<*> (x Prelude..@? "OfferingType")
      Prelude.<*> ( x Prelude..@? "RecurringCharges"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "RecurringCharge")
                  )
      Prelude.<*> (x Prelude..@? "ProductDescription")
      Prelude.<*> (x Prelude..@? "ReservedCacheNodeId")
      Prelude.<*> (x Prelude..@? "ReservationARN")

instance Prelude.Hashable ReservedCacheNode

instance Prelude.NFData ReservedCacheNode
