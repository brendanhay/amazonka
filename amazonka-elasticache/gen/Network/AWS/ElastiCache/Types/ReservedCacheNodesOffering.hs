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
-- Module      : Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.RecurringCharge
import qualified Network.AWS.Lens as Lens

-- | Describes all of the attributes of a reserved cache node offering.
--
-- /See:/ 'newReservedCacheNodesOffering' smart constructor.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering'
  { -- | A unique identifier for the reserved cache node offering.
    reservedCacheNodesOfferingId :: Core.Maybe Core.Text,
    -- | The duration of the offering. in seconds.
    duration :: Core.Maybe Core.Int,
    -- | The cache node type for the reserved cache node.
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
    cacheNodeType :: Core.Maybe Core.Text,
    -- | The fixed price charged for this offering.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The hourly price charged for this offering.
    usagePrice :: Core.Maybe Core.Double,
    -- | The offering type.
    offeringType :: Core.Maybe Core.Text,
    -- | The recurring price charged to run this reserved cache node.
    recurringCharges :: Core.Maybe [RecurringCharge],
    -- | The cache engine used by the offering.
    productDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedCacheNodesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedCacheNodesOfferingId', 'reservedCacheNodesOffering_reservedCacheNodesOfferingId' - A unique identifier for the reserved cache node offering.
--
-- 'duration', 'reservedCacheNodesOffering_duration' - The duration of the offering. in seconds.
--
-- 'cacheNodeType', 'reservedCacheNodesOffering_cacheNodeType' - The cache node type for the reserved cache node.
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
-- 'fixedPrice', 'reservedCacheNodesOffering_fixedPrice' - The fixed price charged for this offering.
--
-- 'usagePrice', 'reservedCacheNodesOffering_usagePrice' - The hourly price charged for this offering.
--
-- 'offeringType', 'reservedCacheNodesOffering_offeringType' - The offering type.
--
-- 'recurringCharges', 'reservedCacheNodesOffering_recurringCharges' - The recurring price charged to run this reserved cache node.
--
-- 'productDescription', 'reservedCacheNodesOffering_productDescription' - The cache engine used by the offering.
newReservedCacheNodesOffering ::
  ReservedCacheNodesOffering
newReservedCacheNodesOffering =
  ReservedCacheNodesOffering'
    { reservedCacheNodesOfferingId =
        Core.Nothing,
      duration = Core.Nothing,
      cacheNodeType = Core.Nothing,
      fixedPrice = Core.Nothing,
      usagePrice = Core.Nothing,
      offeringType = Core.Nothing,
      recurringCharges = Core.Nothing,
      productDescription = Core.Nothing
    }

-- | A unique identifier for the reserved cache node offering.
reservedCacheNodesOffering_reservedCacheNodesOfferingId :: Lens.Lens' ReservedCacheNodesOffering (Core.Maybe Core.Text)
reservedCacheNodesOffering_reservedCacheNodesOfferingId = Lens.lens (\ReservedCacheNodesOffering' {reservedCacheNodesOfferingId} -> reservedCacheNodesOfferingId) (\s@ReservedCacheNodesOffering' {} a -> s {reservedCacheNodesOfferingId = a} :: ReservedCacheNodesOffering)

-- | The duration of the offering. in seconds.
reservedCacheNodesOffering_duration :: Lens.Lens' ReservedCacheNodesOffering (Core.Maybe Core.Int)
reservedCacheNodesOffering_duration = Lens.lens (\ReservedCacheNodesOffering' {duration} -> duration) (\s@ReservedCacheNodesOffering' {} a -> s {duration = a} :: ReservedCacheNodesOffering)

-- | The cache node type for the reserved cache node.
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
reservedCacheNodesOffering_cacheNodeType :: Lens.Lens' ReservedCacheNodesOffering (Core.Maybe Core.Text)
reservedCacheNodesOffering_cacheNodeType = Lens.lens (\ReservedCacheNodesOffering' {cacheNodeType} -> cacheNodeType) (\s@ReservedCacheNodesOffering' {} a -> s {cacheNodeType = a} :: ReservedCacheNodesOffering)

-- | The fixed price charged for this offering.
reservedCacheNodesOffering_fixedPrice :: Lens.Lens' ReservedCacheNodesOffering (Core.Maybe Core.Double)
reservedCacheNodesOffering_fixedPrice = Lens.lens (\ReservedCacheNodesOffering' {fixedPrice} -> fixedPrice) (\s@ReservedCacheNodesOffering' {} a -> s {fixedPrice = a} :: ReservedCacheNodesOffering)

-- | The hourly price charged for this offering.
reservedCacheNodesOffering_usagePrice :: Lens.Lens' ReservedCacheNodesOffering (Core.Maybe Core.Double)
reservedCacheNodesOffering_usagePrice = Lens.lens (\ReservedCacheNodesOffering' {usagePrice} -> usagePrice) (\s@ReservedCacheNodesOffering' {} a -> s {usagePrice = a} :: ReservedCacheNodesOffering)

-- | The offering type.
reservedCacheNodesOffering_offeringType :: Lens.Lens' ReservedCacheNodesOffering (Core.Maybe Core.Text)
reservedCacheNodesOffering_offeringType = Lens.lens (\ReservedCacheNodesOffering' {offeringType} -> offeringType) (\s@ReservedCacheNodesOffering' {} a -> s {offeringType = a} :: ReservedCacheNodesOffering)

-- | The recurring price charged to run this reserved cache node.
reservedCacheNodesOffering_recurringCharges :: Lens.Lens' ReservedCacheNodesOffering (Core.Maybe [RecurringCharge])
reservedCacheNodesOffering_recurringCharges = Lens.lens (\ReservedCacheNodesOffering' {recurringCharges} -> recurringCharges) (\s@ReservedCacheNodesOffering' {} a -> s {recurringCharges = a} :: ReservedCacheNodesOffering) Core.. Lens.mapping Lens._Coerce

-- | The cache engine used by the offering.
reservedCacheNodesOffering_productDescription :: Lens.Lens' ReservedCacheNodesOffering (Core.Maybe Core.Text)
reservedCacheNodesOffering_productDescription = Lens.lens (\ReservedCacheNodesOffering' {productDescription} -> productDescription) (\s@ReservedCacheNodesOffering' {} a -> s {productDescription = a} :: ReservedCacheNodesOffering)

instance Core.FromXML ReservedCacheNodesOffering where
  parseXML x =
    ReservedCacheNodesOffering'
      Core.<$> (x Core..@? "ReservedCacheNodesOfferingId")
      Core.<*> (x Core..@? "Duration")
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "FixedPrice")
      Core.<*> (x Core..@? "UsagePrice")
      Core.<*> (x Core..@? "OfferingType")
      Core.<*> ( x Core..@? "RecurringCharges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "RecurringCharge")
               )
      Core.<*> (x Core..@? "ProductDescription")

instance Core.Hashable ReservedCacheNodesOffering

instance Core.NFData ReservedCacheNodesOffering
