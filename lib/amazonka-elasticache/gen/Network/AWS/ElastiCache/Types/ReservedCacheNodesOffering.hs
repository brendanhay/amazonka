-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering
  ( ReservedCacheNodesOffering (..),

    -- * Smart constructor
    mkReservedCacheNodesOffering,

    -- * Lenses
    rcnoCacheNodeType,
    rcnoProductDescription,
    rcnoRecurringCharges,
    rcnoOfferingType,
    rcnoUsagePrice,
    rcnoFixedPrice,
    rcnoDuration,
    rcnoReservedCacheNodesOfferingId,
  )
where

import Network.AWS.ElastiCache.Types.RecurringCharge
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes all of the attributes of a reserved cache node offering.
--
-- /See:/ 'mkReservedCacheNodesOffering' smart constructor.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering'
  { cacheNodeType ::
      Lude.Maybe Lude.Text,
    productDescription ::
      Lude.Maybe Lude.Text,
    recurringCharges ::
      Lude.Maybe [RecurringCharge],
    offeringType :: Lude.Maybe Lude.Text,
    usagePrice :: Lude.Maybe Lude.Double,
    fixedPrice :: Lude.Maybe Lude.Double,
    duration :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'ReservedCacheNodesOffering' with the minimum fields required to make a request.
--
-- * 'cacheNodeType' - The cache node type for the reserved cache node.
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
-- * 'duration' - The duration of the offering. in seconds.
-- * 'fixedPrice' - The fixed price charged for this offering.
-- * 'offeringType' - The offering type.
-- * 'productDescription' - The cache engine used by the offering.
-- * 'recurringCharges' - The recurring price charged to run this reserved cache node.
-- * 'reservedCacheNodesOfferingId' - A unique identifier for the reserved cache node offering.
-- * 'usagePrice' - The hourly price charged for this offering.
mkReservedCacheNodesOffering ::
  ReservedCacheNodesOffering
mkReservedCacheNodesOffering =
  ReservedCacheNodesOffering'
    { cacheNodeType = Lude.Nothing,
      productDescription = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      duration = Lude.Nothing,
      reservedCacheNodesOfferingId = Lude.Nothing
    }

-- | The cache node type for the reserved cache node.
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
rcnoCacheNodeType :: Lens.Lens' ReservedCacheNodesOffering (Lude.Maybe Lude.Text)
rcnoCacheNodeType = Lens.lens (cacheNodeType :: ReservedCacheNodesOffering -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: ReservedCacheNodesOffering)
{-# DEPRECATED rcnoCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The cache engine used by the offering.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnoProductDescription :: Lens.Lens' ReservedCacheNodesOffering (Lude.Maybe Lude.Text)
rcnoProductDescription = Lens.lens (productDescription :: ReservedCacheNodesOffering -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: ReservedCacheNodesOffering)
{-# DEPRECATED rcnoProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The recurring price charged to run this reserved cache node.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnoRecurringCharges :: Lens.Lens' ReservedCacheNodesOffering (Lude.Maybe [RecurringCharge])
rcnoRecurringCharges = Lens.lens (recurringCharges :: ReservedCacheNodesOffering -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedCacheNodesOffering)
{-# DEPRECATED rcnoRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnoOfferingType :: Lens.Lens' ReservedCacheNodesOffering (Lude.Maybe Lude.Text)
rcnoOfferingType = Lens.lens (offeringType :: ReservedCacheNodesOffering -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: ReservedCacheNodesOffering)
{-# DEPRECATED rcnoOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The hourly price charged for this offering.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnoUsagePrice :: Lens.Lens' ReservedCacheNodesOffering (Lude.Maybe Lude.Double)
rcnoUsagePrice = Lens.lens (usagePrice :: ReservedCacheNodesOffering -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedCacheNodesOffering)
{-# DEPRECATED rcnoUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The fixed price charged for this offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnoFixedPrice :: Lens.Lens' ReservedCacheNodesOffering (Lude.Maybe Lude.Double)
rcnoFixedPrice = Lens.lens (fixedPrice :: ReservedCacheNodesOffering -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedCacheNodesOffering)
{-# DEPRECATED rcnoFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The duration of the offering. in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnoDuration :: Lens.Lens' ReservedCacheNodesOffering (Lude.Maybe Lude.Int)
rcnoDuration = Lens.lens (duration :: ReservedCacheNodesOffering -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ReservedCacheNodesOffering)
{-# DEPRECATED rcnoDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | A unique identifier for the reserved cache node offering.
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnoReservedCacheNodesOfferingId :: Lens.Lens' ReservedCacheNodesOffering (Lude.Maybe Lude.Text)
rcnoReservedCacheNodesOfferingId = Lens.lens (reservedCacheNodesOfferingId :: ReservedCacheNodesOffering -> Lude.Maybe Lude.Text) (\s a -> s {reservedCacheNodesOfferingId = a} :: ReservedCacheNodesOffering)
{-# DEPRECATED rcnoReservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead." #-}

instance Lude.FromXML ReservedCacheNodesOffering where
  parseXML x =
    ReservedCacheNodesOffering'
      Lude.<$> (x Lude..@? "CacheNodeType")
      Lude.<*> (x Lude..@? "ProductDescription")
      Lude.<*> ( x Lude..@? "RecurringCharges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "RecurringCharge")
               )
      Lude.<*> (x Lude..@? "OfferingType")
      Lude.<*> (x Lude..@? "UsagePrice")
      Lude.<*> (x Lude..@? "FixedPrice")
      Lude.<*> (x Lude..@? "Duration")
      Lude.<*> (x Lude..@? "ReservedCacheNodesOfferingId")
