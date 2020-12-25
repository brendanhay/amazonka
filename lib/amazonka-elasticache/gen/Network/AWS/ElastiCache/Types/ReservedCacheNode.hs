{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReservedCacheNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReservedCacheNode
  ( ReservedCacheNode (..),

    -- * Smart constructor
    mkReservedCacheNode,

    -- * Lenses
    rcnCacheNodeCount,
    rcnCacheNodeType,
    rcnDuration,
    rcnFixedPrice,
    rcnOfferingType,
    rcnProductDescription,
    rcnRecurringCharges,
    rcnReservationARN,
    rcnReservedCacheNodeId,
    rcnReservedCacheNodesOfferingId,
    rcnStartTime,
    rcnState,
    rcnUsagePrice,
  )
where

import qualified Network.AWS.ElastiCache.Types.RecurringCharge as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @PurchaseReservedCacheNodesOffering@ operation.
--
-- /See:/ 'mkReservedCacheNode' smart constructor.
data ReservedCacheNode = ReservedCacheNode'
  { -- | The number of cache nodes that have been reserved.
    cacheNodeCount :: Core.Maybe Core.Int,
    -- | The cache node type for the reserved cache nodes.
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
    -- | The duration of the reservation in seconds.
    duration :: Core.Maybe Core.Int,
    -- | The fixed price charged for this reserved cache node.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The offering type of this reserved cache node.
    offeringType :: Core.Maybe Types.String,
    -- | The description of the reserved cache node.
    productDescription :: Core.Maybe Types.String,
    -- | The recurring price charged to run this reserved cache node.
    recurringCharges :: Core.Maybe [Types.RecurringCharge],
    -- | The Amazon Resource Name (ARN) of the reserved cache node.
    --
    -- Example: @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
    reservationARN :: Core.Maybe Types.String,
    -- | The unique identifier for the reservation.
    reservedCacheNodeId :: Core.Maybe Types.String,
    -- | The offering identifier.
    reservedCacheNodesOfferingId :: Core.Maybe Types.String,
    -- | The time the reservation started.
    startTime :: Core.Maybe Core.UTCTime,
    -- | The state of the reserved cache node.
    state :: Core.Maybe Types.String,
    -- | The hourly price charged for this reserved cache node.
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReservedCacheNode' value with any optional fields omitted.
mkReservedCacheNode ::
  ReservedCacheNode
mkReservedCacheNode =
  ReservedCacheNode'
    { cacheNodeCount = Core.Nothing,
      cacheNodeType = Core.Nothing,
      duration = Core.Nothing,
      fixedPrice = Core.Nothing,
      offeringType = Core.Nothing,
      productDescription = Core.Nothing,
      recurringCharges = Core.Nothing,
      reservationARN = Core.Nothing,
      reservedCacheNodeId = Core.Nothing,
      reservedCacheNodesOfferingId = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | The number of cache nodes that have been reserved.
--
-- /Note:/ Consider using 'cacheNodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnCacheNodeCount :: Lens.Lens' ReservedCacheNode (Core.Maybe Core.Int)
rcnCacheNodeCount = Lens.field @"cacheNodeCount"
{-# DEPRECATED rcnCacheNodeCount "Use generic-lens or generic-optics with 'cacheNodeCount' instead." #-}

-- | The cache node type for the reserved cache nodes.
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
rcnCacheNodeType :: Lens.Lens' ReservedCacheNode (Core.Maybe Types.String)
rcnCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED rcnCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The duration of the reservation in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnDuration :: Lens.Lens' ReservedCacheNode (Core.Maybe Core.Int)
rcnDuration = Lens.field @"duration"
{-# DEPRECATED rcnDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The fixed price charged for this reserved cache node.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnFixedPrice :: Lens.Lens' ReservedCacheNode (Core.Maybe Core.Double)
rcnFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED rcnFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The offering type of this reserved cache node.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnOfferingType :: Lens.Lens' ReservedCacheNode (Core.Maybe Types.String)
rcnOfferingType = Lens.field @"offeringType"
{-# DEPRECATED rcnOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The description of the reserved cache node.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnProductDescription :: Lens.Lens' ReservedCacheNode (Core.Maybe Types.String)
rcnProductDescription = Lens.field @"productDescription"
{-# DEPRECATED rcnProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The recurring price charged to run this reserved cache node.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnRecurringCharges :: Lens.Lens' ReservedCacheNode (Core.Maybe [Types.RecurringCharge])
rcnRecurringCharges = Lens.field @"recurringCharges"
{-# DEPRECATED rcnRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The Amazon Resource Name (ARN) of the reserved cache node.
--
-- Example: @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
--
-- /Note:/ Consider using 'reservationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnReservationARN :: Lens.Lens' ReservedCacheNode (Core.Maybe Types.String)
rcnReservationARN = Lens.field @"reservationARN"
{-# DEPRECATED rcnReservationARN "Use generic-lens or generic-optics with 'reservationARN' instead." #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedCacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnReservedCacheNodeId :: Lens.Lens' ReservedCacheNode (Core.Maybe Types.String)
rcnReservedCacheNodeId = Lens.field @"reservedCacheNodeId"
{-# DEPRECATED rcnReservedCacheNodeId "Use generic-lens or generic-optics with 'reservedCacheNodeId' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnReservedCacheNodesOfferingId :: Lens.Lens' ReservedCacheNode (Core.Maybe Types.String)
rcnReservedCacheNodesOfferingId = Lens.field @"reservedCacheNodesOfferingId"
{-# DEPRECATED rcnReservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead." #-}

-- | The time the reservation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnStartTime :: Lens.Lens' ReservedCacheNode (Core.Maybe Core.UTCTime)
rcnStartTime = Lens.field @"startTime"
{-# DEPRECATED rcnStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The state of the reserved cache node.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnState :: Lens.Lens' ReservedCacheNode (Core.Maybe Types.String)
rcnState = Lens.field @"state"
{-# DEPRECATED rcnState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The hourly price charged for this reserved cache node.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnUsagePrice :: Lens.Lens' ReservedCacheNode (Core.Maybe Core.Double)
rcnUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED rcnUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromXML ReservedCacheNode where
  parseXML x =
    ReservedCacheNode'
      Core.<$> (x Core..@? "CacheNodeCount")
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "Duration")
      Core.<*> (x Core..@? "FixedPrice")
      Core.<*> (x Core..@? "OfferingType")
      Core.<*> (x Core..@? "ProductDescription")
      Core.<*> ( x Core..@? "RecurringCharges"
                   Core..<@> Core.parseXMLList "RecurringCharge"
               )
      Core.<*> (x Core..@? "ReservationARN")
      Core.<*> (x Core..@? "ReservedCacheNodeId")
      Core.<*> (x Core..@? "ReservedCacheNodesOfferingId")
      Core.<*> (x Core..@? "StartTime")
      Core.<*> (x Core..@? "State")
      Core.<*> (x Core..@? "UsagePrice")
