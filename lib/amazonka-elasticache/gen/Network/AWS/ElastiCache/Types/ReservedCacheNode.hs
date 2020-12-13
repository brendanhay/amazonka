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
    rcnCacheNodeType,
    rcnState,
    rcnStartTime,
    rcnProductDescription,
    rcnReservationARN,
    rcnCacheNodeCount,
    rcnReservedCacheNodeId,
    rcnRecurringCharges,
    rcnOfferingType,
    rcnUsagePrice,
    rcnFixedPrice,
    rcnDuration,
    rcnReservedCacheNodesOfferingId,
  )
where

import Network.AWS.ElastiCache.Types.RecurringCharge
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @PurchaseReservedCacheNodesOffering@ operation.
--
-- /See:/ 'mkReservedCacheNode' smart constructor.
data ReservedCacheNode = ReservedCacheNode'
  { -- | The cache node type for the reserved cache nodes.
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
    cacheNodeType :: Lude.Maybe Lude.Text,
    -- | The state of the reserved cache node.
    state :: Lude.Maybe Lude.Text,
    -- | The time the reservation started.
    startTime :: Lude.Maybe Lude.DateTime,
    -- | The description of the reserved cache node.
    productDescription :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the reserved cache node.
    --
    -- Example: @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
    reservationARN :: Lude.Maybe Lude.Text,
    -- | The number of cache nodes that have been reserved.
    cacheNodeCount :: Lude.Maybe Lude.Int,
    -- | The unique identifier for the reservation.
    reservedCacheNodeId :: Lude.Maybe Lude.Text,
    -- | The recurring price charged to run this reserved cache node.
    recurringCharges :: Lude.Maybe [RecurringCharge],
    -- | The offering type of this reserved cache node.
    offeringType :: Lude.Maybe Lude.Text,
    -- | The hourly price charged for this reserved cache node.
    usagePrice :: Lude.Maybe Lude.Double,
    -- | The fixed price charged for this reserved cache node.
    fixedPrice :: Lude.Maybe Lude.Double,
    -- | The duration of the reservation in seconds.
    duration :: Lude.Maybe Lude.Int,
    -- | The offering identifier.
    reservedCacheNodesOfferingId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedCacheNode' with the minimum fields required to make a request.
--
-- * 'cacheNodeType' - The cache node type for the reserved cache nodes.
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
-- * 'state' - The state of the reserved cache node.
-- * 'startTime' - The time the reservation started.
-- * 'productDescription' - The description of the reserved cache node.
-- * 'reservationARN' - The Amazon Resource Name (ARN) of the reserved cache node.
--
-- Example: @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
-- * 'cacheNodeCount' - The number of cache nodes that have been reserved.
-- * 'reservedCacheNodeId' - The unique identifier for the reservation.
-- * 'recurringCharges' - The recurring price charged to run this reserved cache node.
-- * 'offeringType' - The offering type of this reserved cache node.
-- * 'usagePrice' - The hourly price charged for this reserved cache node.
-- * 'fixedPrice' - The fixed price charged for this reserved cache node.
-- * 'duration' - The duration of the reservation in seconds.
-- * 'reservedCacheNodesOfferingId' - The offering identifier.
mkReservedCacheNode ::
  ReservedCacheNode
mkReservedCacheNode =
  ReservedCacheNode'
    { cacheNodeType = Lude.Nothing,
      state = Lude.Nothing,
      startTime = Lude.Nothing,
      productDescription = Lude.Nothing,
      reservationARN = Lude.Nothing,
      cacheNodeCount = Lude.Nothing,
      reservedCacheNodeId = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      duration = Lude.Nothing,
      reservedCacheNodesOfferingId = Lude.Nothing
    }

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
rcnCacheNodeType :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Text)
rcnCacheNodeType = Lens.lens (cacheNodeType :: ReservedCacheNode -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: ReservedCacheNode)
{-# DEPRECATED rcnCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The state of the reserved cache node.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnState :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Text)
rcnState = Lens.lens (state :: ReservedCacheNode -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ReservedCacheNode)
{-# DEPRECATED rcnState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The time the reservation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnStartTime :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.DateTime)
rcnStartTime = Lens.lens (startTime :: ReservedCacheNode -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: ReservedCacheNode)
{-# DEPRECATED rcnStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The description of the reserved cache node.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnProductDescription :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Text)
rcnProductDescription = Lens.lens (productDescription :: ReservedCacheNode -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: ReservedCacheNode)
{-# DEPRECATED rcnProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The Amazon Resource Name (ARN) of the reserved cache node.
--
-- Example: @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
--
-- /Note:/ Consider using 'reservationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnReservationARN :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Text)
rcnReservationARN = Lens.lens (reservationARN :: ReservedCacheNode -> Lude.Maybe Lude.Text) (\s a -> s {reservationARN = a} :: ReservedCacheNode)
{-# DEPRECATED rcnReservationARN "Use generic-lens or generic-optics with 'reservationARN' instead." #-}

-- | The number of cache nodes that have been reserved.
--
-- /Note:/ Consider using 'cacheNodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnCacheNodeCount :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Int)
rcnCacheNodeCount = Lens.lens (cacheNodeCount :: ReservedCacheNode -> Lude.Maybe Lude.Int) (\s a -> s {cacheNodeCount = a} :: ReservedCacheNode)
{-# DEPRECATED rcnCacheNodeCount "Use generic-lens or generic-optics with 'cacheNodeCount' instead." #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedCacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnReservedCacheNodeId :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Text)
rcnReservedCacheNodeId = Lens.lens (reservedCacheNodeId :: ReservedCacheNode -> Lude.Maybe Lude.Text) (\s a -> s {reservedCacheNodeId = a} :: ReservedCacheNode)
{-# DEPRECATED rcnReservedCacheNodeId "Use generic-lens or generic-optics with 'reservedCacheNodeId' instead." #-}

-- | The recurring price charged to run this reserved cache node.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnRecurringCharges :: Lens.Lens' ReservedCacheNode (Lude.Maybe [RecurringCharge])
rcnRecurringCharges = Lens.lens (recurringCharges :: ReservedCacheNode -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedCacheNode)
{-# DEPRECATED rcnRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The offering type of this reserved cache node.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnOfferingType :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Text)
rcnOfferingType = Lens.lens (offeringType :: ReservedCacheNode -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: ReservedCacheNode)
{-# DEPRECATED rcnOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The hourly price charged for this reserved cache node.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnUsagePrice :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Double)
rcnUsagePrice = Lens.lens (usagePrice :: ReservedCacheNode -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedCacheNode)
{-# DEPRECATED rcnUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The fixed price charged for this reserved cache node.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnFixedPrice :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Double)
rcnFixedPrice = Lens.lens (fixedPrice :: ReservedCacheNode -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedCacheNode)
{-# DEPRECATED rcnFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The duration of the reservation in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnDuration :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Int)
rcnDuration = Lens.lens (duration :: ReservedCacheNode -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ReservedCacheNode)
{-# DEPRECATED rcnDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcnReservedCacheNodesOfferingId :: Lens.Lens' ReservedCacheNode (Lude.Maybe Lude.Text)
rcnReservedCacheNodesOfferingId = Lens.lens (reservedCacheNodesOfferingId :: ReservedCacheNode -> Lude.Maybe Lude.Text) (\s a -> s {reservedCacheNodesOfferingId = a} :: ReservedCacheNode)
{-# DEPRECATED rcnReservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead." #-}

instance Lude.FromXML ReservedCacheNode where
  parseXML x =
    ReservedCacheNode'
      Lude.<$> (x Lude..@? "CacheNodeType")
      Lude.<*> (x Lude..@? "State")
      Lude.<*> (x Lude..@? "StartTime")
      Lude.<*> (x Lude..@? "ProductDescription")
      Lude.<*> (x Lude..@? "ReservationARN")
      Lude.<*> (x Lude..@? "CacheNodeCount")
      Lude.<*> (x Lude..@? "ReservedCacheNodeId")
      Lude.<*> ( x Lude..@? "RecurringCharges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "RecurringCharge")
               )
      Lude.<*> (x Lude..@? "OfferingType")
      Lude.<*> (x Lude..@? "UsagePrice")
      Lude.<*> (x Lude..@? "FixedPrice")
      Lude.<*> (x Lude..@? "Duration")
      Lude.<*> (x Lude..@? "ReservedCacheNodesOfferingId")
