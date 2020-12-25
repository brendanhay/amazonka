{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ReservedNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ReservedNode
  ( ReservedNode (..),

    -- * Smart constructor
    mkReservedNode,

    -- * Lenses
    rnCurrencyCode,
    rnDuration,
    rnFixedPrice,
    rnNodeCount,
    rnNodeType,
    rnOfferingType,
    rnRecurringCharges,
    rnReservedNodeId,
    rnReservedNodeOfferingId,
    rnReservedNodeOfferingType,
    rnStartTime,
    rnState,
    rnUsagePrice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.RecurringCharge as Types
import qualified Network.AWS.Redshift.Types.ReservedNodeOfferingType as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes a reserved node. You can call the 'DescribeReservedNodeOfferings' API to obtain the available reserved node offerings.
--
-- /See:/ 'mkReservedNode' smart constructor.
data ReservedNode = ReservedNode'
  { -- | The currency code for the reserved cluster.
    currencyCode :: Core.Maybe Types.String,
    -- | The duration of the node reservation in seconds.
    duration :: Core.Maybe Core.Int,
    -- | The fixed cost Amazon Redshift charges you for this reserved node.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The number of reserved compute nodes.
    nodeCount :: Core.Maybe Core.Int,
    -- | The node type of the reserved node.
    nodeType :: Core.Maybe Types.String,
    -- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
    offeringType :: Core.Maybe Types.String,
    -- | The recurring charges for the reserved node.
    recurringCharges :: Core.Maybe [Types.RecurringCharge],
    -- | The unique identifier for the reservation.
    reservedNodeId :: Core.Maybe Types.String,
    -- | The identifier for the reserved node offering.
    reservedNodeOfferingId :: Core.Maybe Types.String,
    -- |
    reservedNodeOfferingType :: Core.Maybe Types.ReservedNodeOfferingType,
    -- | The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
    startTime :: Core.Maybe Core.UTCTime,
    -- | The state of the reserved compute node.
    --
    -- Possible Values:
    --
    --     * pending-payment-This reserved node has recently been purchased, and the sale has been approved, but payment has not yet been confirmed.
    --
    --
    --     * active-This reserved node is owned by the caller and is available for use.
    --
    --
    --     * payment-failed-Payment failed for the purchase attempt.
    --
    --
    --     * retired-The reserved node is no longer available.
    --
    --
    --     * exchanging-The owner is exchanging the reserved node for another reserved node.
    state :: Core.Maybe Types.String,
    -- | The hourly rate Amazon Redshift charges you for this reserved node.
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReservedNode' value with any optional fields omitted.
mkReservedNode ::
  ReservedNode
mkReservedNode =
  ReservedNode'
    { currencyCode = Core.Nothing,
      duration = Core.Nothing,
      fixedPrice = Core.Nothing,
      nodeCount = Core.Nothing,
      nodeType = Core.Nothing,
      offeringType = Core.Nothing,
      recurringCharges = Core.Nothing,
      reservedNodeId = Core.Nothing,
      reservedNodeOfferingId = Core.Nothing,
      reservedNodeOfferingType = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | The currency code for the reserved cluster.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnCurrencyCode :: Lens.Lens' ReservedNode (Core.Maybe Types.String)
rnCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED rnCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The duration of the node reservation in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnDuration :: Lens.Lens' ReservedNode (Core.Maybe Core.Int)
rnDuration = Lens.field @"duration"
{-# DEPRECATED rnDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The fixed cost Amazon Redshift charges you for this reserved node.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnFixedPrice :: Lens.Lens' ReservedNode (Core.Maybe Core.Double)
rnFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED rnFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The number of reserved compute nodes.
--
-- /Note:/ Consider using 'nodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeCount :: Lens.Lens' ReservedNode (Core.Maybe Core.Int)
rnNodeCount = Lens.field @"nodeCount"
{-# DEPRECATED rnNodeCount "Use generic-lens or generic-optics with 'nodeCount' instead." #-}

-- | The node type of the reserved node.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeType :: Lens.Lens' ReservedNode (Core.Maybe Types.String)
rnNodeType = Lens.field @"nodeType"
{-# DEPRECATED rnNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnOfferingType :: Lens.Lens' ReservedNode (Core.Maybe Types.String)
rnOfferingType = Lens.field @"offeringType"
{-# DEPRECATED rnOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The recurring charges for the reserved node.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnRecurringCharges :: Lens.Lens' ReservedNode (Core.Maybe [Types.RecurringCharge])
rnRecurringCharges = Lens.field @"recurringCharges"
{-# DEPRECATED rnRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeId :: Lens.Lens' ReservedNode (Core.Maybe Types.String)
rnReservedNodeId = Lens.field @"reservedNodeId"
{-# DEPRECATED rnReservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead." #-}

-- | The identifier for the reserved node offering.
--
-- /Note:/ Consider using 'reservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeOfferingId :: Lens.Lens' ReservedNode (Core.Maybe Types.String)
rnReservedNodeOfferingId = Lens.field @"reservedNodeOfferingId"
{-# DEPRECATED rnReservedNodeOfferingId "Use generic-lens or generic-optics with 'reservedNodeOfferingId' instead." #-}

-- |
--
-- /Note:/ Consider using 'reservedNodeOfferingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeOfferingType :: Lens.Lens' ReservedNode (Core.Maybe Types.ReservedNodeOfferingType)
rnReservedNodeOfferingType = Lens.field @"reservedNodeOfferingType"
{-# DEPRECATED rnReservedNodeOfferingType "Use generic-lens or generic-optics with 'reservedNodeOfferingType' instead." #-}

-- | The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnStartTime :: Lens.Lens' ReservedNode (Core.Maybe Core.UTCTime)
rnStartTime = Lens.field @"startTime"
{-# DEPRECATED rnStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The state of the reserved compute node.
--
-- Possible Values:
--
--     * pending-payment-This reserved node has recently been purchased, and the sale has been approved, but payment has not yet been confirmed.
--
--
--     * active-This reserved node is owned by the caller and is available for use.
--
--
--     * payment-failed-Payment failed for the purchase attempt.
--
--
--     * retired-The reserved node is no longer available.
--
--
--     * exchanging-The owner is exchanging the reserved node for another reserved node.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnState :: Lens.Lens' ReservedNode (Core.Maybe Types.String)
rnState = Lens.field @"state"
{-# DEPRECATED rnState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The hourly rate Amazon Redshift charges you for this reserved node.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnUsagePrice :: Lens.Lens' ReservedNode (Core.Maybe Core.Double)
rnUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED rnUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromXML ReservedNode where
  parseXML x =
    ReservedNode'
      Core.<$> (x Core..@? "CurrencyCode")
      Core.<*> (x Core..@? "Duration")
      Core.<*> (x Core..@? "FixedPrice")
      Core.<*> (x Core..@? "NodeCount")
      Core.<*> (x Core..@? "NodeType")
      Core.<*> (x Core..@? "OfferingType")
      Core.<*> ( x Core..@? "RecurringCharges"
                   Core..<@> Core.parseXMLList "RecurringCharge"
               )
      Core.<*> (x Core..@? "ReservedNodeId")
      Core.<*> (x Core..@? "ReservedNodeOfferingId")
      Core.<*> (x Core..@? "ReservedNodeOfferingType")
      Core.<*> (x Core..@? "StartTime")
      Core.<*> (x Core..@? "State")
      Core.<*> (x Core..@? "UsagePrice")
