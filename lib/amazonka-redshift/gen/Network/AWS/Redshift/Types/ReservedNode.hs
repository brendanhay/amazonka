{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ReservedNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ReservedNode
  ( ReservedNode (..)
  -- * Smart constructor
  , mkReservedNode
  -- * Lenses
  , rnCurrencyCode
  , rnDuration
  , rnFixedPrice
  , rnNodeCount
  , rnNodeType
  , rnOfferingType
  , rnRecurringCharges
  , rnReservedNodeId
  , rnReservedNodeOfferingId
  , rnReservedNodeOfferingType
  , rnStartTime
  , rnState
  , rnUsagePrice
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.RecurringCharge as Types
import qualified Network.AWS.Redshift.Types.ReservedNodeOfferingType as Types

-- | Describes a reserved node. You can call the 'DescribeReservedNodeOfferings' API to obtain the available reserved node offerings. 
--
-- /See:/ 'mkReservedNode' smart constructor.
data ReservedNode = ReservedNode'
  { currencyCode :: Core.Maybe Core.Text
    -- ^ The currency code for the reserved cluster.
  , duration :: Core.Maybe Core.Int
    -- ^ The duration of the node reservation in seconds.
  , fixedPrice :: Core.Maybe Core.Double
    -- ^ The fixed cost Amazon Redshift charges you for this reserved node.
  , nodeCount :: Core.Maybe Core.Int
    -- ^ The number of reserved compute nodes.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The node type of the reserved node.
  , offeringType :: Core.Maybe Core.Text
    -- ^ The anticipated utilization of the reserved node, as defined in the reserved node offering.
  , recurringCharges :: Core.Maybe [Types.RecurringCharge]
    -- ^ The recurring charges for the reserved node.
  , reservedNodeId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the reservation.
  , reservedNodeOfferingId :: Core.Maybe Core.Text
    -- ^ The identifier for the reserved node offering.
  , reservedNodeOfferingType :: Core.Maybe Types.ReservedNodeOfferingType
    -- ^ 
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
  , state :: Core.Maybe Core.Text
    -- ^ The state of the reserved compute node.
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
  , usagePrice :: Core.Maybe Core.Double
    -- ^ The hourly rate Amazon Redshift charges you for this reserved node.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReservedNode' value with any optional fields omitted.
mkReservedNode
    :: ReservedNode
mkReservedNode
  = ReservedNode'{currencyCode = Core.Nothing,
                  duration = Core.Nothing, fixedPrice = Core.Nothing,
                  nodeCount = Core.Nothing, nodeType = Core.Nothing,
                  offeringType = Core.Nothing, recurringCharges = Core.Nothing,
                  reservedNodeId = Core.Nothing,
                  reservedNodeOfferingId = Core.Nothing,
                  reservedNodeOfferingType = Core.Nothing, startTime = Core.Nothing,
                  state = Core.Nothing, usagePrice = Core.Nothing}

-- | The currency code for the reserved cluster.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnCurrencyCode :: Lens.Lens' ReservedNode (Core.Maybe Core.Text)
rnCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE rnCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The duration of the node reservation in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnDuration :: Lens.Lens' ReservedNode (Core.Maybe Core.Int)
rnDuration = Lens.field @"duration"
{-# INLINEABLE rnDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The fixed cost Amazon Redshift charges you for this reserved node.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnFixedPrice :: Lens.Lens' ReservedNode (Core.Maybe Core.Double)
rnFixedPrice = Lens.field @"fixedPrice"
{-# INLINEABLE rnFixedPrice #-}
{-# DEPRECATED fixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead"  #-}

-- | The number of reserved compute nodes.
--
-- /Note:/ Consider using 'nodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeCount :: Lens.Lens' ReservedNode (Core.Maybe Core.Int)
rnNodeCount = Lens.field @"nodeCount"
{-# INLINEABLE rnNodeCount #-}
{-# DEPRECATED nodeCount "Use generic-lens or generic-optics with 'nodeCount' instead"  #-}

-- | The node type of the reserved node.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeType :: Lens.Lens' ReservedNode (Core.Maybe Core.Text)
rnNodeType = Lens.field @"nodeType"
{-# INLINEABLE rnNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnOfferingType :: Lens.Lens' ReservedNode (Core.Maybe Core.Text)
rnOfferingType = Lens.field @"offeringType"
{-# INLINEABLE rnOfferingType #-}
{-# DEPRECATED offeringType "Use generic-lens or generic-optics with 'offeringType' instead"  #-}

-- | The recurring charges for the reserved node.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnRecurringCharges :: Lens.Lens' ReservedNode (Core.Maybe [Types.RecurringCharge])
rnRecurringCharges = Lens.field @"recurringCharges"
{-# INLINEABLE rnRecurringCharges #-}
{-# DEPRECATED recurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead"  #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeId :: Lens.Lens' ReservedNode (Core.Maybe Core.Text)
rnReservedNodeId = Lens.field @"reservedNodeId"
{-# INLINEABLE rnReservedNodeId #-}
{-# DEPRECATED reservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead"  #-}

-- | The identifier for the reserved node offering.
--
-- /Note:/ Consider using 'reservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeOfferingId :: Lens.Lens' ReservedNode (Core.Maybe Core.Text)
rnReservedNodeOfferingId = Lens.field @"reservedNodeOfferingId"
{-# INLINEABLE rnReservedNodeOfferingId #-}
{-# DEPRECATED reservedNodeOfferingId "Use generic-lens or generic-optics with 'reservedNodeOfferingId' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'reservedNodeOfferingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeOfferingType :: Lens.Lens' ReservedNode (Core.Maybe Types.ReservedNodeOfferingType)
rnReservedNodeOfferingType = Lens.field @"reservedNodeOfferingType"
{-# INLINEABLE rnReservedNodeOfferingType #-}
{-# DEPRECATED reservedNodeOfferingType "Use generic-lens or generic-optics with 'reservedNodeOfferingType' instead"  #-}

-- | The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnStartTime :: Lens.Lens' ReservedNode (Core.Maybe Core.UTCTime)
rnStartTime = Lens.field @"startTime"
{-# INLINEABLE rnStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

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
rnState :: Lens.Lens' ReservedNode (Core.Maybe Core.Text)
rnState = Lens.field @"state"
{-# INLINEABLE rnState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The hourly rate Amazon Redshift charges you for this reserved node.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnUsagePrice :: Lens.Lens' ReservedNode (Core.Maybe Core.Double)
rnUsagePrice = Lens.field @"usagePrice"
{-# INLINEABLE rnUsagePrice #-}
{-# DEPRECATED usagePrice "Use generic-lens or generic-optics with 'usagePrice' instead"  #-}

instance Core.FromXML ReservedNode where
        parseXML x
          = ReservedNode' Core.<$>
              (x Core..@? "CurrencyCode") Core.<*> x Core..@? "Duration" Core.<*>
                x Core..@? "FixedPrice"
                Core.<*> x Core..@? "NodeCount"
                Core.<*> x Core..@? "NodeType"
                Core.<*> x Core..@? "OfferingType"
                Core.<*>
                x Core..@? "RecurringCharges" Core..<@>
                  Core.parseXMLList "RecurringCharge"
                Core.<*> x Core..@? "ReservedNodeId"
                Core.<*> x Core..@? "ReservedNodeOfferingId"
                Core.<*> x Core..@? "ReservedNodeOfferingType"
                Core.<*> x Core..@? "StartTime"
                Core.<*> x Core..@? "State"
                Core.<*> x Core..@? "UsagePrice"
