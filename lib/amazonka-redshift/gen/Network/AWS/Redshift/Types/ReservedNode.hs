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
    rnReservedNodeOfferingType,
    rnState,
    rnCurrencyCode,
    rnStartTime,
    rnNodeCount,
    rnReservedNodeId,
    rnReservedNodeOfferingId,
    rnRecurringCharges,
    rnOfferingType,
    rnUsagePrice,
    rnNodeType,
    rnFixedPrice,
    rnDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNodeOfferingType

-- | Describes a reserved node. You can call the 'DescribeReservedNodeOfferings' API to obtain the available reserved node offerings.
--
-- /See:/ 'mkReservedNode' smart constructor.
data ReservedNode = ReservedNode'
  { reservedNodeOfferingType ::
      Lude.Maybe ReservedNodeOfferingType,
    state :: Lude.Maybe Lude.Text,
    currencyCode :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.DateTime,
    nodeCount :: Lude.Maybe Lude.Int,
    reservedNodeId :: Lude.Maybe Lude.Text,
    reservedNodeOfferingId :: Lude.Maybe Lude.Text,
    recurringCharges :: Lude.Maybe [RecurringCharge],
    offeringType :: Lude.Maybe Lude.Text,
    usagePrice :: Lude.Maybe Lude.Double,
    nodeType :: Lude.Maybe Lude.Text,
    fixedPrice :: Lude.Maybe Lude.Double,
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedNode' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency code for the reserved cluster.
-- * 'duration' - The duration of the node reservation in seconds.
-- * 'fixedPrice' - The fixed cost Amazon Redshift charges you for this reserved node.
-- * 'nodeCount' - The number of reserved compute nodes.
-- * 'nodeType' - The node type of the reserved node.
-- * 'offeringType' - The anticipated utilization of the reserved node, as defined in the reserved node offering.
-- * 'recurringCharges' - The recurring charges for the reserved node.
-- * 'reservedNodeId' - The unique identifier for the reservation.
-- * 'reservedNodeOfferingId' - The identifier for the reserved node offering.
-- * 'reservedNodeOfferingType' -
-- * 'startTime' - The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
-- * 'state' - The state of the reserved compute node.
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
-- * 'usagePrice' - The hourly rate Amazon Redshift charges you for this reserved node.
mkReservedNode ::
  ReservedNode
mkReservedNode =
  ReservedNode'
    { reservedNodeOfferingType = Lude.Nothing,
      state = Lude.Nothing,
      currencyCode = Lude.Nothing,
      startTime = Lude.Nothing,
      nodeCount = Lude.Nothing,
      reservedNodeId = Lude.Nothing,
      reservedNodeOfferingId = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      nodeType = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      duration = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'reservedNodeOfferingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeOfferingType :: Lens.Lens' ReservedNode (Lude.Maybe ReservedNodeOfferingType)
rnReservedNodeOfferingType = Lens.lens (reservedNodeOfferingType :: ReservedNode -> Lude.Maybe ReservedNodeOfferingType) (\s a -> s {reservedNodeOfferingType = a} :: ReservedNode)
{-# DEPRECATED rnReservedNodeOfferingType "Use generic-lens or generic-optics with 'reservedNodeOfferingType' instead." #-}

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
rnState :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Text)
rnState = Lens.lens (state :: ReservedNode -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ReservedNode)
{-# DEPRECATED rnState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The currency code for the reserved cluster.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnCurrencyCode :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Text)
rnCurrencyCode = Lens.lens (currencyCode :: ReservedNode -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: ReservedNode)
{-# DEPRECATED rnCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnStartTime :: Lens.Lens' ReservedNode (Lude.Maybe Lude.DateTime)
rnStartTime = Lens.lens (startTime :: ReservedNode -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: ReservedNode)
{-# DEPRECATED rnStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The number of reserved compute nodes.
--
-- /Note:/ Consider using 'nodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeCount :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Int)
rnNodeCount = Lens.lens (nodeCount :: ReservedNode -> Lude.Maybe Lude.Int) (\s a -> s {nodeCount = a} :: ReservedNode)
{-# DEPRECATED rnNodeCount "Use generic-lens or generic-optics with 'nodeCount' instead." #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeId :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Text)
rnReservedNodeId = Lens.lens (reservedNodeId :: ReservedNode -> Lude.Maybe Lude.Text) (\s a -> s {reservedNodeId = a} :: ReservedNode)
{-# DEPRECATED rnReservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead." #-}

-- | The identifier for the reserved node offering.
--
-- /Note:/ Consider using 'reservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnReservedNodeOfferingId :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Text)
rnReservedNodeOfferingId = Lens.lens (reservedNodeOfferingId :: ReservedNode -> Lude.Maybe Lude.Text) (\s a -> s {reservedNodeOfferingId = a} :: ReservedNode)
{-# DEPRECATED rnReservedNodeOfferingId "Use generic-lens or generic-optics with 'reservedNodeOfferingId' instead." #-}

-- | The recurring charges for the reserved node.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnRecurringCharges :: Lens.Lens' ReservedNode (Lude.Maybe [RecurringCharge])
rnRecurringCharges = Lens.lens (recurringCharges :: ReservedNode -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedNode)
{-# DEPRECATED rnRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnOfferingType :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Text)
rnOfferingType = Lens.lens (offeringType :: ReservedNode -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: ReservedNode)
{-# DEPRECATED rnOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The hourly rate Amazon Redshift charges you for this reserved node.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnUsagePrice :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Double)
rnUsagePrice = Lens.lens (usagePrice :: ReservedNode -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedNode)
{-# DEPRECATED rnUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The node type of the reserved node.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeType :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Text)
rnNodeType = Lens.lens (nodeType :: ReservedNode -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: ReservedNode)
{-# DEPRECATED rnNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The fixed cost Amazon Redshift charges you for this reserved node.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnFixedPrice :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Double)
rnFixedPrice = Lens.lens (fixedPrice :: ReservedNode -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedNode)
{-# DEPRECATED rnFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The duration of the node reservation in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnDuration :: Lens.Lens' ReservedNode (Lude.Maybe Lude.Int)
rnDuration = Lens.lens (duration :: ReservedNode -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ReservedNode)
{-# DEPRECATED rnDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromXML ReservedNode where
  parseXML x =
    ReservedNode'
      Lude.<$> (x Lude..@? "ReservedNodeOfferingType")
      Lude.<*> (x Lude..@? "State")
      Lude.<*> (x Lude..@? "CurrencyCode")
      Lude.<*> (x Lude..@? "StartTime")
      Lude.<*> (x Lude..@? "NodeCount")
      Lude.<*> (x Lude..@? "ReservedNodeId")
      Lude.<*> (x Lude..@? "ReservedNodeOfferingId")
      Lude.<*> ( x Lude..@? "RecurringCharges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "RecurringCharge")
               )
      Lude.<*> (x Lude..@? "OfferingType")
      Lude.<*> (x Lude..@? "UsagePrice")
      Lude.<*> (x Lude..@? "NodeType")
      Lude.<*> (x Lude..@? "FixedPrice")
      Lude.<*> (x Lude..@? "Duration")
