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
-- Module      : Network.AWS.Redshift.Types.ReservedNode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ReservedNode where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNodeOfferingType

-- | Describes a reserved node. You can call the
-- DescribeReservedNodeOfferings API to obtain the available reserved node
-- offerings.
--
-- /See:/ 'newReservedNode' smart constructor.
data ReservedNode = ReservedNode'
  { reservedNodeOfferingType :: Prelude.Maybe ReservedNodeOfferingType,
    -- | The unique identifier for the reservation.
    reservedNodeId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the reserved node offering.
    reservedNodeOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The duration of the node reservation in seconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The time the reservation started. You purchase a reserved node offering
    -- for a duration. This is the start time of that duration.
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The currency code for the reserved cluster.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The state of the reserved compute node.
    --
    -- Possible Values:
    --
    -- -   pending-payment-This reserved node has recently been purchased, and
    --     the sale has been approved, but payment has not yet been confirmed.
    --
    -- -   active-This reserved node is owned by the caller and is available
    --     for use.
    --
    -- -   payment-failed-Payment failed for the purchase attempt.
    --
    -- -   retired-The reserved node is no longer available.
    --
    -- -   exchanging-The owner is exchanging the reserved node for another
    --     reserved node.
    state :: Prelude.Maybe Prelude.Text,
    -- | The fixed cost Amazon Redshift charges you for this reserved node.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The number of reserved compute nodes.
    nodeCount :: Prelude.Maybe Prelude.Int,
    -- | The hourly rate Amazon Redshift charges you for this reserved node.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The anticipated utilization of the reserved node, as defined in the
    -- reserved node offering.
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The node type of the reserved node.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The recurring charges for the reserved node.
    recurringCharges :: Prelude.Maybe [RecurringCharge]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservedNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodeOfferingType', 'reservedNode_reservedNodeOfferingType' -
--
-- 'reservedNodeId', 'reservedNode_reservedNodeId' - The unique identifier for the reservation.
--
-- 'reservedNodeOfferingId', 'reservedNode_reservedNodeOfferingId' - The identifier for the reserved node offering.
--
-- 'duration', 'reservedNode_duration' - The duration of the node reservation in seconds.
--
-- 'startTime', 'reservedNode_startTime' - The time the reservation started. You purchase a reserved node offering
-- for a duration. This is the start time of that duration.
--
-- 'currencyCode', 'reservedNode_currencyCode' - The currency code for the reserved cluster.
--
-- 'state', 'reservedNode_state' - The state of the reserved compute node.
--
-- Possible Values:
--
-- -   pending-payment-This reserved node has recently been purchased, and
--     the sale has been approved, but payment has not yet been confirmed.
--
-- -   active-This reserved node is owned by the caller and is available
--     for use.
--
-- -   payment-failed-Payment failed for the purchase attempt.
--
-- -   retired-The reserved node is no longer available.
--
-- -   exchanging-The owner is exchanging the reserved node for another
--     reserved node.
--
-- 'fixedPrice', 'reservedNode_fixedPrice' - The fixed cost Amazon Redshift charges you for this reserved node.
--
-- 'nodeCount', 'reservedNode_nodeCount' - The number of reserved compute nodes.
--
-- 'usagePrice', 'reservedNode_usagePrice' - The hourly rate Amazon Redshift charges you for this reserved node.
--
-- 'offeringType', 'reservedNode_offeringType' - The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
--
-- 'nodeType', 'reservedNode_nodeType' - The node type of the reserved node.
--
-- 'recurringCharges', 'reservedNode_recurringCharges' - The recurring charges for the reserved node.
newReservedNode ::
  ReservedNode
newReservedNode =
  ReservedNode'
    { reservedNodeOfferingType =
        Prelude.Nothing,
      reservedNodeId = Prelude.Nothing,
      reservedNodeOfferingId = Prelude.Nothing,
      duration = Prelude.Nothing,
      startTime = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      state = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      nodeCount = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      recurringCharges = Prelude.Nothing
    }

-- |
reservedNode_reservedNodeOfferingType :: Lens.Lens' ReservedNode (Prelude.Maybe ReservedNodeOfferingType)
reservedNode_reservedNodeOfferingType = Lens.lens (\ReservedNode' {reservedNodeOfferingType} -> reservedNodeOfferingType) (\s@ReservedNode' {} a -> s {reservedNodeOfferingType = a} :: ReservedNode)

-- | The unique identifier for the reservation.
reservedNode_reservedNodeId :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_reservedNodeId = Lens.lens (\ReservedNode' {reservedNodeId} -> reservedNodeId) (\s@ReservedNode' {} a -> s {reservedNodeId = a} :: ReservedNode)

-- | The identifier for the reserved node offering.
reservedNode_reservedNodeOfferingId :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_reservedNodeOfferingId = Lens.lens (\ReservedNode' {reservedNodeOfferingId} -> reservedNodeOfferingId) (\s@ReservedNode' {} a -> s {reservedNodeOfferingId = a} :: ReservedNode)

-- | The duration of the node reservation in seconds.
reservedNode_duration :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Int)
reservedNode_duration = Lens.lens (\ReservedNode' {duration} -> duration) (\s@ReservedNode' {} a -> s {duration = a} :: ReservedNode)

-- | The time the reservation started. You purchase a reserved node offering
-- for a duration. This is the start time of that duration.
reservedNode_startTime :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.UTCTime)
reservedNode_startTime = Lens.lens (\ReservedNode' {startTime} -> startTime) (\s@ReservedNode' {} a -> s {startTime = a} :: ReservedNode) Prelude.. Lens.mapping Prelude._Time

-- | The currency code for the reserved cluster.
reservedNode_currencyCode :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_currencyCode = Lens.lens (\ReservedNode' {currencyCode} -> currencyCode) (\s@ReservedNode' {} a -> s {currencyCode = a} :: ReservedNode)

-- | The state of the reserved compute node.
--
-- Possible Values:
--
-- -   pending-payment-This reserved node has recently been purchased, and
--     the sale has been approved, but payment has not yet been confirmed.
--
-- -   active-This reserved node is owned by the caller and is available
--     for use.
--
-- -   payment-failed-Payment failed for the purchase attempt.
--
-- -   retired-The reserved node is no longer available.
--
-- -   exchanging-The owner is exchanging the reserved node for another
--     reserved node.
reservedNode_state :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_state = Lens.lens (\ReservedNode' {state} -> state) (\s@ReservedNode' {} a -> s {state = a} :: ReservedNode)

-- | The fixed cost Amazon Redshift charges you for this reserved node.
reservedNode_fixedPrice :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Double)
reservedNode_fixedPrice = Lens.lens (\ReservedNode' {fixedPrice} -> fixedPrice) (\s@ReservedNode' {} a -> s {fixedPrice = a} :: ReservedNode)

-- | The number of reserved compute nodes.
reservedNode_nodeCount :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Int)
reservedNode_nodeCount = Lens.lens (\ReservedNode' {nodeCount} -> nodeCount) (\s@ReservedNode' {} a -> s {nodeCount = a} :: ReservedNode)

-- | The hourly rate Amazon Redshift charges you for this reserved node.
reservedNode_usagePrice :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Double)
reservedNode_usagePrice = Lens.lens (\ReservedNode' {usagePrice} -> usagePrice) (\s@ReservedNode' {} a -> s {usagePrice = a} :: ReservedNode)

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
reservedNode_offeringType :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_offeringType = Lens.lens (\ReservedNode' {offeringType} -> offeringType) (\s@ReservedNode' {} a -> s {offeringType = a} :: ReservedNode)

-- | The node type of the reserved node.
reservedNode_nodeType :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_nodeType = Lens.lens (\ReservedNode' {nodeType} -> nodeType) (\s@ReservedNode' {} a -> s {nodeType = a} :: ReservedNode)

-- | The recurring charges for the reserved node.
reservedNode_recurringCharges :: Lens.Lens' ReservedNode (Prelude.Maybe [RecurringCharge])
reservedNode_recurringCharges = Lens.lens (\ReservedNode' {recurringCharges} -> recurringCharges) (\s@ReservedNode' {} a -> s {recurringCharges = a} :: ReservedNode) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML ReservedNode where
  parseXML x =
    ReservedNode'
      Prelude.<$> (x Prelude..@? "ReservedNodeOfferingType")
      Prelude.<*> (x Prelude..@? "ReservedNodeId")
      Prelude.<*> (x Prelude..@? "ReservedNodeOfferingId")
      Prelude.<*> (x Prelude..@? "Duration")
      Prelude.<*> (x Prelude..@? "StartTime")
      Prelude.<*> (x Prelude..@? "CurrencyCode")
      Prelude.<*> (x Prelude..@? "State")
      Prelude.<*> (x Prelude..@? "FixedPrice")
      Prelude.<*> (x Prelude..@? "NodeCount")
      Prelude.<*> (x Prelude..@? "UsagePrice")
      Prelude.<*> (x Prelude..@? "OfferingType")
      Prelude.<*> (x Prelude..@? "NodeType")
      Prelude.<*> ( x Prelude..@? "RecurringCharges"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "RecurringCharge")
                  )

instance Prelude.Hashable ReservedNode

instance Prelude.NFData ReservedNode
