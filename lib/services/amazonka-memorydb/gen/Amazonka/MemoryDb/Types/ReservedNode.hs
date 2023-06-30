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
-- Module      : Amazonka.MemoryDb.Types.ReservedNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ReservedNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.RecurringCharge
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a @PurchaseReservedNodesOffering@ operation.
--
-- /See:/ 'newReservedNode' smart constructor.
data ReservedNode = ReservedNode'
  { -- | The Amazon Resource Name (ARN) of the reserved node.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The duration of the reservation in seconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The fixed price charged for this reserved node.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The number of nodes that have been reserved.
    nodeCount :: Prelude.Maybe Prelude.Int,
    -- | The node type for the reserved nodes.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The offering type of this reserved node.
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The recurring price charged to run this reserved node.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | A customer-specified identifier to track this reservation.
    reservationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the reserved node offering to purchase.
    reservedNodesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The time the reservation started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the reserved node.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'reservedNode_arn' - The Amazon Resource Name (ARN) of the reserved node.
--
-- 'duration', 'reservedNode_duration' - The duration of the reservation in seconds.
--
-- 'fixedPrice', 'reservedNode_fixedPrice' - The fixed price charged for this reserved node.
--
-- 'nodeCount', 'reservedNode_nodeCount' - The number of nodes that have been reserved.
--
-- 'nodeType', 'reservedNode_nodeType' - The node type for the reserved nodes.
--
-- 'offeringType', 'reservedNode_offeringType' - The offering type of this reserved node.
--
-- 'recurringCharges', 'reservedNode_recurringCharges' - The recurring price charged to run this reserved node.
--
-- 'reservationId', 'reservedNode_reservationId' - A customer-specified identifier to track this reservation.
--
-- 'reservedNodesOfferingId', 'reservedNode_reservedNodesOfferingId' - The ID of the reserved node offering to purchase.
--
-- 'startTime', 'reservedNode_startTime' - The time the reservation started.
--
-- 'state', 'reservedNode_state' - The state of the reserved node.
newReservedNode ::
  ReservedNode
newReservedNode =
  ReservedNode'
    { arn = Prelude.Nothing,
      duration = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      nodeCount = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      reservationId = Prelude.Nothing,
      reservedNodesOfferingId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the reserved node.
reservedNode_arn :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_arn = Lens.lens (\ReservedNode' {arn} -> arn) (\s@ReservedNode' {} a -> s {arn = a} :: ReservedNode)

-- | The duration of the reservation in seconds.
reservedNode_duration :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Int)
reservedNode_duration = Lens.lens (\ReservedNode' {duration} -> duration) (\s@ReservedNode' {} a -> s {duration = a} :: ReservedNode)

-- | The fixed price charged for this reserved node.
reservedNode_fixedPrice :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Double)
reservedNode_fixedPrice = Lens.lens (\ReservedNode' {fixedPrice} -> fixedPrice) (\s@ReservedNode' {} a -> s {fixedPrice = a} :: ReservedNode)

-- | The number of nodes that have been reserved.
reservedNode_nodeCount :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Int)
reservedNode_nodeCount = Lens.lens (\ReservedNode' {nodeCount} -> nodeCount) (\s@ReservedNode' {} a -> s {nodeCount = a} :: ReservedNode)

-- | The node type for the reserved nodes.
reservedNode_nodeType :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_nodeType = Lens.lens (\ReservedNode' {nodeType} -> nodeType) (\s@ReservedNode' {} a -> s {nodeType = a} :: ReservedNode)

-- | The offering type of this reserved node.
reservedNode_offeringType :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_offeringType = Lens.lens (\ReservedNode' {offeringType} -> offeringType) (\s@ReservedNode' {} a -> s {offeringType = a} :: ReservedNode)

-- | The recurring price charged to run this reserved node.
reservedNode_recurringCharges :: Lens.Lens' ReservedNode (Prelude.Maybe [RecurringCharge])
reservedNode_recurringCharges = Lens.lens (\ReservedNode' {recurringCharges} -> recurringCharges) (\s@ReservedNode' {} a -> s {recurringCharges = a} :: ReservedNode) Prelude.. Lens.mapping Lens.coerced

-- | A customer-specified identifier to track this reservation.
reservedNode_reservationId :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_reservationId = Lens.lens (\ReservedNode' {reservationId} -> reservationId) (\s@ReservedNode' {} a -> s {reservationId = a} :: ReservedNode)

-- | The ID of the reserved node offering to purchase.
reservedNode_reservedNodesOfferingId :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_reservedNodesOfferingId = Lens.lens (\ReservedNode' {reservedNodesOfferingId} -> reservedNodesOfferingId) (\s@ReservedNode' {} a -> s {reservedNodesOfferingId = a} :: ReservedNode)

-- | The time the reservation started.
reservedNode_startTime :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.UTCTime)
reservedNode_startTime = Lens.lens (\ReservedNode' {startTime} -> startTime) (\s@ReservedNode' {} a -> s {startTime = a} :: ReservedNode) Prelude.. Lens.mapping Data._Time

-- | The state of the reserved node.
reservedNode_state :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_state = Lens.lens (\ReservedNode' {state} -> state) (\s@ReservedNode' {} a -> s {state = a} :: ReservedNode)

instance Data.FromJSON ReservedNode where
  parseJSON =
    Data.withObject
      "ReservedNode"
      ( \x ->
          ReservedNode'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "FixedPrice")
            Prelude.<*> (x Data..:? "NodeCount")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "OfferingType")
            Prelude.<*> ( x
                            Data..:? "RecurringCharges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ReservationId")
            Prelude.<*> (x Data..:? "ReservedNodesOfferingId")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable ReservedNode where
  hashWithSalt _salt ReservedNode' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` nodeCount
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` reservationId
      `Prelude.hashWithSalt` reservedNodesOfferingId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state

instance Prelude.NFData ReservedNode where
  rnf ReservedNode' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf nodeCount
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf reservationId
      `Prelude.seq` Prelude.rnf reservedNodesOfferingId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
