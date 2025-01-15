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
-- Module      : Amazonka.MemoryDb.Types.ReservedNodesOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ReservedNodesOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.RecurringCharge
import qualified Amazonka.Prelude as Prelude

-- | The offering type of this node.
--
-- /See:/ 'newReservedNodesOffering' smart constructor.
data ReservedNodesOffering = ReservedNodesOffering'
  { -- | The duration of the reservation in seconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The fixed price charged for this reserved node.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The node type for the reserved nodes. For more information, see
    -- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The offering type of this reserved node.
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The recurring price charged to run this reserved node.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The offering identifier.
    reservedNodesOfferingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedNodesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'reservedNodesOffering_duration' - The duration of the reservation in seconds.
--
-- 'fixedPrice', 'reservedNodesOffering_fixedPrice' - The fixed price charged for this reserved node.
--
-- 'nodeType', 'reservedNodesOffering_nodeType' - The node type for the reserved nodes. For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
--
-- 'offeringType', 'reservedNodesOffering_offeringType' - The offering type of this reserved node.
--
-- 'recurringCharges', 'reservedNodesOffering_recurringCharges' - The recurring price charged to run this reserved node.
--
-- 'reservedNodesOfferingId', 'reservedNodesOffering_reservedNodesOfferingId' - The offering identifier.
newReservedNodesOffering ::
  ReservedNodesOffering
newReservedNodesOffering =
  ReservedNodesOffering'
    { duration = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      reservedNodesOfferingId = Prelude.Nothing
    }

-- | The duration of the reservation in seconds.
reservedNodesOffering_duration :: Lens.Lens' ReservedNodesOffering (Prelude.Maybe Prelude.Int)
reservedNodesOffering_duration = Lens.lens (\ReservedNodesOffering' {duration} -> duration) (\s@ReservedNodesOffering' {} a -> s {duration = a} :: ReservedNodesOffering)

-- | The fixed price charged for this reserved node.
reservedNodesOffering_fixedPrice :: Lens.Lens' ReservedNodesOffering (Prelude.Maybe Prelude.Double)
reservedNodesOffering_fixedPrice = Lens.lens (\ReservedNodesOffering' {fixedPrice} -> fixedPrice) (\s@ReservedNodesOffering' {} a -> s {fixedPrice = a} :: ReservedNodesOffering)

-- | The node type for the reserved nodes. For more information, see
-- <https://docs.aws.amazon.com/memorydb/latest/devguide/nodes.reserved.html#reserved-nodes-supported Supported node types>.
reservedNodesOffering_nodeType :: Lens.Lens' ReservedNodesOffering (Prelude.Maybe Prelude.Text)
reservedNodesOffering_nodeType = Lens.lens (\ReservedNodesOffering' {nodeType} -> nodeType) (\s@ReservedNodesOffering' {} a -> s {nodeType = a} :: ReservedNodesOffering)

-- | The offering type of this reserved node.
reservedNodesOffering_offeringType :: Lens.Lens' ReservedNodesOffering (Prelude.Maybe Prelude.Text)
reservedNodesOffering_offeringType = Lens.lens (\ReservedNodesOffering' {offeringType} -> offeringType) (\s@ReservedNodesOffering' {} a -> s {offeringType = a} :: ReservedNodesOffering)

-- | The recurring price charged to run this reserved node.
reservedNodesOffering_recurringCharges :: Lens.Lens' ReservedNodesOffering (Prelude.Maybe [RecurringCharge])
reservedNodesOffering_recurringCharges = Lens.lens (\ReservedNodesOffering' {recurringCharges} -> recurringCharges) (\s@ReservedNodesOffering' {} a -> s {recurringCharges = a} :: ReservedNodesOffering) Prelude.. Lens.mapping Lens.coerced

-- | The offering identifier.
reservedNodesOffering_reservedNodesOfferingId :: Lens.Lens' ReservedNodesOffering (Prelude.Maybe Prelude.Text)
reservedNodesOffering_reservedNodesOfferingId = Lens.lens (\ReservedNodesOffering' {reservedNodesOfferingId} -> reservedNodesOfferingId) (\s@ReservedNodesOffering' {} a -> s {reservedNodesOfferingId = a} :: ReservedNodesOffering)

instance Data.FromJSON ReservedNodesOffering where
  parseJSON =
    Data.withObject
      "ReservedNodesOffering"
      ( \x ->
          ReservedNodesOffering'
            Prelude.<$> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "FixedPrice")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "OfferingType")
            Prelude.<*> ( x
                            Data..:? "RecurringCharges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ReservedNodesOfferingId")
      )

instance Prelude.Hashable ReservedNodesOffering where
  hashWithSalt _salt ReservedNodesOffering' {..} =
    _salt
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` reservedNodesOfferingId

instance Prelude.NFData ReservedNodesOffering where
  rnf ReservedNodesOffering' {..} =
    Prelude.rnf duration `Prelude.seq`
      Prelude.rnf fixedPrice `Prelude.seq`
        Prelude.rnf nodeType `Prelude.seq`
          Prelude.rnf offeringType `Prelude.seq`
            Prelude.rnf recurringCharges `Prelude.seq`
              Prelude.rnf reservedNodesOfferingId
