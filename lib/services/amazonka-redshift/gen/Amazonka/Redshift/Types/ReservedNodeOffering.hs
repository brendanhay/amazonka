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
-- Module      : Amazonka.Redshift.Types.ReservedNodeOffering
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ReservedNodeOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.RecurringCharge
import Amazonka.Redshift.Types.ReservedNodeOfferingType

-- | Describes a reserved node offering.
--
-- /See:/ 'newReservedNodeOffering' smart constructor.
data ReservedNodeOffering = ReservedNodeOffering'
  { -- | The charge to your account regardless of whether you are creating any
    -- clusters using the node offering. Recurring charges are only in effect
    -- for heavy-utilization reserved nodes.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The anticipated utilization of the reserved node, as defined in the
    -- reserved node offering.
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The node type offered by the reserved node offering.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The duration, in seconds, for which the offering will reserve the node.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The currency code for the compute nodes offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    reservedNodeOfferingType :: Prelude.Maybe ReservedNodeOfferingType,
    -- | The offering identifier.
    reservedNodeOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The upfront fixed charge you will pay to purchase the specific reserved
    -- node offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The rate you are charged for each hour the cluster that is using the
    -- offering is running.
    usagePrice :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedNodeOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recurringCharges', 'reservedNodeOffering_recurringCharges' - The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect
-- for heavy-utilization reserved nodes.
--
-- 'offeringType', 'reservedNodeOffering_offeringType' - The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
--
-- 'nodeType', 'reservedNodeOffering_nodeType' - The node type offered by the reserved node offering.
--
-- 'duration', 'reservedNodeOffering_duration' - The duration, in seconds, for which the offering will reserve the node.
--
-- 'currencyCode', 'reservedNodeOffering_currencyCode' - The currency code for the compute nodes offering.
--
-- 'reservedNodeOfferingType', 'reservedNodeOffering_reservedNodeOfferingType' -
--
-- 'reservedNodeOfferingId', 'reservedNodeOffering_reservedNodeOfferingId' - The offering identifier.
--
-- 'fixedPrice', 'reservedNodeOffering_fixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
--
-- 'usagePrice', 'reservedNodeOffering_usagePrice' - The rate you are charged for each hour the cluster that is using the
-- offering is running.
newReservedNodeOffering ::
  ReservedNodeOffering
newReservedNodeOffering =
  ReservedNodeOffering'
    { recurringCharges =
        Prelude.Nothing,
      offeringType = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      duration = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      reservedNodeOfferingType = Prelude.Nothing,
      reservedNodeOfferingId = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | The charge to your account regardless of whether you are creating any
-- clusters using the node offering. Recurring charges are only in effect
-- for heavy-utilization reserved nodes.
reservedNodeOffering_recurringCharges :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe [RecurringCharge])
reservedNodeOffering_recurringCharges = Lens.lens (\ReservedNodeOffering' {recurringCharges} -> recurringCharges) (\s@ReservedNodeOffering' {} a -> s {recurringCharges = a} :: ReservedNodeOffering) Prelude.. Lens.mapping Lens.coerced

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
reservedNodeOffering_offeringType :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe Prelude.Text)
reservedNodeOffering_offeringType = Lens.lens (\ReservedNodeOffering' {offeringType} -> offeringType) (\s@ReservedNodeOffering' {} a -> s {offeringType = a} :: ReservedNodeOffering)

-- | The node type offered by the reserved node offering.
reservedNodeOffering_nodeType :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe Prelude.Text)
reservedNodeOffering_nodeType = Lens.lens (\ReservedNodeOffering' {nodeType} -> nodeType) (\s@ReservedNodeOffering' {} a -> s {nodeType = a} :: ReservedNodeOffering)

-- | The duration, in seconds, for which the offering will reserve the node.
reservedNodeOffering_duration :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe Prelude.Int)
reservedNodeOffering_duration = Lens.lens (\ReservedNodeOffering' {duration} -> duration) (\s@ReservedNodeOffering' {} a -> s {duration = a} :: ReservedNodeOffering)

-- | The currency code for the compute nodes offering.
reservedNodeOffering_currencyCode :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe Prelude.Text)
reservedNodeOffering_currencyCode = Lens.lens (\ReservedNodeOffering' {currencyCode} -> currencyCode) (\s@ReservedNodeOffering' {} a -> s {currencyCode = a} :: ReservedNodeOffering)

-- |
reservedNodeOffering_reservedNodeOfferingType :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe ReservedNodeOfferingType)
reservedNodeOffering_reservedNodeOfferingType = Lens.lens (\ReservedNodeOffering' {reservedNodeOfferingType} -> reservedNodeOfferingType) (\s@ReservedNodeOffering' {} a -> s {reservedNodeOfferingType = a} :: ReservedNodeOffering)

-- | The offering identifier.
reservedNodeOffering_reservedNodeOfferingId :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe Prelude.Text)
reservedNodeOffering_reservedNodeOfferingId = Lens.lens (\ReservedNodeOffering' {reservedNodeOfferingId} -> reservedNodeOfferingId) (\s@ReservedNodeOffering' {} a -> s {reservedNodeOfferingId = a} :: ReservedNodeOffering)

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- node offering.
reservedNodeOffering_fixedPrice :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe Prelude.Double)
reservedNodeOffering_fixedPrice = Lens.lens (\ReservedNodeOffering' {fixedPrice} -> fixedPrice) (\s@ReservedNodeOffering' {} a -> s {fixedPrice = a} :: ReservedNodeOffering)

-- | The rate you are charged for each hour the cluster that is using the
-- offering is running.
reservedNodeOffering_usagePrice :: Lens.Lens' ReservedNodeOffering (Prelude.Maybe Prelude.Double)
reservedNodeOffering_usagePrice = Lens.lens (\ReservedNodeOffering' {usagePrice} -> usagePrice) (\s@ReservedNodeOffering' {} a -> s {usagePrice = a} :: ReservedNodeOffering)

instance Data.FromXML ReservedNodeOffering where
  parseXML x =
    ReservedNodeOffering'
      Prelude.<$> ( x Data..@? "RecurringCharges"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "RecurringCharge")
                  )
      Prelude.<*> (x Data..@? "OfferingType")
      Prelude.<*> (x Data..@? "NodeType")
      Prelude.<*> (x Data..@? "Duration")
      Prelude.<*> (x Data..@? "CurrencyCode")
      Prelude.<*> (x Data..@? "ReservedNodeOfferingType")
      Prelude.<*> (x Data..@? "ReservedNodeOfferingId")
      Prelude.<*> (x Data..@? "FixedPrice")
      Prelude.<*> (x Data..@? "UsagePrice")

instance Prelude.Hashable ReservedNodeOffering where
  hashWithSalt _salt ReservedNodeOffering' {..} =
    _salt `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` reservedNodeOfferingType
      `Prelude.hashWithSalt` reservedNodeOfferingId
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` usagePrice

instance Prelude.NFData ReservedNodeOffering where
  rnf ReservedNodeOffering' {..} =
    Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf reservedNodeOfferingType
      `Prelude.seq` Prelude.rnf reservedNodeOfferingId
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf usagePrice
