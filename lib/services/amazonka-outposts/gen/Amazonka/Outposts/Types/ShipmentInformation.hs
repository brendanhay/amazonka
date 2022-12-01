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
-- Module      : Amazonka.Outposts.Types.ShipmentInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.ShipmentInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types.ShipmentCarrier
import qualified Amazonka.Prelude as Prelude

-- | Information about a line item shipment.
--
-- /See:/ 'newShipmentInformation' smart constructor.
data ShipmentInformation = ShipmentInformation'
  { -- | The tracking number of the shipment.
    shipmentTrackingNumber :: Prelude.Maybe Prelude.Text,
    -- | The carrier of the shipment.
    shipmentCarrier :: Prelude.Maybe ShipmentCarrier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShipmentInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shipmentTrackingNumber', 'shipmentInformation_shipmentTrackingNumber' - The tracking number of the shipment.
--
-- 'shipmentCarrier', 'shipmentInformation_shipmentCarrier' - The carrier of the shipment.
newShipmentInformation ::
  ShipmentInformation
newShipmentInformation =
  ShipmentInformation'
    { shipmentTrackingNumber =
        Prelude.Nothing,
      shipmentCarrier = Prelude.Nothing
    }

-- | The tracking number of the shipment.
shipmentInformation_shipmentTrackingNumber :: Lens.Lens' ShipmentInformation (Prelude.Maybe Prelude.Text)
shipmentInformation_shipmentTrackingNumber = Lens.lens (\ShipmentInformation' {shipmentTrackingNumber} -> shipmentTrackingNumber) (\s@ShipmentInformation' {} a -> s {shipmentTrackingNumber = a} :: ShipmentInformation)

-- | The carrier of the shipment.
shipmentInformation_shipmentCarrier :: Lens.Lens' ShipmentInformation (Prelude.Maybe ShipmentCarrier)
shipmentInformation_shipmentCarrier = Lens.lens (\ShipmentInformation' {shipmentCarrier} -> shipmentCarrier) (\s@ShipmentInformation' {} a -> s {shipmentCarrier = a} :: ShipmentInformation)

instance Core.FromJSON ShipmentInformation where
  parseJSON =
    Core.withObject
      "ShipmentInformation"
      ( \x ->
          ShipmentInformation'
            Prelude.<$> (x Core..:? "ShipmentTrackingNumber")
            Prelude.<*> (x Core..:? "ShipmentCarrier")
      )

instance Prelude.Hashable ShipmentInformation where
  hashWithSalt _salt ShipmentInformation' {..} =
    _salt `Prelude.hashWithSalt` shipmentTrackingNumber
      `Prelude.hashWithSalt` shipmentCarrier

instance Prelude.NFData ShipmentInformation where
  rnf ShipmentInformation' {..} =
    Prelude.rnf shipmentTrackingNumber
      `Prelude.seq` Prelude.rnf shipmentCarrier
