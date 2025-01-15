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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.ShipmentInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types.ShipmentCarrier
import qualified Amazonka.Prelude as Prelude

-- | Information about a line item shipment.
--
-- /See:/ 'newShipmentInformation' smart constructor.
data ShipmentInformation = ShipmentInformation'
  { -- | The carrier of the shipment.
    shipmentCarrier :: Prelude.Maybe ShipmentCarrier,
    -- | The tracking number of the shipment.
    shipmentTrackingNumber :: Prelude.Maybe Prelude.Text
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
-- 'shipmentCarrier', 'shipmentInformation_shipmentCarrier' - The carrier of the shipment.
--
-- 'shipmentTrackingNumber', 'shipmentInformation_shipmentTrackingNumber' - The tracking number of the shipment.
newShipmentInformation ::
  ShipmentInformation
newShipmentInformation =
  ShipmentInformation'
    { shipmentCarrier =
        Prelude.Nothing,
      shipmentTrackingNumber = Prelude.Nothing
    }

-- | The carrier of the shipment.
shipmentInformation_shipmentCarrier :: Lens.Lens' ShipmentInformation (Prelude.Maybe ShipmentCarrier)
shipmentInformation_shipmentCarrier = Lens.lens (\ShipmentInformation' {shipmentCarrier} -> shipmentCarrier) (\s@ShipmentInformation' {} a -> s {shipmentCarrier = a} :: ShipmentInformation)

-- | The tracking number of the shipment.
shipmentInformation_shipmentTrackingNumber :: Lens.Lens' ShipmentInformation (Prelude.Maybe Prelude.Text)
shipmentInformation_shipmentTrackingNumber = Lens.lens (\ShipmentInformation' {shipmentTrackingNumber} -> shipmentTrackingNumber) (\s@ShipmentInformation' {} a -> s {shipmentTrackingNumber = a} :: ShipmentInformation)

instance Data.FromJSON ShipmentInformation where
  parseJSON =
    Data.withObject
      "ShipmentInformation"
      ( \x ->
          ShipmentInformation'
            Prelude.<$> (x Data..:? "ShipmentCarrier")
            Prelude.<*> (x Data..:? "ShipmentTrackingNumber")
      )

instance Prelude.Hashable ShipmentInformation where
  hashWithSalt _salt ShipmentInformation' {..} =
    _salt
      `Prelude.hashWithSalt` shipmentCarrier
      `Prelude.hashWithSalt` shipmentTrackingNumber

instance Prelude.NFData ShipmentInformation where
  rnf ShipmentInformation' {..} =
    Prelude.rnf shipmentCarrier `Prelude.seq`
      Prelude.rnf shipmentTrackingNumber
