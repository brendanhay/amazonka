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
-- Module      : Amazonka.Snowball.Types.Shipment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.Shipment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The @Status@ and @TrackingNumber@ information for an inbound or outbound
-- shipment.
--
-- /See:/ 'newShipment' smart constructor.
data Shipment = Shipment'
  { -- | Status information for a shipment.
    status :: Prelude.Maybe Prelude.Text,
    -- | The tracking number for this job. Using this tracking number with your
    -- region\'s carrier\'s website, you can track a Snow device as the carrier
    -- transports it.
    --
    -- For India, the carrier is Amazon Logistics. For all other regions, UPS
    -- is the carrier.
    trackingNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Shipment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'shipment_status' - Status information for a shipment.
--
-- 'trackingNumber', 'shipment_trackingNumber' - The tracking number for this job. Using this tracking number with your
-- region\'s carrier\'s website, you can track a Snow device as the carrier
-- transports it.
--
-- For India, the carrier is Amazon Logistics. For all other regions, UPS
-- is the carrier.
newShipment ::
  Shipment
newShipment =
  Shipment'
    { status = Prelude.Nothing,
      trackingNumber = Prelude.Nothing
    }

-- | Status information for a shipment.
shipment_status :: Lens.Lens' Shipment (Prelude.Maybe Prelude.Text)
shipment_status = Lens.lens (\Shipment' {status} -> status) (\s@Shipment' {} a -> s {status = a} :: Shipment)

-- | The tracking number for this job. Using this tracking number with your
-- region\'s carrier\'s website, you can track a Snow device as the carrier
-- transports it.
--
-- For India, the carrier is Amazon Logistics. For all other regions, UPS
-- is the carrier.
shipment_trackingNumber :: Lens.Lens' Shipment (Prelude.Maybe Prelude.Text)
shipment_trackingNumber = Lens.lens (\Shipment' {trackingNumber} -> trackingNumber) (\s@Shipment' {} a -> s {trackingNumber = a} :: Shipment)

instance Core.FromJSON Shipment where
  parseJSON =
    Core.withObject
      "Shipment"
      ( \x ->
          Shipment'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "TrackingNumber")
      )

instance Prelude.Hashable Shipment where
  hashWithSalt _salt Shipment' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` trackingNumber

instance Prelude.NFData Shipment where
  rnf Shipment' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf trackingNumber
