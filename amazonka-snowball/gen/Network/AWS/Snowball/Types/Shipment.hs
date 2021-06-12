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
-- Module      : Network.AWS.Snowball.Types.Shipment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Shipment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The @Status@ and @TrackingNumber@ information for an inbound or outbound
-- shipment.
--
-- /See:/ 'newShipment' smart constructor.
data Shipment = Shipment'
  { -- | The tracking number for this job. Using this tracking number with your
    -- region\'s carrier\'s website, you can track a Snow device as the carrier
    -- transports it.
    --
    -- For India, the carrier is Amazon Logistics. For all other regions, UPS
    -- is the carrier.
    trackingNumber :: Core.Maybe Core.Text,
    -- | Status information for a shipment.
    status :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Shipment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackingNumber', 'shipment_trackingNumber' - The tracking number for this job. Using this tracking number with your
-- region\'s carrier\'s website, you can track a Snow device as the carrier
-- transports it.
--
-- For India, the carrier is Amazon Logistics. For all other regions, UPS
-- is the carrier.
--
-- 'status', 'shipment_status' - Status information for a shipment.
newShipment ::
  Shipment
newShipment =
  Shipment'
    { trackingNumber = Core.Nothing,
      status = Core.Nothing
    }

-- | The tracking number for this job. Using this tracking number with your
-- region\'s carrier\'s website, you can track a Snow device as the carrier
-- transports it.
--
-- For India, the carrier is Amazon Logistics. For all other regions, UPS
-- is the carrier.
shipment_trackingNumber :: Lens.Lens' Shipment (Core.Maybe Core.Text)
shipment_trackingNumber = Lens.lens (\Shipment' {trackingNumber} -> trackingNumber) (\s@Shipment' {} a -> s {trackingNumber = a} :: Shipment)

-- | Status information for a shipment.
shipment_status :: Lens.Lens' Shipment (Core.Maybe Core.Text)
shipment_status = Lens.lens (\Shipment' {status} -> status) (\s@Shipment' {} a -> s {status = a} :: Shipment)

instance Core.FromJSON Shipment where
  parseJSON =
    Core.withObject
      "Shipment"
      ( \x ->
          Shipment'
            Core.<$> (x Core..:? "TrackingNumber")
            Core.<*> (x Core..:? "Status")
      )

instance Core.Hashable Shipment

instance Core.NFData Shipment
