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
-- Module      : Network.AWS.Snowball.Types.Shipment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Shipment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    trackingNumber :: Prelude.Maybe Prelude.Text,
    -- | Status information for a shipment.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { trackingNumber = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The tracking number for this job. Using this tracking number with your
-- region\'s carrier\'s website, you can track a Snow device as the carrier
-- transports it.
--
-- For India, the carrier is Amazon Logistics. For all other regions, UPS
-- is the carrier.
shipment_trackingNumber :: Lens.Lens' Shipment (Prelude.Maybe Prelude.Text)
shipment_trackingNumber = Lens.lens (\Shipment' {trackingNumber} -> trackingNumber) (\s@Shipment' {} a -> s {trackingNumber = a} :: Shipment)

-- | Status information for a shipment.
shipment_status :: Lens.Lens' Shipment (Prelude.Maybe Prelude.Text)
shipment_status = Lens.lens (\Shipment' {status} -> status) (\s@Shipment' {} a -> s {status = a} :: Shipment)

instance Prelude.FromJSON Shipment where
  parseJSON =
    Prelude.withObject
      "Shipment"
      ( \x ->
          Shipment'
            Prelude.<$> (x Prelude..:? "TrackingNumber")
            Prelude.<*> (x Prelude..:? "Status")
      )

instance Prelude.Hashable Shipment

instance Prelude.NFData Shipment
