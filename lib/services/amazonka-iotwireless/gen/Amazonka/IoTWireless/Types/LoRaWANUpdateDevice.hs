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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANUpdateDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANUpdateDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN object for update functions.
--
-- /See:/ 'newLoRaWANUpdateDevice' smart constructor.
data LoRaWANUpdateDevice = LoRaWANUpdateDevice'
  { -- | The ID of the service profile.
    serviceProfileId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device profile for the wireless device.
    deviceProfileId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANUpdateDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceProfileId', 'loRaWANUpdateDevice_serviceProfileId' - The ID of the service profile.
--
-- 'deviceProfileId', 'loRaWANUpdateDevice_deviceProfileId' - The ID of the device profile for the wireless device.
newLoRaWANUpdateDevice ::
  LoRaWANUpdateDevice
newLoRaWANUpdateDevice =
  LoRaWANUpdateDevice'
    { serviceProfileId =
        Prelude.Nothing,
      deviceProfileId = Prelude.Nothing
    }

-- | The ID of the service profile.
loRaWANUpdateDevice_serviceProfileId :: Lens.Lens' LoRaWANUpdateDevice (Prelude.Maybe Prelude.Text)
loRaWANUpdateDevice_serviceProfileId = Lens.lens (\LoRaWANUpdateDevice' {serviceProfileId} -> serviceProfileId) (\s@LoRaWANUpdateDevice' {} a -> s {serviceProfileId = a} :: LoRaWANUpdateDevice)

-- | The ID of the device profile for the wireless device.
loRaWANUpdateDevice_deviceProfileId :: Lens.Lens' LoRaWANUpdateDevice (Prelude.Maybe Prelude.Text)
loRaWANUpdateDevice_deviceProfileId = Lens.lens (\LoRaWANUpdateDevice' {deviceProfileId} -> deviceProfileId) (\s@LoRaWANUpdateDevice' {} a -> s {deviceProfileId = a} :: LoRaWANUpdateDevice)

instance Prelude.Hashable LoRaWANUpdateDevice

instance Prelude.NFData LoRaWANUpdateDevice

instance Core.ToJSON LoRaWANUpdateDevice where
  toJSON LoRaWANUpdateDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServiceProfileId" Core..=)
              Prelude.<$> serviceProfileId,
            ("DeviceProfileId" Core..=)
              Prelude.<$> deviceProfileId
          ]
      )
