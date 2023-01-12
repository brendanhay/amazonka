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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANUpdateDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.UpdateAbpV1_0_x
import Amazonka.IoTWireless.Types.UpdateAbpV1_1
import Amazonka.IoTWireless.Types.UpdateFPorts
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN object for update functions.
--
-- /See:/ 'newLoRaWANUpdateDevice' smart constructor.
data LoRaWANUpdateDevice = LoRaWANUpdateDevice'
  { -- | ABP device object for update APIs for v1.0.x
    abpV1_0_x :: Prelude.Maybe UpdateAbpV1_0_x,
    -- | ABP device object for update APIs for v1.1
    abpV1_1 :: Prelude.Maybe UpdateAbpV1_1,
    -- | The ID of the device profile for the wireless device.
    deviceProfileId :: Prelude.Maybe Prelude.Text,
    -- | FPorts object for the positioning information of the device.
    fPorts :: Prelude.Maybe UpdateFPorts,
    -- | The ID of the service profile.
    serviceProfileId :: Prelude.Maybe Prelude.Text
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
-- 'abpV1_0_x', 'loRaWANUpdateDevice_abpV1_0_x' - ABP device object for update APIs for v1.0.x
--
-- 'abpV1_1', 'loRaWANUpdateDevice_abpV1_1' - ABP device object for update APIs for v1.1
--
-- 'deviceProfileId', 'loRaWANUpdateDevice_deviceProfileId' - The ID of the device profile for the wireless device.
--
-- 'fPorts', 'loRaWANUpdateDevice_fPorts' - FPorts object for the positioning information of the device.
--
-- 'serviceProfileId', 'loRaWANUpdateDevice_serviceProfileId' - The ID of the service profile.
newLoRaWANUpdateDevice ::
  LoRaWANUpdateDevice
newLoRaWANUpdateDevice =
  LoRaWANUpdateDevice'
    { abpV1_0_x = Prelude.Nothing,
      abpV1_1 = Prelude.Nothing,
      deviceProfileId = Prelude.Nothing,
      fPorts = Prelude.Nothing,
      serviceProfileId = Prelude.Nothing
    }

-- | ABP device object for update APIs for v1.0.x
loRaWANUpdateDevice_abpV1_0_x :: Lens.Lens' LoRaWANUpdateDevice (Prelude.Maybe UpdateAbpV1_0_x)
loRaWANUpdateDevice_abpV1_0_x = Lens.lens (\LoRaWANUpdateDevice' {abpV1_0_x} -> abpV1_0_x) (\s@LoRaWANUpdateDevice' {} a -> s {abpV1_0_x = a} :: LoRaWANUpdateDevice)

-- | ABP device object for update APIs for v1.1
loRaWANUpdateDevice_abpV1_1 :: Lens.Lens' LoRaWANUpdateDevice (Prelude.Maybe UpdateAbpV1_1)
loRaWANUpdateDevice_abpV1_1 = Lens.lens (\LoRaWANUpdateDevice' {abpV1_1} -> abpV1_1) (\s@LoRaWANUpdateDevice' {} a -> s {abpV1_1 = a} :: LoRaWANUpdateDevice)

-- | The ID of the device profile for the wireless device.
loRaWANUpdateDevice_deviceProfileId :: Lens.Lens' LoRaWANUpdateDevice (Prelude.Maybe Prelude.Text)
loRaWANUpdateDevice_deviceProfileId = Lens.lens (\LoRaWANUpdateDevice' {deviceProfileId} -> deviceProfileId) (\s@LoRaWANUpdateDevice' {} a -> s {deviceProfileId = a} :: LoRaWANUpdateDevice)

-- | FPorts object for the positioning information of the device.
loRaWANUpdateDevice_fPorts :: Lens.Lens' LoRaWANUpdateDevice (Prelude.Maybe UpdateFPorts)
loRaWANUpdateDevice_fPorts = Lens.lens (\LoRaWANUpdateDevice' {fPorts} -> fPorts) (\s@LoRaWANUpdateDevice' {} a -> s {fPorts = a} :: LoRaWANUpdateDevice)

-- | The ID of the service profile.
loRaWANUpdateDevice_serviceProfileId :: Lens.Lens' LoRaWANUpdateDevice (Prelude.Maybe Prelude.Text)
loRaWANUpdateDevice_serviceProfileId = Lens.lens (\LoRaWANUpdateDevice' {serviceProfileId} -> serviceProfileId) (\s@LoRaWANUpdateDevice' {} a -> s {serviceProfileId = a} :: LoRaWANUpdateDevice)

instance Prelude.Hashable LoRaWANUpdateDevice where
  hashWithSalt _salt LoRaWANUpdateDevice' {..} =
    _salt `Prelude.hashWithSalt` abpV1_0_x
      `Prelude.hashWithSalt` abpV1_1
      `Prelude.hashWithSalt` deviceProfileId
      `Prelude.hashWithSalt` fPorts
      `Prelude.hashWithSalt` serviceProfileId

instance Prelude.NFData LoRaWANUpdateDevice where
  rnf LoRaWANUpdateDevice' {..} =
    Prelude.rnf abpV1_0_x
      `Prelude.seq` Prelude.rnf abpV1_1
      `Prelude.seq` Prelude.rnf deviceProfileId
      `Prelude.seq` Prelude.rnf fPorts
      `Prelude.seq` Prelude.rnf serviceProfileId

instance Data.ToJSON LoRaWANUpdateDevice where
  toJSON LoRaWANUpdateDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AbpV1_0_x" Data..=) Prelude.<$> abpV1_0_x,
            ("AbpV1_1" Data..=) Prelude.<$> abpV1_1,
            ("DeviceProfileId" Data..=)
              Prelude.<$> deviceProfileId,
            ("FPorts" Data..=) Prelude.<$> fPorts,
            ("ServiceProfileId" Data..=)
              Prelude.<$> serviceProfileId
          ]
      )
