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
-- Module      : Amazonka.IoTWireless.Types.SidewalkDeviceMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkDeviceMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.BatteryLevel
import Amazonka.IoTWireless.Types.DeviceState
import Amazonka.IoTWireless.Types.Event
import qualified Amazonka.Prelude as Prelude

-- | MetaData for Sidewalk device.
--
-- /See:/ 'newSidewalkDeviceMetadata' smart constructor.
data SidewalkDeviceMetadata = SidewalkDeviceMetadata'
  { -- | Device state defines the device status of sidewalk device.
    deviceState :: Prelude.Maybe DeviceState,
    -- | Sidewalk device battery level.
    batteryLevel :: Prelude.Maybe BatteryLevel,
    -- | Sidewalk device status notification.
    event :: Prelude.Maybe Event,
    -- | The RSSI value.
    rssi :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkDeviceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceState', 'sidewalkDeviceMetadata_deviceState' - Device state defines the device status of sidewalk device.
--
-- 'batteryLevel', 'sidewalkDeviceMetadata_batteryLevel' - Sidewalk device battery level.
--
-- 'event', 'sidewalkDeviceMetadata_event' - Sidewalk device status notification.
--
-- 'rssi', 'sidewalkDeviceMetadata_rssi' - The RSSI value.
newSidewalkDeviceMetadata ::
  SidewalkDeviceMetadata
newSidewalkDeviceMetadata =
  SidewalkDeviceMetadata'
    { deviceState =
        Prelude.Nothing,
      batteryLevel = Prelude.Nothing,
      event = Prelude.Nothing,
      rssi = Prelude.Nothing
    }

-- | Device state defines the device status of sidewalk device.
sidewalkDeviceMetadata_deviceState :: Lens.Lens' SidewalkDeviceMetadata (Prelude.Maybe DeviceState)
sidewalkDeviceMetadata_deviceState = Lens.lens (\SidewalkDeviceMetadata' {deviceState} -> deviceState) (\s@SidewalkDeviceMetadata' {} a -> s {deviceState = a} :: SidewalkDeviceMetadata)

-- | Sidewalk device battery level.
sidewalkDeviceMetadata_batteryLevel :: Lens.Lens' SidewalkDeviceMetadata (Prelude.Maybe BatteryLevel)
sidewalkDeviceMetadata_batteryLevel = Lens.lens (\SidewalkDeviceMetadata' {batteryLevel} -> batteryLevel) (\s@SidewalkDeviceMetadata' {} a -> s {batteryLevel = a} :: SidewalkDeviceMetadata)

-- | Sidewalk device status notification.
sidewalkDeviceMetadata_event :: Lens.Lens' SidewalkDeviceMetadata (Prelude.Maybe Event)
sidewalkDeviceMetadata_event = Lens.lens (\SidewalkDeviceMetadata' {event} -> event) (\s@SidewalkDeviceMetadata' {} a -> s {event = a} :: SidewalkDeviceMetadata)

-- | The RSSI value.
sidewalkDeviceMetadata_rssi :: Lens.Lens' SidewalkDeviceMetadata (Prelude.Maybe Prelude.Int)
sidewalkDeviceMetadata_rssi = Lens.lens (\SidewalkDeviceMetadata' {rssi} -> rssi) (\s@SidewalkDeviceMetadata' {} a -> s {rssi = a} :: SidewalkDeviceMetadata)

instance Core.FromJSON SidewalkDeviceMetadata where
  parseJSON =
    Core.withObject
      "SidewalkDeviceMetadata"
      ( \x ->
          SidewalkDeviceMetadata'
            Prelude.<$> (x Core..:? "DeviceState")
            Prelude.<*> (x Core..:? "BatteryLevel")
            Prelude.<*> (x Core..:? "Event")
            Prelude.<*> (x Core..:? "Rssi")
      )

instance Prelude.Hashable SidewalkDeviceMetadata where
  hashWithSalt _salt SidewalkDeviceMetadata' {..} =
    _salt `Prelude.hashWithSalt` deviceState
      `Prelude.hashWithSalt` batteryLevel
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` rssi

instance Prelude.NFData SidewalkDeviceMetadata where
  rnf SidewalkDeviceMetadata' {..} =
    Prelude.rnf deviceState
      `Prelude.seq` Prelude.rnf batteryLevel
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf rssi
