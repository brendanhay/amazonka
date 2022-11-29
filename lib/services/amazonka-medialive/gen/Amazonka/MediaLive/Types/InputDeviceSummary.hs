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
-- Module      : Amazonka.MediaLive.Types.InputDeviceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDeviceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.DeviceSettingsSyncState
import Amazonka.MediaLive.Types.DeviceUpdateStatus
import Amazonka.MediaLive.Types.InputDeviceConnectionState
import Amazonka.MediaLive.Types.InputDeviceHdSettings
import Amazonka.MediaLive.Types.InputDeviceNetworkSettings
import Amazonka.MediaLive.Types.InputDeviceType
import Amazonka.MediaLive.Types.InputDeviceUhdSettings
import qualified Amazonka.Prelude as Prelude

-- | Details of the input device.
--
-- /See:/ 'newInputDeviceSummary' smart constructor.
data InputDeviceSummary = InputDeviceSummary'
  { -- | A name that you specify for the input device.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the input device.
    type' :: Prelude.Maybe InputDeviceType,
    -- | The status of the action to synchronize the device configuration. If you
    -- change the configuration of the input device (for example, the maximum
    -- bitrate), MediaLive sends the new data to the device. The device might
    -- not update itself immediately. SYNCED means the device has updated its
    -- configuration. SYNCING means that it has not updated its configuration.
    deviceSettingsSyncState :: Prelude.Maybe DeviceSettingsSyncState,
    -- | Network settings for the input device.
    networkSettings :: Prelude.Maybe InputDeviceNetworkSettings,
    -- | Settings that describe an input device that is type UHD.
    uhdDeviceSettings :: Prelude.Maybe InputDeviceUhdSettings,
    -- | The state of the connection between the input device and AWS.
    connectionState :: Prelude.Maybe InputDeviceConnectionState,
    -- | The unique ARN of the input device.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Settings that describe an input device that is type HD.
    hdDeviceSettings :: Prelude.Maybe InputDeviceHdSettings,
    -- | The unique ID of the input device.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status of software on the input device.
    deviceUpdateStatus :: Prelude.Maybe DeviceUpdateStatus,
    -- | The network MAC address of the input device.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The unique serial number of the input device.
    serialNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'inputDeviceSummary_name' - A name that you specify for the input device.
--
-- 'type'', 'inputDeviceSummary_type' - The type of the input device.
--
-- 'deviceSettingsSyncState', 'inputDeviceSummary_deviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
--
-- 'networkSettings', 'inputDeviceSummary_networkSettings' - Network settings for the input device.
--
-- 'uhdDeviceSettings', 'inputDeviceSummary_uhdDeviceSettings' - Settings that describe an input device that is type UHD.
--
-- 'connectionState', 'inputDeviceSummary_connectionState' - The state of the connection between the input device and AWS.
--
-- 'arn', 'inputDeviceSummary_arn' - The unique ARN of the input device.
--
-- 'hdDeviceSettings', 'inputDeviceSummary_hdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- 'id', 'inputDeviceSummary_id' - The unique ID of the input device.
--
-- 'deviceUpdateStatus', 'inputDeviceSummary_deviceUpdateStatus' - The status of software on the input device.
--
-- 'macAddress', 'inputDeviceSummary_macAddress' - The network MAC address of the input device.
--
-- 'serialNumber', 'inputDeviceSummary_serialNumber' - The unique serial number of the input device.
newInputDeviceSummary ::
  InputDeviceSummary
newInputDeviceSummary =
  InputDeviceSummary'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      deviceSettingsSyncState = Prelude.Nothing,
      networkSettings = Prelude.Nothing,
      uhdDeviceSettings = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      arn = Prelude.Nothing,
      hdDeviceSettings = Prelude.Nothing,
      id = Prelude.Nothing,
      deviceUpdateStatus = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      serialNumber = Prelude.Nothing
    }

-- | A name that you specify for the input device.
inputDeviceSummary_name :: Lens.Lens' InputDeviceSummary (Prelude.Maybe Prelude.Text)
inputDeviceSummary_name = Lens.lens (\InputDeviceSummary' {name} -> name) (\s@InputDeviceSummary' {} a -> s {name = a} :: InputDeviceSummary)

-- | The type of the input device.
inputDeviceSummary_type :: Lens.Lens' InputDeviceSummary (Prelude.Maybe InputDeviceType)
inputDeviceSummary_type = Lens.lens (\InputDeviceSummary' {type'} -> type') (\s@InputDeviceSummary' {} a -> s {type' = a} :: InputDeviceSummary)

-- | The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
inputDeviceSummary_deviceSettingsSyncState :: Lens.Lens' InputDeviceSummary (Prelude.Maybe DeviceSettingsSyncState)
inputDeviceSummary_deviceSettingsSyncState = Lens.lens (\InputDeviceSummary' {deviceSettingsSyncState} -> deviceSettingsSyncState) (\s@InputDeviceSummary' {} a -> s {deviceSettingsSyncState = a} :: InputDeviceSummary)

-- | Network settings for the input device.
inputDeviceSummary_networkSettings :: Lens.Lens' InputDeviceSummary (Prelude.Maybe InputDeviceNetworkSettings)
inputDeviceSummary_networkSettings = Lens.lens (\InputDeviceSummary' {networkSettings} -> networkSettings) (\s@InputDeviceSummary' {} a -> s {networkSettings = a} :: InputDeviceSummary)

-- | Settings that describe an input device that is type UHD.
inputDeviceSummary_uhdDeviceSettings :: Lens.Lens' InputDeviceSummary (Prelude.Maybe InputDeviceUhdSettings)
inputDeviceSummary_uhdDeviceSettings = Lens.lens (\InputDeviceSummary' {uhdDeviceSettings} -> uhdDeviceSettings) (\s@InputDeviceSummary' {} a -> s {uhdDeviceSettings = a} :: InputDeviceSummary)

-- | The state of the connection between the input device and AWS.
inputDeviceSummary_connectionState :: Lens.Lens' InputDeviceSummary (Prelude.Maybe InputDeviceConnectionState)
inputDeviceSummary_connectionState = Lens.lens (\InputDeviceSummary' {connectionState} -> connectionState) (\s@InputDeviceSummary' {} a -> s {connectionState = a} :: InputDeviceSummary)

-- | The unique ARN of the input device.
inputDeviceSummary_arn :: Lens.Lens' InputDeviceSummary (Prelude.Maybe Prelude.Text)
inputDeviceSummary_arn = Lens.lens (\InputDeviceSummary' {arn} -> arn) (\s@InputDeviceSummary' {} a -> s {arn = a} :: InputDeviceSummary)

-- | Settings that describe an input device that is type HD.
inputDeviceSummary_hdDeviceSettings :: Lens.Lens' InputDeviceSummary (Prelude.Maybe InputDeviceHdSettings)
inputDeviceSummary_hdDeviceSettings = Lens.lens (\InputDeviceSummary' {hdDeviceSettings} -> hdDeviceSettings) (\s@InputDeviceSummary' {} a -> s {hdDeviceSettings = a} :: InputDeviceSummary)

-- | The unique ID of the input device.
inputDeviceSummary_id :: Lens.Lens' InputDeviceSummary (Prelude.Maybe Prelude.Text)
inputDeviceSummary_id = Lens.lens (\InputDeviceSummary' {id} -> id) (\s@InputDeviceSummary' {} a -> s {id = a} :: InputDeviceSummary)

-- | The status of software on the input device.
inputDeviceSummary_deviceUpdateStatus :: Lens.Lens' InputDeviceSummary (Prelude.Maybe DeviceUpdateStatus)
inputDeviceSummary_deviceUpdateStatus = Lens.lens (\InputDeviceSummary' {deviceUpdateStatus} -> deviceUpdateStatus) (\s@InputDeviceSummary' {} a -> s {deviceUpdateStatus = a} :: InputDeviceSummary)

-- | The network MAC address of the input device.
inputDeviceSummary_macAddress :: Lens.Lens' InputDeviceSummary (Prelude.Maybe Prelude.Text)
inputDeviceSummary_macAddress = Lens.lens (\InputDeviceSummary' {macAddress} -> macAddress) (\s@InputDeviceSummary' {} a -> s {macAddress = a} :: InputDeviceSummary)

-- | The unique serial number of the input device.
inputDeviceSummary_serialNumber :: Lens.Lens' InputDeviceSummary (Prelude.Maybe Prelude.Text)
inputDeviceSummary_serialNumber = Lens.lens (\InputDeviceSummary' {serialNumber} -> serialNumber) (\s@InputDeviceSummary' {} a -> s {serialNumber = a} :: InputDeviceSummary)

instance Core.FromJSON InputDeviceSummary where
  parseJSON =
    Core.withObject
      "InputDeviceSummary"
      ( \x ->
          InputDeviceSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "deviceSettingsSyncState")
            Prelude.<*> (x Core..:? "networkSettings")
            Prelude.<*> (x Core..:? "uhdDeviceSettings")
            Prelude.<*> (x Core..:? "connectionState")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "hdDeviceSettings")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "deviceUpdateStatus")
            Prelude.<*> (x Core..:? "macAddress")
            Prelude.<*> (x Core..:? "serialNumber")
      )

instance Prelude.Hashable InputDeviceSummary where
  hashWithSalt _salt InputDeviceSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` deviceSettingsSyncState
      `Prelude.hashWithSalt` networkSettings
      `Prelude.hashWithSalt` uhdDeviceSettings
      `Prelude.hashWithSalt` connectionState
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` hdDeviceSettings
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` deviceUpdateStatus
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` serialNumber

instance Prelude.NFData InputDeviceSummary where
  rnf InputDeviceSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf deviceSettingsSyncState
      `Prelude.seq` Prelude.rnf networkSettings
      `Prelude.seq` Prelude.rnf uhdDeviceSettings
      `Prelude.seq` Prelude.rnf connectionState
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf hdDeviceSettings
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf deviceUpdateStatus
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf serialNumber
