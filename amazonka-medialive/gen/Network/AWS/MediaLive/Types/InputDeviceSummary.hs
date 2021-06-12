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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.DeviceSettingsSyncState
import Network.AWS.MediaLive.Types.DeviceUpdateStatus
import Network.AWS.MediaLive.Types.InputDeviceConnectionState
import Network.AWS.MediaLive.Types.InputDeviceHdSettings
import Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
import Network.AWS.MediaLive.Types.InputDeviceType
import Network.AWS.MediaLive.Types.InputDeviceUhdSettings

-- | Details of the input device.
--
-- /See:/ 'newInputDeviceSummary' smart constructor.
data InputDeviceSummary = InputDeviceSummary'
  { -- | Settings that describe an input device that is type UHD.
    uhdDeviceSettings :: Core.Maybe InputDeviceUhdSettings,
    -- | Settings that describe an input device that is type HD.
    hdDeviceSettings :: Core.Maybe InputDeviceHdSettings,
    -- | The network MAC address of the input device.
    macAddress :: Core.Maybe Core.Text,
    -- | The state of the connection between the input device and AWS.
    connectionState :: Core.Maybe InputDeviceConnectionState,
    -- | Network settings for the input device.
    networkSettings :: Core.Maybe InputDeviceNetworkSettings,
    -- | The unique ARN of the input device.
    arn :: Core.Maybe Core.Text,
    -- | The unique ID of the input device.
    id :: Core.Maybe Core.Text,
    -- | The status of software on the input device.
    deviceUpdateStatus :: Core.Maybe DeviceUpdateStatus,
    -- | A name that you specify for the input device.
    name :: Core.Maybe Core.Text,
    -- | The unique serial number of the input device.
    serialNumber :: Core.Maybe Core.Text,
    -- | The type of the input device.
    type' :: Core.Maybe InputDeviceType,
    -- | The status of the action to synchronize the device configuration. If you
    -- change the configuration of the input device (for example, the maximum
    -- bitrate), MediaLive sends the new data to the device. The device might
    -- not update itself immediately. SYNCED means the device has updated its
    -- configuration. SYNCING means that it has not updated its configuration.
    deviceSettingsSyncState :: Core.Maybe DeviceSettingsSyncState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputDeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uhdDeviceSettings', 'inputDeviceSummary_uhdDeviceSettings' - Settings that describe an input device that is type UHD.
--
-- 'hdDeviceSettings', 'inputDeviceSummary_hdDeviceSettings' - Settings that describe an input device that is type HD.
--
-- 'macAddress', 'inputDeviceSummary_macAddress' - The network MAC address of the input device.
--
-- 'connectionState', 'inputDeviceSummary_connectionState' - The state of the connection between the input device and AWS.
--
-- 'networkSettings', 'inputDeviceSummary_networkSettings' - Network settings for the input device.
--
-- 'arn', 'inputDeviceSummary_arn' - The unique ARN of the input device.
--
-- 'id', 'inputDeviceSummary_id' - The unique ID of the input device.
--
-- 'deviceUpdateStatus', 'inputDeviceSummary_deviceUpdateStatus' - The status of software on the input device.
--
-- 'name', 'inputDeviceSummary_name' - A name that you specify for the input device.
--
-- 'serialNumber', 'inputDeviceSummary_serialNumber' - The unique serial number of the input device.
--
-- 'type'', 'inputDeviceSummary_type' - The type of the input device.
--
-- 'deviceSettingsSyncState', 'inputDeviceSummary_deviceSettingsSyncState' - The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
newInputDeviceSummary ::
  InputDeviceSummary
newInputDeviceSummary =
  InputDeviceSummary'
    { uhdDeviceSettings =
        Core.Nothing,
      hdDeviceSettings = Core.Nothing,
      macAddress = Core.Nothing,
      connectionState = Core.Nothing,
      networkSettings = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      deviceUpdateStatus = Core.Nothing,
      name = Core.Nothing,
      serialNumber = Core.Nothing,
      type' = Core.Nothing,
      deviceSettingsSyncState = Core.Nothing
    }

-- | Settings that describe an input device that is type UHD.
inputDeviceSummary_uhdDeviceSettings :: Lens.Lens' InputDeviceSummary (Core.Maybe InputDeviceUhdSettings)
inputDeviceSummary_uhdDeviceSettings = Lens.lens (\InputDeviceSummary' {uhdDeviceSettings} -> uhdDeviceSettings) (\s@InputDeviceSummary' {} a -> s {uhdDeviceSettings = a} :: InputDeviceSummary)

-- | Settings that describe an input device that is type HD.
inputDeviceSummary_hdDeviceSettings :: Lens.Lens' InputDeviceSummary (Core.Maybe InputDeviceHdSettings)
inputDeviceSummary_hdDeviceSettings = Lens.lens (\InputDeviceSummary' {hdDeviceSettings} -> hdDeviceSettings) (\s@InputDeviceSummary' {} a -> s {hdDeviceSettings = a} :: InputDeviceSummary)

-- | The network MAC address of the input device.
inputDeviceSummary_macAddress :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
inputDeviceSummary_macAddress = Lens.lens (\InputDeviceSummary' {macAddress} -> macAddress) (\s@InputDeviceSummary' {} a -> s {macAddress = a} :: InputDeviceSummary)

-- | The state of the connection between the input device and AWS.
inputDeviceSummary_connectionState :: Lens.Lens' InputDeviceSummary (Core.Maybe InputDeviceConnectionState)
inputDeviceSummary_connectionState = Lens.lens (\InputDeviceSummary' {connectionState} -> connectionState) (\s@InputDeviceSummary' {} a -> s {connectionState = a} :: InputDeviceSummary)

-- | Network settings for the input device.
inputDeviceSummary_networkSettings :: Lens.Lens' InputDeviceSummary (Core.Maybe InputDeviceNetworkSettings)
inputDeviceSummary_networkSettings = Lens.lens (\InputDeviceSummary' {networkSettings} -> networkSettings) (\s@InputDeviceSummary' {} a -> s {networkSettings = a} :: InputDeviceSummary)

-- | The unique ARN of the input device.
inputDeviceSummary_arn :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
inputDeviceSummary_arn = Lens.lens (\InputDeviceSummary' {arn} -> arn) (\s@InputDeviceSummary' {} a -> s {arn = a} :: InputDeviceSummary)

-- | The unique ID of the input device.
inputDeviceSummary_id :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
inputDeviceSummary_id = Lens.lens (\InputDeviceSummary' {id} -> id) (\s@InputDeviceSummary' {} a -> s {id = a} :: InputDeviceSummary)

-- | The status of software on the input device.
inputDeviceSummary_deviceUpdateStatus :: Lens.Lens' InputDeviceSummary (Core.Maybe DeviceUpdateStatus)
inputDeviceSummary_deviceUpdateStatus = Lens.lens (\InputDeviceSummary' {deviceUpdateStatus} -> deviceUpdateStatus) (\s@InputDeviceSummary' {} a -> s {deviceUpdateStatus = a} :: InputDeviceSummary)

-- | A name that you specify for the input device.
inputDeviceSummary_name :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
inputDeviceSummary_name = Lens.lens (\InputDeviceSummary' {name} -> name) (\s@InputDeviceSummary' {} a -> s {name = a} :: InputDeviceSummary)

-- | The unique serial number of the input device.
inputDeviceSummary_serialNumber :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
inputDeviceSummary_serialNumber = Lens.lens (\InputDeviceSummary' {serialNumber} -> serialNumber) (\s@InputDeviceSummary' {} a -> s {serialNumber = a} :: InputDeviceSummary)

-- | The type of the input device.
inputDeviceSummary_type :: Lens.Lens' InputDeviceSummary (Core.Maybe InputDeviceType)
inputDeviceSummary_type = Lens.lens (\InputDeviceSummary' {type'} -> type') (\s@InputDeviceSummary' {} a -> s {type' = a} :: InputDeviceSummary)

-- | The status of the action to synchronize the device configuration. If you
-- change the configuration of the input device (for example, the maximum
-- bitrate), MediaLive sends the new data to the device. The device might
-- not update itself immediately. SYNCED means the device has updated its
-- configuration. SYNCING means that it has not updated its configuration.
inputDeviceSummary_deviceSettingsSyncState :: Lens.Lens' InputDeviceSummary (Core.Maybe DeviceSettingsSyncState)
inputDeviceSummary_deviceSettingsSyncState = Lens.lens (\InputDeviceSummary' {deviceSettingsSyncState} -> deviceSettingsSyncState) (\s@InputDeviceSummary' {} a -> s {deviceSettingsSyncState = a} :: InputDeviceSummary)

instance Core.FromJSON InputDeviceSummary where
  parseJSON =
    Core.withObject
      "InputDeviceSummary"
      ( \x ->
          InputDeviceSummary'
            Core.<$> (x Core..:? "uhdDeviceSettings")
            Core.<*> (x Core..:? "hdDeviceSettings")
            Core.<*> (x Core..:? "macAddress")
            Core.<*> (x Core..:? "connectionState")
            Core.<*> (x Core..:? "networkSettings")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "deviceUpdateStatus")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "serialNumber")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "deviceSettingsSyncState")
      )

instance Core.Hashable InputDeviceSummary

instance Core.NFData InputDeviceSummary
