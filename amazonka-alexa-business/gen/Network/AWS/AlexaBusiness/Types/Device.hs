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
-- Module      : Network.AWS.AlexaBusiness.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Device where

import Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A device with attributes.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The status of a device. If the status is not READY, check the
    -- DeviceStatusInfo value for details.
    deviceStatus :: Prelude.Maybe DeviceStatus,
    -- | The MAC address of a device.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The room ARN of a device.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about a device\'s status.
    deviceStatusInfo :: Prelude.Maybe DeviceStatusInfo,
    -- | The name of a device.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about a device\'s network profile.
    networkProfileInfo :: Prelude.Maybe DeviceNetworkProfileInfo,
    -- | The serial number of a device.
    deviceSerialNumber :: Prelude.Maybe Prelude.Text,
    -- | The type of a device.
    deviceType :: Prelude.Maybe Prelude.Text,
    -- | The software version of a device.
    softwareVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceStatus', 'device_deviceStatus' - The status of a device. If the status is not READY, check the
-- DeviceStatusInfo value for details.
--
-- 'macAddress', 'device_macAddress' - The MAC address of a device.
--
-- 'deviceArn', 'device_deviceArn' - The ARN of a device.
--
-- 'roomArn', 'device_roomArn' - The room ARN of a device.
--
-- 'deviceStatusInfo', 'device_deviceStatusInfo' - Detailed information about a device\'s status.
--
-- 'deviceName', 'device_deviceName' - The name of a device.
--
-- 'networkProfileInfo', 'device_networkProfileInfo' - Detailed information about a device\'s network profile.
--
-- 'deviceSerialNumber', 'device_deviceSerialNumber' - The serial number of a device.
--
-- 'deviceType', 'device_deviceType' - The type of a device.
--
-- 'softwareVersion', 'device_softwareVersion' - The software version of a device.
newDevice ::
  Device
newDevice =
  Device'
    { deviceStatus = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      deviceArn = Prelude.Nothing,
      roomArn = Prelude.Nothing,
      deviceStatusInfo = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      networkProfileInfo = Prelude.Nothing,
      deviceSerialNumber = Prelude.Nothing,
      deviceType = Prelude.Nothing,
      softwareVersion = Prelude.Nothing
    }

-- | The status of a device. If the status is not READY, check the
-- DeviceStatusInfo value for details.
device_deviceStatus :: Lens.Lens' Device (Prelude.Maybe DeviceStatus)
device_deviceStatus = Lens.lens (\Device' {deviceStatus} -> deviceStatus) (\s@Device' {} a -> s {deviceStatus = a} :: Device)

-- | The MAC address of a device.
device_macAddress :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_macAddress = Lens.lens (\Device' {macAddress} -> macAddress) (\s@Device' {} a -> s {macAddress = a} :: Device)

-- | The ARN of a device.
device_deviceArn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceArn = Lens.lens (\Device' {deviceArn} -> deviceArn) (\s@Device' {} a -> s {deviceArn = a} :: Device)

-- | The room ARN of a device.
device_roomArn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_roomArn = Lens.lens (\Device' {roomArn} -> roomArn) (\s@Device' {} a -> s {roomArn = a} :: Device)

-- | Detailed information about a device\'s status.
device_deviceStatusInfo :: Lens.Lens' Device (Prelude.Maybe DeviceStatusInfo)
device_deviceStatusInfo = Lens.lens (\Device' {deviceStatusInfo} -> deviceStatusInfo) (\s@Device' {} a -> s {deviceStatusInfo = a} :: Device)

-- | The name of a device.
device_deviceName :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceName = Lens.lens (\Device' {deviceName} -> deviceName) (\s@Device' {} a -> s {deviceName = a} :: Device)

-- | Detailed information about a device\'s network profile.
device_networkProfileInfo :: Lens.Lens' Device (Prelude.Maybe DeviceNetworkProfileInfo)
device_networkProfileInfo = Lens.lens (\Device' {networkProfileInfo} -> networkProfileInfo) (\s@Device' {} a -> s {networkProfileInfo = a} :: Device)

-- | The serial number of a device.
device_deviceSerialNumber :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceSerialNumber = Lens.lens (\Device' {deviceSerialNumber} -> deviceSerialNumber) (\s@Device' {} a -> s {deviceSerialNumber = a} :: Device)

-- | The type of a device.
device_deviceType :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceType = Lens.lens (\Device' {deviceType} -> deviceType) (\s@Device' {} a -> s {deviceType = a} :: Device)

-- | The software version of a device.
device_softwareVersion :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_softwareVersion = Lens.lens (\Device' {softwareVersion} -> softwareVersion) (\s@Device' {} a -> s {softwareVersion = a} :: Device)

instance Prelude.FromJSON Device where
  parseJSON =
    Prelude.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Prelude..:? "DeviceStatus")
            Prelude.<*> (x Prelude..:? "MacAddress")
            Prelude.<*> (x Prelude..:? "DeviceArn")
            Prelude.<*> (x Prelude..:? "RoomArn")
            Prelude.<*> (x Prelude..:? "DeviceStatusInfo")
            Prelude.<*> (x Prelude..:? "DeviceName")
            Prelude.<*> (x Prelude..:? "NetworkProfileInfo")
            Prelude.<*> (x Prelude..:? "DeviceSerialNumber")
            Prelude.<*> (x Prelude..:? "DeviceType")
            Prelude.<*> (x Prelude..:? "SoftwareVersion")
      )

instance Prelude.Hashable Device

instance Prelude.NFData Device
