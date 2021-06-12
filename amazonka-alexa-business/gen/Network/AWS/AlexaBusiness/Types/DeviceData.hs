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
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceData where

import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Device attributes.
--
-- /See:/ 'newDeviceData' smart constructor.
data DeviceData = DeviceData'
  { -- | The status of a device.
    deviceStatus :: Core.Maybe DeviceStatus,
    -- | The MAC address of a device.
    macAddress :: Core.Maybe Core.Text,
    -- | The time (in epoch) when the device data was created.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The ARN of a device.
    deviceArn :: Core.Maybe Core.Text,
    -- | The room ARN associated with a device.
    roomArn :: Core.Maybe Core.Text,
    -- | The name of the network profile associated with a device.
    networkProfileName :: Core.Maybe Core.Text,
    -- | Detailed information about a device\'s status.
    deviceStatusInfo :: Core.Maybe DeviceStatusInfo,
    -- | The name of a device.
    deviceName :: Core.Maybe Core.Text,
    -- | The serial number of a device.
    deviceSerialNumber :: Core.Maybe Core.Text,
    -- | The name of the room associated with a device.
    roomName :: Core.Maybe Core.Text,
    -- | The type of a device.
    deviceType :: Core.Maybe Core.Text,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Core.Maybe Core.Text,
    -- | The software version of a device.
    softwareVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceStatus', 'deviceData_deviceStatus' - The status of a device.
--
-- 'macAddress', 'deviceData_macAddress' - The MAC address of a device.
--
-- 'createdTime', 'deviceData_createdTime' - The time (in epoch) when the device data was created.
--
-- 'deviceArn', 'deviceData_deviceArn' - The ARN of a device.
--
-- 'roomArn', 'deviceData_roomArn' - The room ARN associated with a device.
--
-- 'networkProfileName', 'deviceData_networkProfileName' - The name of the network profile associated with a device.
--
-- 'deviceStatusInfo', 'deviceData_deviceStatusInfo' - Detailed information about a device\'s status.
--
-- 'deviceName', 'deviceData_deviceName' - The name of a device.
--
-- 'deviceSerialNumber', 'deviceData_deviceSerialNumber' - The serial number of a device.
--
-- 'roomName', 'deviceData_roomName' - The name of the room associated with a device.
--
-- 'deviceType', 'deviceData_deviceType' - The type of a device.
--
-- 'networkProfileArn', 'deviceData_networkProfileArn' - The ARN of the network profile associated with a device.
--
-- 'softwareVersion', 'deviceData_softwareVersion' - The software version of a device.
newDeviceData ::
  DeviceData
newDeviceData =
  DeviceData'
    { deviceStatus = Core.Nothing,
      macAddress = Core.Nothing,
      createdTime = Core.Nothing,
      deviceArn = Core.Nothing,
      roomArn = Core.Nothing,
      networkProfileName = Core.Nothing,
      deviceStatusInfo = Core.Nothing,
      deviceName = Core.Nothing,
      deviceSerialNumber = Core.Nothing,
      roomName = Core.Nothing,
      deviceType = Core.Nothing,
      networkProfileArn = Core.Nothing,
      softwareVersion = Core.Nothing
    }

-- | The status of a device.
deviceData_deviceStatus :: Lens.Lens' DeviceData (Core.Maybe DeviceStatus)
deviceData_deviceStatus = Lens.lens (\DeviceData' {deviceStatus} -> deviceStatus) (\s@DeviceData' {} a -> s {deviceStatus = a} :: DeviceData)

-- | The MAC address of a device.
deviceData_macAddress :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_macAddress = Lens.lens (\DeviceData' {macAddress} -> macAddress) (\s@DeviceData' {} a -> s {macAddress = a} :: DeviceData)

-- | The time (in epoch) when the device data was created.
deviceData_createdTime :: Lens.Lens' DeviceData (Core.Maybe Core.UTCTime)
deviceData_createdTime = Lens.lens (\DeviceData' {createdTime} -> createdTime) (\s@DeviceData' {} a -> s {createdTime = a} :: DeviceData) Core.. Lens.mapping Core._Time

-- | The ARN of a device.
deviceData_deviceArn :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_deviceArn = Lens.lens (\DeviceData' {deviceArn} -> deviceArn) (\s@DeviceData' {} a -> s {deviceArn = a} :: DeviceData)

-- | The room ARN associated with a device.
deviceData_roomArn :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_roomArn = Lens.lens (\DeviceData' {roomArn} -> roomArn) (\s@DeviceData' {} a -> s {roomArn = a} :: DeviceData)

-- | The name of the network profile associated with a device.
deviceData_networkProfileName :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_networkProfileName = Lens.lens (\DeviceData' {networkProfileName} -> networkProfileName) (\s@DeviceData' {} a -> s {networkProfileName = a} :: DeviceData)

-- | Detailed information about a device\'s status.
deviceData_deviceStatusInfo :: Lens.Lens' DeviceData (Core.Maybe DeviceStatusInfo)
deviceData_deviceStatusInfo = Lens.lens (\DeviceData' {deviceStatusInfo} -> deviceStatusInfo) (\s@DeviceData' {} a -> s {deviceStatusInfo = a} :: DeviceData)

-- | The name of a device.
deviceData_deviceName :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_deviceName = Lens.lens (\DeviceData' {deviceName} -> deviceName) (\s@DeviceData' {} a -> s {deviceName = a} :: DeviceData)

-- | The serial number of a device.
deviceData_deviceSerialNumber :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_deviceSerialNumber = Lens.lens (\DeviceData' {deviceSerialNumber} -> deviceSerialNumber) (\s@DeviceData' {} a -> s {deviceSerialNumber = a} :: DeviceData)

-- | The name of the room associated with a device.
deviceData_roomName :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_roomName = Lens.lens (\DeviceData' {roomName} -> roomName) (\s@DeviceData' {} a -> s {roomName = a} :: DeviceData)

-- | The type of a device.
deviceData_deviceType :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_deviceType = Lens.lens (\DeviceData' {deviceType} -> deviceType) (\s@DeviceData' {} a -> s {deviceType = a} :: DeviceData)

-- | The ARN of the network profile associated with a device.
deviceData_networkProfileArn :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_networkProfileArn = Lens.lens (\DeviceData' {networkProfileArn} -> networkProfileArn) (\s@DeviceData' {} a -> s {networkProfileArn = a} :: DeviceData)

-- | The software version of a device.
deviceData_softwareVersion :: Lens.Lens' DeviceData (Core.Maybe Core.Text)
deviceData_softwareVersion = Lens.lens (\DeviceData' {softwareVersion} -> softwareVersion) (\s@DeviceData' {} a -> s {softwareVersion = a} :: DeviceData)

instance Core.FromJSON DeviceData where
  parseJSON =
    Core.withObject
      "DeviceData"
      ( \x ->
          DeviceData'
            Core.<$> (x Core..:? "DeviceStatus")
            Core.<*> (x Core..:? "MacAddress")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "DeviceArn")
            Core.<*> (x Core..:? "RoomArn")
            Core.<*> (x Core..:? "NetworkProfileName")
            Core.<*> (x Core..:? "DeviceStatusInfo")
            Core.<*> (x Core..:? "DeviceName")
            Core.<*> (x Core..:? "DeviceSerialNumber")
            Core.<*> (x Core..:? "RoomName")
            Core.<*> (x Core..:? "DeviceType")
            Core.<*> (x Core..:? "NetworkProfileArn")
            Core.<*> (x Core..:? "SoftwareVersion")
      )

instance Core.Hashable DeviceData

instance Core.NFData DeviceData
