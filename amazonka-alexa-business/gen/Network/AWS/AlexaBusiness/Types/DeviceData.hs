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
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceData where

import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Device attributes.
--
-- /See:/ 'newDeviceData' smart constructor.
data DeviceData = DeviceData'
  { -- | The status of a device.
    deviceStatus :: Prelude.Maybe DeviceStatus,
    -- | The MAC address of a device.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The time (in epoch) when the device data was created.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ARN of a device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The room ARN associated with a device.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the network profile associated with a device.
    networkProfileName :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about a device\'s status.
    deviceStatusInfo :: Prelude.Maybe DeviceStatusInfo,
    -- | The name of a device.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The serial number of a device.
    deviceSerialNumber :: Prelude.Maybe Prelude.Text,
    -- | The name of the room associated with a device.
    roomName :: Prelude.Maybe Prelude.Text,
    -- | The type of a device.
    deviceType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The software version of a device.
    softwareVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { deviceStatus = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      deviceArn = Prelude.Nothing,
      roomArn = Prelude.Nothing,
      networkProfileName = Prelude.Nothing,
      deviceStatusInfo = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      deviceSerialNumber = Prelude.Nothing,
      roomName = Prelude.Nothing,
      deviceType = Prelude.Nothing,
      networkProfileArn = Prelude.Nothing,
      softwareVersion = Prelude.Nothing
    }

-- | The status of a device.
deviceData_deviceStatus :: Lens.Lens' DeviceData (Prelude.Maybe DeviceStatus)
deviceData_deviceStatus = Lens.lens (\DeviceData' {deviceStatus} -> deviceStatus) (\s@DeviceData' {} a -> s {deviceStatus = a} :: DeviceData)

-- | The MAC address of a device.
deviceData_macAddress :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_macAddress = Lens.lens (\DeviceData' {macAddress} -> macAddress) (\s@DeviceData' {} a -> s {macAddress = a} :: DeviceData)

-- | The time (in epoch) when the device data was created.
deviceData_createdTime :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.UTCTime)
deviceData_createdTime = Lens.lens (\DeviceData' {createdTime} -> createdTime) (\s@DeviceData' {} a -> s {createdTime = a} :: DeviceData) Prelude.. Lens.mapping Prelude._Time

-- | The ARN of a device.
deviceData_deviceArn :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_deviceArn = Lens.lens (\DeviceData' {deviceArn} -> deviceArn) (\s@DeviceData' {} a -> s {deviceArn = a} :: DeviceData)

-- | The room ARN associated with a device.
deviceData_roomArn :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_roomArn = Lens.lens (\DeviceData' {roomArn} -> roomArn) (\s@DeviceData' {} a -> s {roomArn = a} :: DeviceData)

-- | The name of the network profile associated with a device.
deviceData_networkProfileName :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_networkProfileName = Lens.lens (\DeviceData' {networkProfileName} -> networkProfileName) (\s@DeviceData' {} a -> s {networkProfileName = a} :: DeviceData)

-- | Detailed information about a device\'s status.
deviceData_deviceStatusInfo :: Lens.Lens' DeviceData (Prelude.Maybe DeviceStatusInfo)
deviceData_deviceStatusInfo = Lens.lens (\DeviceData' {deviceStatusInfo} -> deviceStatusInfo) (\s@DeviceData' {} a -> s {deviceStatusInfo = a} :: DeviceData)

-- | The name of a device.
deviceData_deviceName :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_deviceName = Lens.lens (\DeviceData' {deviceName} -> deviceName) (\s@DeviceData' {} a -> s {deviceName = a} :: DeviceData)

-- | The serial number of a device.
deviceData_deviceSerialNumber :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_deviceSerialNumber = Lens.lens (\DeviceData' {deviceSerialNumber} -> deviceSerialNumber) (\s@DeviceData' {} a -> s {deviceSerialNumber = a} :: DeviceData)

-- | The name of the room associated with a device.
deviceData_roomName :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_roomName = Lens.lens (\DeviceData' {roomName} -> roomName) (\s@DeviceData' {} a -> s {roomName = a} :: DeviceData)

-- | The type of a device.
deviceData_deviceType :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_deviceType = Lens.lens (\DeviceData' {deviceType} -> deviceType) (\s@DeviceData' {} a -> s {deviceType = a} :: DeviceData)

-- | The ARN of the network profile associated with a device.
deviceData_networkProfileArn :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_networkProfileArn = Lens.lens (\DeviceData' {networkProfileArn} -> networkProfileArn) (\s@DeviceData' {} a -> s {networkProfileArn = a} :: DeviceData)

-- | The software version of a device.
deviceData_softwareVersion :: Lens.Lens' DeviceData (Prelude.Maybe Prelude.Text)
deviceData_softwareVersion = Lens.lens (\DeviceData' {softwareVersion} -> softwareVersion) (\s@DeviceData' {} a -> s {softwareVersion = a} :: DeviceData)

instance Prelude.FromJSON DeviceData where
  parseJSON =
    Prelude.withObject
      "DeviceData"
      ( \x ->
          DeviceData'
            Prelude.<$> (x Prelude..:? "DeviceStatus")
            Prelude.<*> (x Prelude..:? "MacAddress")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "DeviceArn")
            Prelude.<*> (x Prelude..:? "RoomArn")
            Prelude.<*> (x Prelude..:? "NetworkProfileName")
            Prelude.<*> (x Prelude..:? "DeviceStatusInfo")
            Prelude.<*> (x Prelude..:? "DeviceName")
            Prelude.<*> (x Prelude..:? "DeviceSerialNumber")
            Prelude.<*> (x Prelude..:? "RoomName")
            Prelude.<*> (x Prelude..:? "DeviceType")
            Prelude.<*> (x Prelude..:? "NetworkProfileArn")
            Prelude.<*> (x Prelude..:? "SoftwareVersion")
      )

instance Prelude.Hashable DeviceData

instance Prelude.NFData DeviceData
