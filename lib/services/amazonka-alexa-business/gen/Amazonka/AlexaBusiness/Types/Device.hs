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
-- Module      : Amazonka.AlexaBusiness.Types.Device
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Device where

import Amazonka.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Amazonka.AlexaBusiness.Types.DeviceStatus
import Amazonka.AlexaBusiness.Types.DeviceStatusInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A device with attributes.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The ARN of a device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a device.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The serial number of a device.
    deviceSerialNumber :: Prelude.Maybe Prelude.Text,
    -- | The status of a device. If the status is not READY, check the
    -- DeviceStatusInfo value for details.
    deviceStatus :: Prelude.Maybe DeviceStatus,
    -- | Detailed information about a device\'s status.
    deviceStatusInfo :: Prelude.Maybe DeviceStatusInfo,
    -- | The type of a device.
    deviceType :: Prelude.Maybe Prelude.Text,
    -- | The MAC address of a device.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about a device\'s network profile.
    networkProfileInfo :: Prelude.Maybe DeviceNetworkProfileInfo,
    -- | The room ARN of a device.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The software version of a device.
    softwareVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'device_deviceArn' - The ARN of a device.
--
-- 'deviceName', 'device_deviceName' - The name of a device.
--
-- 'deviceSerialNumber', 'device_deviceSerialNumber' - The serial number of a device.
--
-- 'deviceStatus', 'device_deviceStatus' - The status of a device. If the status is not READY, check the
-- DeviceStatusInfo value for details.
--
-- 'deviceStatusInfo', 'device_deviceStatusInfo' - Detailed information about a device\'s status.
--
-- 'deviceType', 'device_deviceType' - The type of a device.
--
-- 'macAddress', 'device_macAddress' - The MAC address of a device.
--
-- 'networkProfileInfo', 'device_networkProfileInfo' - Detailed information about a device\'s network profile.
--
-- 'roomArn', 'device_roomArn' - The room ARN of a device.
--
-- 'softwareVersion', 'device_softwareVersion' - The software version of a device.
newDevice ::
  Device
newDevice =
  Device'
    { deviceArn = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      deviceSerialNumber = Prelude.Nothing,
      deviceStatus = Prelude.Nothing,
      deviceStatusInfo = Prelude.Nothing,
      deviceType = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      networkProfileInfo = Prelude.Nothing,
      roomArn = Prelude.Nothing,
      softwareVersion = Prelude.Nothing
    }

-- | The ARN of a device.
device_deviceArn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceArn = Lens.lens (\Device' {deviceArn} -> deviceArn) (\s@Device' {} a -> s {deviceArn = a} :: Device)

-- | The name of a device.
device_deviceName :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceName = Lens.lens (\Device' {deviceName} -> deviceName) (\s@Device' {} a -> s {deviceName = a} :: Device)

-- | The serial number of a device.
device_deviceSerialNumber :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceSerialNumber = Lens.lens (\Device' {deviceSerialNumber} -> deviceSerialNumber) (\s@Device' {} a -> s {deviceSerialNumber = a} :: Device)

-- | The status of a device. If the status is not READY, check the
-- DeviceStatusInfo value for details.
device_deviceStatus :: Lens.Lens' Device (Prelude.Maybe DeviceStatus)
device_deviceStatus = Lens.lens (\Device' {deviceStatus} -> deviceStatus) (\s@Device' {} a -> s {deviceStatus = a} :: Device)

-- | Detailed information about a device\'s status.
device_deviceStatusInfo :: Lens.Lens' Device (Prelude.Maybe DeviceStatusInfo)
device_deviceStatusInfo = Lens.lens (\Device' {deviceStatusInfo} -> deviceStatusInfo) (\s@Device' {} a -> s {deviceStatusInfo = a} :: Device)

-- | The type of a device.
device_deviceType :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceType = Lens.lens (\Device' {deviceType} -> deviceType) (\s@Device' {} a -> s {deviceType = a} :: Device)

-- | The MAC address of a device.
device_macAddress :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_macAddress = Lens.lens (\Device' {macAddress} -> macAddress) (\s@Device' {} a -> s {macAddress = a} :: Device)

-- | Detailed information about a device\'s network profile.
device_networkProfileInfo :: Lens.Lens' Device (Prelude.Maybe DeviceNetworkProfileInfo)
device_networkProfileInfo = Lens.lens (\Device' {networkProfileInfo} -> networkProfileInfo) (\s@Device' {} a -> s {networkProfileInfo = a} :: Device)

-- | The room ARN of a device.
device_roomArn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_roomArn = Lens.lens (\Device' {roomArn} -> roomArn) (\s@Device' {} a -> s {roomArn = a} :: Device)

-- | The software version of a device.
device_softwareVersion :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_softwareVersion = Lens.lens (\Device' {softwareVersion} -> softwareVersion) (\s@Device' {} a -> s {softwareVersion = a} :: Device)

instance Data.FromJSON Device where
  parseJSON =
    Data.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Data..:? "DeviceArn")
            Prelude.<*> (x Data..:? "DeviceName")
            Prelude.<*> (x Data..:? "DeviceSerialNumber")
            Prelude.<*> (x Data..:? "DeviceStatus")
            Prelude.<*> (x Data..:? "DeviceStatusInfo")
            Prelude.<*> (x Data..:? "DeviceType")
            Prelude.<*> (x Data..:? "MacAddress")
            Prelude.<*> (x Data..:? "NetworkProfileInfo")
            Prelude.<*> (x Data..:? "RoomArn")
            Prelude.<*> (x Data..:? "SoftwareVersion")
      )

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt
      `Prelude.hashWithSalt` deviceArn
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` deviceSerialNumber
      `Prelude.hashWithSalt` deviceStatus
      `Prelude.hashWithSalt` deviceStatusInfo
      `Prelude.hashWithSalt` deviceType
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` networkProfileInfo
      `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` softwareVersion

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf deviceArn `Prelude.seq`
      Prelude.rnf deviceName `Prelude.seq`
        Prelude.rnf deviceSerialNumber `Prelude.seq`
          Prelude.rnf deviceStatus `Prelude.seq`
            Prelude.rnf deviceStatusInfo `Prelude.seq`
              Prelude.rnf deviceType `Prelude.seq`
                Prelude.rnf macAddress `Prelude.seq`
                  Prelude.rnf networkProfileInfo `Prelude.seq`
                    Prelude.rnf roomArn `Prelude.seq`
                      Prelude.rnf softwareVersion
