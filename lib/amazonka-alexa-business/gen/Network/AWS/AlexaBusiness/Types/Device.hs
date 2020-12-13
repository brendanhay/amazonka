{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Device
  ( Device (..),

    -- * Smart constructor
    mkDevice,

    -- * Lenses
    dDeviceStatus,
    dDeviceStatusInfo,
    dDeviceARN,
    dMACAddress,
    dDeviceName,
    dRoomARN,
    dSoftwareVersion,
    dDeviceType,
    dNetworkProfileInfo,
    dDeviceSerialNumber,
  )
where

import Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A device with attributes.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { -- | The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
    deviceStatus :: Lude.Maybe DeviceStatus,
    -- | Detailed information about a device's status.
    deviceStatusInfo :: Lude.Maybe DeviceStatusInfo,
    -- | The ARN of a device.
    deviceARN :: Lude.Maybe Lude.Text,
    -- | The MAC address of a device.
    mACAddress :: Lude.Maybe Lude.Text,
    -- | The name of a device.
    deviceName :: Lude.Maybe Lude.Text,
    -- | The room ARN of a device.
    roomARN :: Lude.Maybe Lude.Text,
    -- | The software version of a device.
    softwareVersion :: Lude.Maybe Lude.Text,
    -- | The type of a device.
    deviceType :: Lude.Maybe Lude.Text,
    -- | Detailed information about a device's network profile.
    networkProfileInfo :: Lude.Maybe DeviceNetworkProfileInfo,
    -- | The serial number of a device.
    deviceSerialNumber :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- * 'deviceStatus' - The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
-- * 'deviceStatusInfo' - Detailed information about a device's status.
-- * 'deviceARN' - The ARN of a device.
-- * 'mACAddress' - The MAC address of a device.
-- * 'deviceName' - The name of a device.
-- * 'roomARN' - The room ARN of a device.
-- * 'softwareVersion' - The software version of a device.
-- * 'deviceType' - The type of a device.
-- * 'networkProfileInfo' - Detailed information about a device's network profile.
-- * 'deviceSerialNumber' - The serial number of a device.
mkDevice ::
  Device
mkDevice =
  Device'
    { deviceStatus = Lude.Nothing,
      deviceStatusInfo = Lude.Nothing,
      deviceARN = Lude.Nothing,
      mACAddress = Lude.Nothing,
      deviceName = Lude.Nothing,
      roomARN = Lude.Nothing,
      softwareVersion = Lude.Nothing,
      deviceType = Lude.Nothing,
      networkProfileInfo = Lude.Nothing,
      deviceSerialNumber = Lude.Nothing
    }

-- | The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
--
-- /Note:/ Consider using 'deviceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceStatus :: Lens.Lens' Device (Lude.Maybe DeviceStatus)
dDeviceStatus = Lens.lens (deviceStatus :: Device -> Lude.Maybe DeviceStatus) (\s a -> s {deviceStatus = a} :: Device)
{-# DEPRECATED dDeviceStatus "Use generic-lens or generic-optics with 'deviceStatus' instead." #-}

-- | Detailed information about a device's status.
--
-- /Note:/ Consider using 'deviceStatusInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceStatusInfo :: Lens.Lens' Device (Lude.Maybe DeviceStatusInfo)
dDeviceStatusInfo = Lens.lens (deviceStatusInfo :: Device -> Lude.Maybe DeviceStatusInfo) (\s a -> s {deviceStatusInfo = a} :: Device)
{-# DEPRECATED dDeviceStatusInfo "Use generic-lens or generic-optics with 'deviceStatusInfo' instead." #-}

-- | The ARN of a device.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceARN :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dDeviceARN = Lens.lens (deviceARN :: Device -> Lude.Maybe Lude.Text) (\s a -> s {deviceARN = a} :: Device)
{-# DEPRECATED dDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | The MAC address of a device.
--
-- /Note:/ Consider using 'mACAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMACAddress :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dMACAddress = Lens.lens (mACAddress :: Device -> Lude.Maybe Lude.Text) (\s a -> s {mACAddress = a} :: Device)
{-# DEPRECATED dMACAddress "Use generic-lens or generic-optics with 'mACAddress' instead." #-}

-- | The name of a device.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceName :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dDeviceName = Lens.lens (deviceName :: Device -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: Device)
{-# DEPRECATED dDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The room ARN of a device.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRoomARN :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dRoomARN = Lens.lens (roomARN :: Device -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: Device)
{-# DEPRECATED dRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The software version of a device.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSoftwareVersion :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dSoftwareVersion = Lens.lens (softwareVersion :: Device -> Lude.Maybe Lude.Text) (\s a -> s {softwareVersion = a} :: Device)
{-# DEPRECATED dSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

-- | The type of a device.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceType :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dDeviceType = Lens.lens (deviceType :: Device -> Lude.Maybe Lude.Text) (\s a -> s {deviceType = a} :: Device)
{-# DEPRECATED dDeviceType "Use generic-lens or generic-optics with 'deviceType' instead." #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'networkProfileInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNetworkProfileInfo :: Lens.Lens' Device (Lude.Maybe DeviceNetworkProfileInfo)
dNetworkProfileInfo = Lens.lens (networkProfileInfo :: Device -> Lude.Maybe DeviceNetworkProfileInfo) (\s a -> s {networkProfileInfo = a} :: Device)
{-# DEPRECATED dNetworkProfileInfo "Use generic-lens or generic-optics with 'networkProfileInfo' instead." #-}

-- | The serial number of a device.
--
-- /Note:/ Consider using 'deviceSerialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceSerialNumber :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dDeviceSerialNumber = Lens.lens (deviceSerialNumber :: Device -> Lude.Maybe Lude.Text) (\s a -> s {deviceSerialNumber = a} :: Device)
{-# DEPRECATED dDeviceSerialNumber "Use generic-lens or generic-optics with 'deviceSerialNumber' instead." #-}

instance Lude.FromJSON Device where
  parseJSON =
    Lude.withObject
      "Device"
      ( \x ->
          Device'
            Lude.<$> (x Lude..:? "DeviceStatus")
            Lude.<*> (x Lude..:? "DeviceStatusInfo")
            Lude.<*> (x Lude..:? "DeviceArn")
            Lude.<*> (x Lude..:? "MacAddress")
            Lude.<*> (x Lude..:? "DeviceName")
            Lude.<*> (x Lude..:? "RoomArn")
            Lude.<*> (x Lude..:? "SoftwareVersion")
            Lude.<*> (x Lude..:? "DeviceType")
            Lude.<*> (x Lude..:? "NetworkProfileInfo")
            Lude.<*> (x Lude..:? "DeviceSerialNumber")
      )
