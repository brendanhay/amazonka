{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceData
  ( DeviceData (..),

    -- * Smart constructor
    mkDeviceData,

    -- * Lenses
    ddDeviceStatus,
    ddNetworkProfileName,
    ddDeviceStatusInfo,
    ddCreatedTime,
    ddDeviceARN,
    ddNetworkProfileARN,
    ddMACAddress,
    ddDeviceName,
    ddRoomARN,
    ddSoftwareVersion,
    ddDeviceType,
    ddRoomName,
    ddDeviceSerialNumber,
  )
where

import Network.AWS.AlexaBusiness.Types.DeviceStatus
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Device attributes.
--
-- /See:/ 'mkDeviceData' smart constructor.
data DeviceData = DeviceData'
  { deviceStatus ::
      Lude.Maybe DeviceStatus,
    networkProfileName :: Lude.Maybe Lude.Text,
    deviceStatusInfo :: Lude.Maybe DeviceStatusInfo,
    createdTime :: Lude.Maybe Lude.Timestamp,
    deviceARN :: Lude.Maybe Lude.Text,
    networkProfileARN :: Lude.Maybe Lude.Text,
    mACAddress :: Lude.Maybe Lude.Text,
    deviceName :: Lude.Maybe Lude.Text,
    roomARN :: Lude.Maybe Lude.Text,
    softwareVersion :: Lude.Maybe Lude.Text,
    deviceType :: Lude.Maybe Lude.Text,
    roomName :: Lude.Maybe Lude.Text,
    deviceSerialNumber :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceData' with the minimum fields required to make a request.
--
-- * 'createdTime' - The time (in epoch) when the device data was created.
-- * 'deviceARN' - The ARN of a device.
-- * 'deviceName' - The name of a device.
-- * 'deviceSerialNumber' - The serial number of a device.
-- * 'deviceStatus' - The status of a device.
-- * 'deviceStatusInfo' - Detailed information about a device's status.
-- * 'deviceType' - The type of a device.
-- * 'mACAddress' - The MAC address of a device.
-- * 'networkProfileARN' - The ARN of the network profile associated with a device.
-- * 'networkProfileName' - The name of the network profile associated with a device.
-- * 'roomARN' - The room ARN associated with a device.
-- * 'roomName' - The name of the room associated with a device.
-- * 'softwareVersion' - The software version of a device.
mkDeviceData ::
  DeviceData
mkDeviceData =
  DeviceData'
    { deviceStatus = Lude.Nothing,
      networkProfileName = Lude.Nothing,
      deviceStatusInfo = Lude.Nothing,
      createdTime = Lude.Nothing,
      deviceARN = Lude.Nothing,
      networkProfileARN = Lude.Nothing,
      mACAddress = Lude.Nothing,
      deviceName = Lude.Nothing,
      roomARN = Lude.Nothing,
      softwareVersion = Lude.Nothing,
      deviceType = Lude.Nothing,
      roomName = Lude.Nothing,
      deviceSerialNumber = Lude.Nothing
    }

-- | The status of a device.
--
-- /Note:/ Consider using 'deviceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceStatus :: Lens.Lens' DeviceData (Lude.Maybe DeviceStatus)
ddDeviceStatus = Lens.lens (deviceStatus :: DeviceData -> Lude.Maybe DeviceStatus) (\s a -> s {deviceStatus = a} :: DeviceData)
{-# DEPRECATED ddDeviceStatus "Use generic-lens or generic-optics with 'deviceStatus' instead." #-}

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddNetworkProfileName :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddNetworkProfileName = Lens.lens (networkProfileName :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileName = a} :: DeviceData)
{-# DEPRECATED ddNetworkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead." #-}

-- | Detailed information about a device's status.
--
-- /Note:/ Consider using 'deviceStatusInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceStatusInfo :: Lens.Lens' DeviceData (Lude.Maybe DeviceStatusInfo)
ddDeviceStatusInfo = Lens.lens (deviceStatusInfo :: DeviceData -> Lude.Maybe DeviceStatusInfo) (\s a -> s {deviceStatusInfo = a} :: DeviceData)
{-# DEPRECATED ddDeviceStatusInfo "Use generic-lens or generic-optics with 'deviceStatusInfo' instead." #-}

-- | The time (in epoch) when the device data was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddCreatedTime :: Lens.Lens' DeviceData (Lude.Maybe Lude.Timestamp)
ddCreatedTime = Lens.lens (createdTime :: DeviceData -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: DeviceData)
{-# DEPRECATED ddCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The ARN of a device.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceARN :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddDeviceARN = Lens.lens (deviceARN :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {deviceARN = a} :: DeviceData)
{-# DEPRECATED ddDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddNetworkProfileARN :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddNetworkProfileARN = Lens.lens (networkProfileARN :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileARN = a} :: DeviceData)
{-# DEPRECATED ddNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

-- | The MAC address of a device.
--
-- /Note:/ Consider using 'mACAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddMACAddress :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddMACAddress = Lens.lens (mACAddress :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {mACAddress = a} :: DeviceData)
{-# DEPRECATED ddMACAddress "Use generic-lens or generic-optics with 'mACAddress' instead." #-}

-- | The name of a device.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceName :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddDeviceName = Lens.lens (deviceName :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: DeviceData)
{-# DEPRECATED ddDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The room ARN associated with a device.
--
-- /Note:/ Consider using 'roomARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRoomARN :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddRoomARN = Lens.lens (roomARN :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {roomARN = a} :: DeviceData)
{-# DEPRECATED ddRoomARN "Use generic-lens or generic-optics with 'roomARN' instead." #-}

-- | The software version of a device.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSoftwareVersion :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddSoftwareVersion = Lens.lens (softwareVersion :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {softwareVersion = a} :: DeviceData)
{-# DEPRECATED ddSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

-- | The type of a device.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceType :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddDeviceType = Lens.lens (deviceType :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {deviceType = a} :: DeviceData)
{-# DEPRECATED ddDeviceType "Use generic-lens or generic-optics with 'deviceType' instead." #-}

-- | The name of the room associated with a device.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRoomName :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddRoomName = Lens.lens (roomName :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {roomName = a} :: DeviceData)
{-# DEPRECATED ddRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

-- | The serial number of a device.
--
-- /Note:/ Consider using 'deviceSerialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceSerialNumber :: Lens.Lens' DeviceData (Lude.Maybe Lude.Text)
ddDeviceSerialNumber = Lens.lens (deviceSerialNumber :: DeviceData -> Lude.Maybe Lude.Text) (\s a -> s {deviceSerialNumber = a} :: DeviceData)
{-# DEPRECATED ddDeviceSerialNumber "Use generic-lens or generic-optics with 'deviceSerialNumber' instead." #-}

instance Lude.FromJSON DeviceData where
  parseJSON =
    Lude.withObject
      "DeviceData"
      ( \x ->
          DeviceData'
            Lude.<$> (x Lude..:? "DeviceStatus")
            Lude.<*> (x Lude..:? "NetworkProfileName")
            Lude.<*> (x Lude..:? "DeviceStatusInfo")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "DeviceArn")
            Lude.<*> (x Lude..:? "NetworkProfileArn")
            Lude.<*> (x Lude..:? "MacAddress")
            Lude.<*> (x Lude..:? "DeviceName")
            Lude.<*> (x Lude..:? "RoomArn")
            Lude.<*> (x Lude..:? "SoftwareVersion")
            Lude.<*> (x Lude..:? "DeviceType")
            Lude.<*> (x Lude..:? "RoomName")
            Lude.<*> (x Lude..:? "DeviceSerialNumber")
      )
