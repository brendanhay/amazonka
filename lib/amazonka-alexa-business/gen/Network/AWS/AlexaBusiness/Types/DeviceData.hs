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
    ddCreatedTime,
    ddDeviceArn,
    ddDeviceName,
    ddDeviceSerialNumber,
    ddDeviceStatus,
    ddDeviceStatusInfo,
    ddDeviceType,
    ddMacAddress,
    ddNetworkProfileArn,
    ddNetworkProfileName,
    ddRoomArn,
    ddRoomName,
    ddSoftwareVersion,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceName as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceRoomName as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceSerialNumber as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceStatus as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceStatusInfo as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceType as Types
import qualified Network.AWS.AlexaBusiness.Types.MacAddress as Types
import qualified Network.AWS.AlexaBusiness.Types.NetworkProfileName as Types
import qualified Network.AWS.AlexaBusiness.Types.SoftwareVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Device attributes.
--
-- /See:/ 'mkDeviceData' smart constructor.
data DeviceData = DeviceData'
  { -- | The time (in epoch) when the device data was created.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of a device.
    deviceArn :: Core.Maybe Types.Arn,
    -- | The name of a device.
    deviceName :: Core.Maybe Types.DeviceName,
    -- | The serial number of a device.
    deviceSerialNumber :: Core.Maybe Types.DeviceSerialNumber,
    -- | The status of a device.
    deviceStatus :: Core.Maybe Types.DeviceStatus,
    -- | Detailed information about a device's status.
    deviceStatusInfo :: Core.Maybe Types.DeviceStatusInfo,
    -- | The type of a device.
    deviceType :: Core.Maybe Types.DeviceType,
    -- | The MAC address of a device.
    macAddress :: Core.Maybe Types.MacAddress,
    -- | The ARN of the network profile associated with a device.
    networkProfileArn :: Core.Maybe Types.Arn,
    -- | The name of the network profile associated with a device.
    networkProfileName :: Core.Maybe Types.NetworkProfileName,
    -- | The room ARN associated with a device.
    roomArn :: Core.Maybe Types.Arn,
    -- | The name of the room associated with a device.
    roomName :: Core.Maybe Types.DeviceRoomName,
    -- | The software version of a device.
    softwareVersion :: Core.Maybe Types.SoftwareVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeviceData' value with any optional fields omitted.
mkDeviceData ::
  DeviceData
mkDeviceData =
  DeviceData'
    { createdTime = Core.Nothing,
      deviceArn = Core.Nothing,
      deviceName = Core.Nothing,
      deviceSerialNumber = Core.Nothing,
      deviceStatus = Core.Nothing,
      deviceStatusInfo = Core.Nothing,
      deviceType = Core.Nothing,
      macAddress = Core.Nothing,
      networkProfileArn = Core.Nothing,
      networkProfileName = Core.Nothing,
      roomArn = Core.Nothing,
      roomName = Core.Nothing,
      softwareVersion = Core.Nothing
    }

-- | The time (in epoch) when the device data was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddCreatedTime :: Lens.Lens' DeviceData (Core.Maybe Core.NominalDiffTime)
ddCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED ddCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The ARN of a device.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceArn :: Lens.Lens' DeviceData (Core.Maybe Types.Arn)
ddDeviceArn = Lens.field @"deviceArn"
{-# DEPRECATED ddDeviceArn "Use generic-lens or generic-optics with 'deviceArn' instead." #-}

-- | The name of a device.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceName :: Lens.Lens' DeviceData (Core.Maybe Types.DeviceName)
ddDeviceName = Lens.field @"deviceName"
{-# DEPRECATED ddDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The serial number of a device.
--
-- /Note:/ Consider using 'deviceSerialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceSerialNumber :: Lens.Lens' DeviceData (Core.Maybe Types.DeviceSerialNumber)
ddDeviceSerialNumber = Lens.field @"deviceSerialNumber"
{-# DEPRECATED ddDeviceSerialNumber "Use generic-lens or generic-optics with 'deviceSerialNumber' instead." #-}

-- | The status of a device.
--
-- /Note:/ Consider using 'deviceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceStatus :: Lens.Lens' DeviceData (Core.Maybe Types.DeviceStatus)
ddDeviceStatus = Lens.field @"deviceStatus"
{-# DEPRECATED ddDeviceStatus "Use generic-lens or generic-optics with 'deviceStatus' instead." #-}

-- | Detailed information about a device's status.
--
-- /Note:/ Consider using 'deviceStatusInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceStatusInfo :: Lens.Lens' DeviceData (Core.Maybe Types.DeviceStatusInfo)
ddDeviceStatusInfo = Lens.field @"deviceStatusInfo"
{-# DEPRECATED ddDeviceStatusInfo "Use generic-lens or generic-optics with 'deviceStatusInfo' instead." #-}

-- | The type of a device.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDeviceType :: Lens.Lens' DeviceData (Core.Maybe Types.DeviceType)
ddDeviceType = Lens.field @"deviceType"
{-# DEPRECATED ddDeviceType "Use generic-lens or generic-optics with 'deviceType' instead." #-}

-- | The MAC address of a device.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddMacAddress :: Lens.Lens' DeviceData (Core.Maybe Types.MacAddress)
ddMacAddress = Lens.field @"macAddress"
{-# DEPRECATED ddMacAddress "Use generic-lens or generic-optics with 'macAddress' instead." #-}

-- | The ARN of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddNetworkProfileArn :: Lens.Lens' DeviceData (Core.Maybe Types.Arn)
ddNetworkProfileArn = Lens.field @"networkProfileArn"
{-# DEPRECATED ddNetworkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead." #-}

-- | The name of the network profile associated with a device.
--
-- /Note:/ Consider using 'networkProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddNetworkProfileName :: Lens.Lens' DeviceData (Core.Maybe Types.NetworkProfileName)
ddNetworkProfileName = Lens.field @"networkProfileName"
{-# DEPRECATED ddNetworkProfileName "Use generic-lens or generic-optics with 'networkProfileName' instead." #-}

-- | The room ARN associated with a device.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRoomArn :: Lens.Lens' DeviceData (Core.Maybe Types.Arn)
ddRoomArn = Lens.field @"roomArn"
{-# DEPRECATED ddRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

-- | The name of the room associated with a device.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddRoomName :: Lens.Lens' DeviceData (Core.Maybe Types.DeviceRoomName)
ddRoomName = Lens.field @"roomName"
{-# DEPRECATED ddRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

-- | The software version of a device.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddSoftwareVersion :: Lens.Lens' DeviceData (Core.Maybe Types.SoftwareVersion)
ddSoftwareVersion = Lens.field @"softwareVersion"
{-# DEPRECATED ddSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

instance Core.FromJSON DeviceData where
  parseJSON =
    Core.withObject "DeviceData" Core.$
      \x ->
        DeviceData'
          Core.<$> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "DeviceArn")
          Core.<*> (x Core..:? "DeviceName")
          Core.<*> (x Core..:? "DeviceSerialNumber")
          Core.<*> (x Core..:? "DeviceStatus")
          Core.<*> (x Core..:? "DeviceStatusInfo")
          Core.<*> (x Core..:? "DeviceType")
          Core.<*> (x Core..:? "MacAddress")
          Core.<*> (x Core..:? "NetworkProfileArn")
          Core.<*> (x Core..:? "NetworkProfileName")
          Core.<*> (x Core..:? "RoomArn")
          Core.<*> (x Core..:? "RoomName")
          Core.<*> (x Core..:? "SoftwareVersion")
