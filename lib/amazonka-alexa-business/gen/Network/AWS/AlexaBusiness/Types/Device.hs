{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Device
  ( Device (..)
  -- * Smart constructor
  , mkDevice
  -- * Lenses
  , dDeviceArn
  , dDeviceName
  , dDeviceSerialNumber
  , dDeviceStatus
  , dDeviceStatusInfo
  , dDeviceType
  , dMacAddress
  , dNetworkProfileInfo
  , dRoomArn
  , dSoftwareVersion
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceName as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceSerialNumber as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceStatus as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceStatusInfo as Types
import qualified Network.AWS.AlexaBusiness.Types.DeviceType as Types
import qualified Network.AWS.AlexaBusiness.Types.MacAddress as Types
import qualified Network.AWS.AlexaBusiness.Types.SoftwareVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A device with attributes.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { deviceArn :: Core.Maybe Types.Arn
    -- ^ The ARN of a device.
  , deviceName :: Core.Maybe Types.DeviceName
    -- ^ The name of a device.
  , deviceSerialNumber :: Core.Maybe Types.DeviceSerialNumber
    -- ^ The serial number of a device.
  , deviceStatus :: Core.Maybe Types.DeviceStatus
    -- ^ The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
  , deviceStatusInfo :: Core.Maybe Types.DeviceStatusInfo
    -- ^ Detailed information about a device's status.
  , deviceType :: Core.Maybe Types.DeviceType
    -- ^ The type of a device.
  , macAddress :: Core.Maybe Types.MacAddress
    -- ^ The MAC address of a device.
  , networkProfileInfo :: Core.Maybe Types.DeviceNetworkProfileInfo
    -- ^ Detailed information about a device's network profile.
  , roomArn :: Core.Maybe Types.Arn
    -- ^ The room ARN of a device.
  , softwareVersion :: Core.Maybe Types.SoftwareVersion
    -- ^ The software version of a device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Device' value with any optional fields omitted.
mkDevice
    :: Device
mkDevice
  = Device'{deviceArn = Core.Nothing, deviceName = Core.Nothing,
            deviceSerialNumber = Core.Nothing, deviceStatus = Core.Nothing,
            deviceStatusInfo = Core.Nothing, deviceType = Core.Nothing,
            macAddress = Core.Nothing, networkProfileInfo = Core.Nothing,
            roomArn = Core.Nothing, softwareVersion = Core.Nothing}

-- | The ARN of a device.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceArn :: Lens.Lens' Device (Core.Maybe Types.Arn)
dDeviceArn = Lens.field @"deviceArn"
{-# INLINEABLE dDeviceArn #-}
{-# DEPRECATED deviceArn "Use generic-lens or generic-optics with 'deviceArn' instead"  #-}

-- | The name of a device.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceName :: Lens.Lens' Device (Core.Maybe Types.DeviceName)
dDeviceName = Lens.field @"deviceName"
{-# INLINEABLE dDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | The serial number of a device.
--
-- /Note:/ Consider using 'deviceSerialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceSerialNumber :: Lens.Lens' Device (Core.Maybe Types.DeviceSerialNumber)
dDeviceSerialNumber = Lens.field @"deviceSerialNumber"
{-# INLINEABLE dDeviceSerialNumber #-}
{-# DEPRECATED deviceSerialNumber "Use generic-lens or generic-optics with 'deviceSerialNumber' instead"  #-}

-- | The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
--
-- /Note:/ Consider using 'deviceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceStatus :: Lens.Lens' Device (Core.Maybe Types.DeviceStatus)
dDeviceStatus = Lens.field @"deviceStatus"
{-# INLINEABLE dDeviceStatus #-}
{-# DEPRECATED deviceStatus "Use generic-lens or generic-optics with 'deviceStatus' instead"  #-}

-- | Detailed information about a device's status.
--
-- /Note:/ Consider using 'deviceStatusInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceStatusInfo :: Lens.Lens' Device (Core.Maybe Types.DeviceStatusInfo)
dDeviceStatusInfo = Lens.field @"deviceStatusInfo"
{-# INLINEABLE dDeviceStatusInfo #-}
{-# DEPRECATED deviceStatusInfo "Use generic-lens or generic-optics with 'deviceStatusInfo' instead"  #-}

-- | The type of a device.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeviceType :: Lens.Lens' Device (Core.Maybe Types.DeviceType)
dDeviceType = Lens.field @"deviceType"
{-# INLINEABLE dDeviceType #-}
{-# DEPRECATED deviceType "Use generic-lens or generic-optics with 'deviceType' instead"  #-}

-- | The MAC address of a device.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMacAddress :: Lens.Lens' Device (Core.Maybe Types.MacAddress)
dMacAddress = Lens.field @"macAddress"
{-# INLINEABLE dMacAddress #-}
{-# DEPRECATED macAddress "Use generic-lens or generic-optics with 'macAddress' instead"  #-}

-- | Detailed information about a device's network profile.
--
-- /Note:/ Consider using 'networkProfileInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNetworkProfileInfo :: Lens.Lens' Device (Core.Maybe Types.DeviceNetworkProfileInfo)
dNetworkProfileInfo = Lens.field @"networkProfileInfo"
{-# INLINEABLE dNetworkProfileInfo #-}
{-# DEPRECATED networkProfileInfo "Use generic-lens or generic-optics with 'networkProfileInfo' instead"  #-}

-- | The room ARN of a device.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRoomArn :: Lens.Lens' Device (Core.Maybe Types.Arn)
dRoomArn = Lens.field @"roomArn"
{-# INLINEABLE dRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

-- | The software version of a device.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSoftwareVersion :: Lens.Lens' Device (Core.Maybe Types.SoftwareVersion)
dSoftwareVersion = Lens.field @"softwareVersion"
{-# INLINEABLE dSoftwareVersion #-}
{-# DEPRECATED softwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead"  #-}

instance Core.FromJSON Device where
        parseJSON
          = Core.withObject "Device" Core.$
              \ x ->
                Device' Core.<$>
                  (x Core..:? "DeviceArn") Core.<*> x Core..:? "DeviceName" Core.<*>
                    x Core..:? "DeviceSerialNumber"
                    Core.<*> x Core..:? "DeviceStatus"
                    Core.<*> x Core..:? "DeviceStatusInfo"
                    Core.<*> x Core..:? "DeviceType"
                    Core.<*> x Core..:? "MacAddress"
                    Core.<*> x Core..:? "NetworkProfileInfo"
                    Core.<*> x Core..:? "RoomArn"
                    Core.<*> x Core..:? "SoftwareVersion"
