{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputDeviceSummary
  ( InputDeviceSummary (..)
  -- * Smart constructor
  , mkInputDeviceSummary
  -- * Lenses
  , idsArn
  , idsConnectionState
  , idsDeviceSettingsSyncState
  , idsDeviceUpdateStatus
  , idsHdDeviceSettings
  , idsId
  , idsMacAddress
  , idsName
  , idsNetworkSettings
  , idsSerialNumber
  , idsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.DeviceSettingsSyncState as Types
import qualified Network.AWS.MediaLive.Types.DeviceUpdateStatus as Types
import qualified Network.AWS.MediaLive.Types.InputDeviceConnectionState as Types
import qualified Network.AWS.MediaLive.Types.InputDeviceHdSettings as Types
import qualified Network.AWS.MediaLive.Types.InputDeviceNetworkSettings as Types
import qualified Network.AWS.MediaLive.Types.InputDeviceType as Types
import qualified Network.AWS.Prelude as Core

-- | Details of the input device.
--
-- /See:/ 'mkInputDeviceSummary' smart constructor.
data InputDeviceSummary = InputDeviceSummary'
  { arn :: Core.Maybe Core.Text
    -- ^ The unique ARN of the input device.
  , connectionState :: Core.Maybe Types.InputDeviceConnectionState
    -- ^ The state of the connection between the input device and AWS.
  , deviceSettingsSyncState :: Core.Maybe Types.DeviceSettingsSyncState
    -- ^ The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
  , deviceUpdateStatus :: Core.Maybe Types.DeviceUpdateStatus
    -- ^ The status of software on the input device.
  , hdDeviceSettings :: Core.Maybe Types.InputDeviceHdSettings
    -- ^ Settings that describe an input device that is type HD.
  , id :: Core.Maybe Core.Text
    -- ^ The unique ID of the input device.
  , macAddress :: Core.Maybe Core.Text
    -- ^ The network MAC address of the input device.
  , name :: Core.Maybe Core.Text
    -- ^ A name that you specify for the input device.
  , networkSettings :: Core.Maybe Types.InputDeviceNetworkSettings
    -- ^ Network settings for the input device.
  , serialNumber :: Core.Maybe Core.Text
    -- ^ The unique serial number of the input device.
  , type' :: Core.Maybe Types.InputDeviceType
    -- ^ The type of the input device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDeviceSummary' value with any optional fields omitted.
mkInputDeviceSummary
    :: InputDeviceSummary
mkInputDeviceSummary
  = InputDeviceSummary'{arn = Core.Nothing,
                        connectionState = Core.Nothing,
                        deviceSettingsSyncState = Core.Nothing,
                        deviceUpdateStatus = Core.Nothing, hdDeviceSettings = Core.Nothing,
                        id = Core.Nothing, macAddress = Core.Nothing, name = Core.Nothing,
                        networkSettings = Core.Nothing, serialNumber = Core.Nothing,
                        type' = Core.Nothing}

-- | The unique ARN of the input device.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsArn :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
idsArn = Lens.field @"arn"
{-# INLINEABLE idsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The state of the connection between the input device and AWS.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsConnectionState :: Lens.Lens' InputDeviceSummary (Core.Maybe Types.InputDeviceConnectionState)
idsConnectionState = Lens.field @"connectionState"
{-# INLINEABLE idsConnectionState #-}
{-# DEPRECATED connectionState "Use generic-lens or generic-optics with 'connectionState' instead"  #-}

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- /Note:/ Consider using 'deviceSettingsSyncState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsDeviceSettingsSyncState :: Lens.Lens' InputDeviceSummary (Core.Maybe Types.DeviceSettingsSyncState)
idsDeviceSettingsSyncState = Lens.field @"deviceSettingsSyncState"
{-# INLINEABLE idsDeviceSettingsSyncState #-}
{-# DEPRECATED deviceSettingsSyncState "Use generic-lens or generic-optics with 'deviceSettingsSyncState' instead"  #-}

-- | The status of software on the input device.
--
-- /Note:/ Consider using 'deviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsDeviceUpdateStatus :: Lens.Lens' InputDeviceSummary (Core.Maybe Types.DeviceUpdateStatus)
idsDeviceUpdateStatus = Lens.field @"deviceUpdateStatus"
{-# INLINEABLE idsDeviceUpdateStatus #-}
{-# DEPRECATED deviceUpdateStatus "Use generic-lens or generic-optics with 'deviceUpdateStatus' instead"  #-}

-- | Settings that describe an input device that is type HD.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsHdDeviceSettings :: Lens.Lens' InputDeviceSummary (Core.Maybe Types.InputDeviceHdSettings)
idsHdDeviceSettings = Lens.field @"hdDeviceSettings"
{-# INLINEABLE idsHdDeviceSettings #-}
{-# DEPRECATED hdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead"  #-}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsId :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
idsId = Lens.field @"id"
{-# INLINEABLE idsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The network MAC address of the input device.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsMacAddress :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
idsMacAddress = Lens.field @"macAddress"
{-# INLINEABLE idsMacAddress #-}
{-# DEPRECATED macAddress "Use generic-lens or generic-optics with 'macAddress' instead"  #-}

-- | A name that you specify for the input device.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsName :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
idsName = Lens.field @"name"
{-# INLINEABLE idsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Network settings for the input device.
--
-- /Note:/ Consider using 'networkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsNetworkSettings :: Lens.Lens' InputDeviceSummary (Core.Maybe Types.InputDeviceNetworkSettings)
idsNetworkSettings = Lens.field @"networkSettings"
{-# INLINEABLE idsNetworkSettings #-}
{-# DEPRECATED networkSettings "Use generic-lens or generic-optics with 'networkSettings' instead"  #-}

-- | The unique serial number of the input device.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsSerialNumber :: Lens.Lens' InputDeviceSummary (Core.Maybe Core.Text)
idsSerialNumber = Lens.field @"serialNumber"
{-# INLINEABLE idsSerialNumber #-}
{-# DEPRECATED serialNumber "Use generic-lens or generic-optics with 'serialNumber' instead"  #-}

-- | The type of the input device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idsType :: Lens.Lens' InputDeviceSummary (Core.Maybe Types.InputDeviceType)
idsType = Lens.field @"type'"
{-# INLINEABLE idsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON InputDeviceSummary where
        parseJSON
          = Core.withObject "InputDeviceSummary" Core.$
              \ x ->
                InputDeviceSummary' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "connectionState" Core.<*>
                    x Core..:? "deviceSettingsSyncState"
                    Core.<*> x Core..:? "deviceUpdateStatus"
                    Core.<*> x Core..:? "hdDeviceSettings"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "macAddress"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "networkSettings"
                    Core.<*> x Core..:? "serialNumber"
                    Core.<*> x Core..:? "type"
