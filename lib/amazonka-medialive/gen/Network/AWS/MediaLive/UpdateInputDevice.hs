{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateInputDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the parameters for the input device.
module Network.AWS.MediaLive.UpdateInputDevice
    (
    -- * Creating a request
      UpdateInputDevice (..)
    , mkUpdateInputDevice
    -- ** Request lenses
    , uidInputDeviceId
    , uidHdDeviceSettings
    , uidName

    -- * Destructuring the response
    , UpdateInputDeviceResponse (..)
    , mkUpdateInputDeviceResponse
    -- ** Response lenses
    , uidrrsArn
    , uidrrsConnectionState
    , uidrrsDeviceSettingsSyncState
    , uidrrsDeviceUpdateStatus
    , uidrrsHdDeviceSettings
    , uidrrsId
    , uidrrsMacAddress
    , uidrrsName
    , uidrrsNetworkSettings
    , uidrrsSerialNumber
    , uidrrsType
    , uidrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update an input device.
--
-- /See:/ 'mkUpdateInputDevice' smart constructor.
data UpdateInputDevice = UpdateInputDevice'
  { inputDeviceId :: Core.Text
    -- ^ The unique ID of the input device. For example, hd-123456789abcdef.
  , hdDeviceSettings :: Core.Maybe Types.InputDeviceConfigurableSettings
    -- ^ The settings that you want to apply to the input device.
  , name :: Core.Maybe Core.Text
    -- ^ The name that you assigned to this input device (not the unique ID).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInputDevice' value with any optional fields omitted.
mkUpdateInputDevice
    :: Core.Text -- ^ 'inputDeviceId'
    -> UpdateInputDevice
mkUpdateInputDevice inputDeviceId
  = UpdateInputDevice'{inputDeviceId,
                       hdDeviceSettings = Core.Nothing, name = Core.Nothing}

-- | The unique ID of the input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidInputDeviceId :: Lens.Lens' UpdateInputDevice Core.Text
uidInputDeviceId = Lens.field @"inputDeviceId"
{-# INLINEABLE uidInputDeviceId #-}
{-# DEPRECATED inputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead"  #-}

-- | The settings that you want to apply to the input device.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidHdDeviceSettings :: Lens.Lens' UpdateInputDevice (Core.Maybe Types.InputDeviceConfigurableSettings)
uidHdDeviceSettings = Lens.field @"hdDeviceSettings"
{-# INLINEABLE uidHdDeviceSettings #-}
{-# DEPRECATED hdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead"  #-}

-- | The name that you assigned to this input device (not the unique ID).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidName :: Lens.Lens' UpdateInputDevice (Core.Maybe Core.Text)
uidName = Lens.field @"name"
{-# INLINEABLE uidName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateInputDevice where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateInputDevice where
        toHeaders UpdateInputDevice{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateInputDevice where
        toJSON UpdateInputDevice{..}
          = Core.object
              (Core.catMaybes
                 [("hdDeviceSettings" Core..=) Core.<$> hdDeviceSettings,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateInputDevice where
        type Rs UpdateInputDevice = UpdateInputDeviceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/prod/inputDevices/" Core.<> Core.toText inputDeviceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateInputDeviceResponse' Core.<$>
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
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for UpdateInputDeviceResponse
--
-- /See:/ 'mkUpdateInputDeviceResponse' smart constructor.
data UpdateInputDeviceResponse = UpdateInputDeviceResponse'
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
    -- ^ The network settings for the input device.
  , serialNumber :: Core.Maybe Core.Text
    -- ^ The unique serial number of the input device.
  , type' :: Core.Maybe Types.InputDeviceType
    -- ^ The type of the input device.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInputDeviceResponse' value with any optional fields omitted.
mkUpdateInputDeviceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateInputDeviceResponse
mkUpdateInputDeviceResponse responseStatus
  = UpdateInputDeviceResponse'{arn = Core.Nothing,
                               connectionState = Core.Nothing,
                               deviceSettingsSyncState = Core.Nothing,
                               deviceUpdateStatus = Core.Nothing, hdDeviceSettings = Core.Nothing,
                               id = Core.Nothing, macAddress = Core.Nothing, name = Core.Nothing,
                               networkSettings = Core.Nothing, serialNumber = Core.Nothing,
                               type' = Core.Nothing, responseStatus}

-- | The unique ARN of the input device.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsArn :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsArn = Lens.field @"arn"
{-# INLINEABLE uidrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The state of the connection between the input device and AWS.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsConnectionState :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.InputDeviceConnectionState)
uidrrsConnectionState = Lens.field @"connectionState"
{-# INLINEABLE uidrrsConnectionState #-}
{-# DEPRECATED connectionState "Use generic-lens or generic-optics with 'connectionState' instead"  #-}

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- /Note:/ Consider using 'deviceSettingsSyncState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsDeviceSettingsSyncState :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.DeviceSettingsSyncState)
uidrrsDeviceSettingsSyncState = Lens.field @"deviceSettingsSyncState"
{-# INLINEABLE uidrrsDeviceSettingsSyncState #-}
{-# DEPRECATED deviceSettingsSyncState "Use generic-lens or generic-optics with 'deviceSettingsSyncState' instead"  #-}

-- | The status of software on the input device.
--
-- /Note:/ Consider using 'deviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsDeviceUpdateStatus :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.DeviceUpdateStatus)
uidrrsDeviceUpdateStatus = Lens.field @"deviceUpdateStatus"
{-# INLINEABLE uidrrsDeviceUpdateStatus #-}
{-# DEPRECATED deviceUpdateStatus "Use generic-lens or generic-optics with 'deviceUpdateStatus' instead"  #-}

-- | Settings that describe an input device that is type HD.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsHdDeviceSettings :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.InputDeviceHdSettings)
uidrrsHdDeviceSettings = Lens.field @"hdDeviceSettings"
{-# INLINEABLE uidrrsHdDeviceSettings #-}
{-# DEPRECATED hdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead"  #-}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsId :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsId = Lens.field @"id"
{-# INLINEABLE uidrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The network MAC address of the input device.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsMacAddress :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsMacAddress = Lens.field @"macAddress"
{-# INLINEABLE uidrrsMacAddress #-}
{-# DEPRECATED macAddress "Use generic-lens or generic-optics with 'macAddress' instead"  #-}

-- | A name that you specify for the input device.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsName :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsName = Lens.field @"name"
{-# INLINEABLE uidrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The network settings for the input device.
--
-- /Note:/ Consider using 'networkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsNetworkSettings :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.InputDeviceNetworkSettings)
uidrrsNetworkSettings = Lens.field @"networkSettings"
{-# INLINEABLE uidrrsNetworkSettings #-}
{-# DEPRECATED networkSettings "Use generic-lens or generic-optics with 'networkSettings' instead"  #-}

-- | The unique serial number of the input device.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsSerialNumber :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsSerialNumber = Lens.field @"serialNumber"
{-# INLINEABLE uidrrsSerialNumber #-}
{-# DEPRECATED serialNumber "Use generic-lens or generic-optics with 'serialNumber' instead"  #-}

-- | The type of the input device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsType :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.InputDeviceType)
uidrrsType = Lens.field @"type'"
{-# INLINEABLE uidrrsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsResponseStatus :: Lens.Lens' UpdateInputDeviceResponse Core.Int
uidrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uidrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
