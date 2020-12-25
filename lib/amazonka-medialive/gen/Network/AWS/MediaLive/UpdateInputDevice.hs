{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateInputDevice (..),
    mkUpdateInputDevice,

    -- ** Request lenses
    uidInputDeviceId,
    uidHdDeviceSettings,
    uidName,

    -- * Destructuring the response
    UpdateInputDeviceResponse (..),
    mkUpdateInputDeviceResponse,

    -- ** Response lenses
    uidrrsArn,
    uidrrsConnectionState,
    uidrrsDeviceSettingsSyncState,
    uidrrsDeviceUpdateStatus,
    uidrrsHdDeviceSettings,
    uidrrsId,
    uidrrsMacAddress,
    uidrrsName,
    uidrrsNetworkSettings,
    uidrrsSerialNumber,
    uidrrsType,
    uidrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update an input device.
--
-- /See:/ 'mkUpdateInputDevice' smart constructor.
data UpdateInputDevice = UpdateInputDevice'
  { -- | The unique ID of the input device. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text,
    -- | The settings that you want to apply to the input device.
    hdDeviceSettings :: Core.Maybe Types.InputDeviceConfigurableSettings,
    -- | The name that you assigned to this input device (not the unique ID).
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInputDevice' value with any optional fields omitted.
mkUpdateInputDevice ::
  -- | 'inputDeviceId'
  Core.Text ->
  UpdateInputDevice
mkUpdateInputDevice inputDeviceId =
  UpdateInputDevice'
    { inputDeviceId,
      hdDeviceSettings = Core.Nothing,
      name = Core.Nothing
    }

-- | The unique ID of the input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidInputDeviceId :: Lens.Lens' UpdateInputDevice Core.Text
uidInputDeviceId = Lens.field @"inputDeviceId"
{-# DEPRECATED uidInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

-- | The settings that you want to apply to the input device.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidHdDeviceSettings :: Lens.Lens' UpdateInputDevice (Core.Maybe Types.InputDeviceConfigurableSettings)
uidHdDeviceSettings = Lens.field @"hdDeviceSettings"
{-# DEPRECATED uidHdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead." #-}

-- | The name that you assigned to this input device (not the unique ID).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidName :: Lens.Lens' UpdateInputDevice (Core.Maybe Core.Text)
uidName = Lens.field @"name"
{-# DEPRECATED uidName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateInputDevice where
  toJSON UpdateInputDevice {..} =
    Core.object
      ( Core.catMaybes
          [ ("hdDeviceSettings" Core..=) Core.<$> hdDeviceSettings,
            ("name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateInputDevice where
  type Rs UpdateInputDevice = UpdateInputDeviceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/prod/inputDevices/" Core.<> (Core.toText inputDeviceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputDeviceResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "connectionState")
            Core.<*> (x Core..:? "deviceSettingsSyncState")
            Core.<*> (x Core..:? "deviceUpdateStatus")
            Core.<*> (x Core..:? "hdDeviceSettings")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "macAddress")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "networkSettings")
            Core.<*> (x Core..:? "serialNumber")
            Core.<*> (x Core..:? "type")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for UpdateInputDeviceResponse
--
-- /See:/ 'mkUpdateInputDeviceResponse' smart constructor.
data UpdateInputDeviceResponse = UpdateInputDeviceResponse'
  { -- | The unique ARN of the input device.
    arn :: Core.Maybe Core.Text,
    -- | The state of the connection between the input device and AWS.
    connectionState :: Core.Maybe Types.InputDeviceConnectionState,
    -- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
    deviceSettingsSyncState :: Core.Maybe Types.DeviceSettingsSyncState,
    -- | The status of software on the input device.
    deviceUpdateStatus :: Core.Maybe Types.DeviceUpdateStatus,
    -- | Settings that describe an input device that is type HD.
    hdDeviceSettings :: Core.Maybe Types.InputDeviceHdSettings,
    -- | The unique ID of the input device.
    id :: Core.Maybe Core.Text,
    -- | The network MAC address of the input device.
    macAddress :: Core.Maybe Core.Text,
    -- | A name that you specify for the input device.
    name :: Core.Maybe Core.Text,
    -- | The network settings for the input device.
    networkSettings :: Core.Maybe Types.InputDeviceNetworkSettings,
    -- | The unique serial number of the input device.
    serialNumber :: Core.Maybe Core.Text,
    -- | The type of the input device.
    type' :: Core.Maybe Types.InputDeviceType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInputDeviceResponse' value with any optional fields omitted.
mkUpdateInputDeviceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateInputDeviceResponse
mkUpdateInputDeviceResponse responseStatus =
  UpdateInputDeviceResponse'
    { arn = Core.Nothing,
      connectionState = Core.Nothing,
      deviceSettingsSyncState = Core.Nothing,
      deviceUpdateStatus = Core.Nothing,
      hdDeviceSettings = Core.Nothing,
      id = Core.Nothing,
      macAddress = Core.Nothing,
      name = Core.Nothing,
      networkSettings = Core.Nothing,
      serialNumber = Core.Nothing,
      type' = Core.Nothing,
      responseStatus
    }

-- | The unique ARN of the input device.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsArn :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsArn = Lens.field @"arn"
{-# DEPRECATED uidrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The state of the connection between the input device and AWS.
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsConnectionState :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.InputDeviceConnectionState)
uidrrsConnectionState = Lens.field @"connectionState"
{-# DEPRECATED uidrrsConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The status of the action to synchronize the device configuration. If you change the configuration of the input device (for example, the maximum bitrate), MediaLive sends the new data to the device. The device might not update itself immediately. SYNCED means the device has updated its configuration. SYNCING means that it has not updated its configuration.
--
-- /Note:/ Consider using 'deviceSettingsSyncState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsDeviceSettingsSyncState :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.DeviceSettingsSyncState)
uidrrsDeviceSettingsSyncState = Lens.field @"deviceSettingsSyncState"
{-# DEPRECATED uidrrsDeviceSettingsSyncState "Use generic-lens or generic-optics with 'deviceSettingsSyncState' instead." #-}

-- | The status of software on the input device.
--
-- /Note:/ Consider using 'deviceUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsDeviceUpdateStatus :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.DeviceUpdateStatus)
uidrrsDeviceUpdateStatus = Lens.field @"deviceUpdateStatus"
{-# DEPRECATED uidrrsDeviceUpdateStatus "Use generic-lens or generic-optics with 'deviceUpdateStatus' instead." #-}

-- | Settings that describe an input device that is type HD.
--
-- /Note:/ Consider using 'hdDeviceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsHdDeviceSettings :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.InputDeviceHdSettings)
uidrrsHdDeviceSettings = Lens.field @"hdDeviceSettings"
{-# DEPRECATED uidrrsHdDeviceSettings "Use generic-lens or generic-optics with 'hdDeviceSettings' instead." #-}

-- | The unique ID of the input device.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsId :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsId = Lens.field @"id"
{-# DEPRECATED uidrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The network MAC address of the input device.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsMacAddress :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsMacAddress = Lens.field @"macAddress"
{-# DEPRECATED uidrrsMacAddress "Use generic-lens or generic-optics with 'macAddress' instead." #-}

-- | A name that you specify for the input device.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsName :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsName = Lens.field @"name"
{-# DEPRECATED uidrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The network settings for the input device.
--
-- /Note:/ Consider using 'networkSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsNetworkSettings :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.InputDeviceNetworkSettings)
uidrrsNetworkSettings = Lens.field @"networkSettings"
{-# DEPRECATED uidrrsNetworkSettings "Use generic-lens or generic-optics with 'networkSettings' instead." #-}

-- | The unique serial number of the input device.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsSerialNumber :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Core.Text)
uidrrsSerialNumber = Lens.field @"serialNumber"
{-# DEPRECATED uidrrsSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The type of the input device.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsType :: Lens.Lens' UpdateInputDeviceResponse (Core.Maybe Types.InputDeviceType)
uidrrsType = Lens.field @"type'"
{-# DEPRECATED uidrrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uidrrsResponseStatus :: Lens.Lens' UpdateInputDeviceResponse Core.Int
uidrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uidrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
