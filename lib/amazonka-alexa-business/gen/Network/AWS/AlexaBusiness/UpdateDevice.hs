{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device name by device ARN.
module Network.AWS.AlexaBusiness.UpdateDevice
  ( -- * Creating a request
    UpdateDevice (..),
    mkUpdateDevice,

    -- ** Request lenses
    udDeviceArn,
    udDeviceName,

    -- * Destructuring the response
    UpdateDeviceResponse (..),
    mkUpdateDeviceResponse,

    -- ** Response lenses
    udrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDevice' smart constructor.
data UpdateDevice = UpdateDevice'
  { -- | The ARN of the device to update. Required.
    deviceArn :: Core.Maybe Types.Arn,
    -- | The updated device name. Required.
    deviceName :: Core.Maybe Types.DeviceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDevice' value with any optional fields omitted.
mkUpdateDevice ::
  UpdateDevice
mkUpdateDevice =
  UpdateDevice'
    { deviceArn = Core.Nothing,
      deviceName = Core.Nothing
    }

-- | The ARN of the device to update. Required.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDeviceArn :: Lens.Lens' UpdateDevice (Core.Maybe Types.Arn)
udDeviceArn = Lens.field @"deviceArn"
{-# DEPRECATED udDeviceArn "Use generic-lens or generic-optics with 'deviceArn' instead." #-}

-- | The updated device name. Required.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDeviceName :: Lens.Lens' UpdateDevice (Core.Maybe Types.DeviceName)
udDeviceName = Lens.field @"deviceName"
{-# DEPRECATED udDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Core.FromJSON UpdateDevice where
  toJSON UpdateDevice {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceArn" Core..=) Core.<$> deviceArn,
            ("DeviceName" Core..=) Core.<$> deviceName
          ]
      )

instance Core.AWSRequest UpdateDevice where
  type Rs UpdateDevice = UpdateDeviceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateDevice")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDeviceResponse' smart constructor.
newtype UpdateDeviceResponse = UpdateDeviceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceResponse' value with any optional fields omitted.
mkUpdateDeviceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDeviceResponse
mkUpdateDeviceResponse responseStatus =
  UpdateDeviceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDeviceResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
