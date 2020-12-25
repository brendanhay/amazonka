{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a device from Alexa For Business.
module Network.AWS.AlexaBusiness.DeleteDevice
  ( -- * Creating a request
    DeleteDevice (..),
    mkDeleteDevice,

    -- ** Request lenses
    ddfDeviceArn,

    -- * Destructuring the response
    DeleteDeviceResponse (..),
    mkDeleteDeviceResponse,

    -- ** Response lenses
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDevice' smart constructor.
newtype DeleteDevice = DeleteDevice'
  { -- | The ARN of the device for which to request details.
    deviceArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDevice' value with any optional fields omitted.
mkDeleteDevice ::
  -- | 'deviceArn'
  Types.Arn ->
  DeleteDevice
mkDeleteDevice deviceArn = DeleteDevice' {deviceArn}

-- | The ARN of the device for which to request details.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDeviceArn :: Lens.Lens' DeleteDevice Types.Arn
ddfDeviceArn = Lens.field @"deviceArn"
{-# DEPRECATED ddfDeviceArn "Use generic-lens or generic-optics with 'deviceArn' instead." #-}

instance Core.FromJSON DeleteDevice where
  toJSON DeleteDevice {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DeviceArn" Core..= deviceArn)])

instance Core.AWSRequest DeleteDevice where
  type Rs DeleteDevice = DeleteDeviceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteDevice")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeviceResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDeviceResponse' smart constructor.
newtype DeleteDeviceResponse = DeleteDeviceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeviceResponse' value with any optional fields omitted.
mkDeleteDeviceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDeviceResponse
mkDeleteDeviceResponse responseStatus =
  DeleteDeviceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DeleteDeviceResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
