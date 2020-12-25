{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a device from its current room. The device continues to be connected to the Wi-Fi network and is still registered to the account. The device settings and skills are removed from the room.
module Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
  ( -- * Creating a request
    DisassociateDeviceFromRoom (..),
    mkDisassociateDeviceFromRoom,

    -- ** Request lenses
    ddfrDeviceArn,

    -- * Destructuring the response
    DisassociateDeviceFromRoomResponse (..),
    mkDisassociateDeviceFromRoomResponse,

    -- ** Response lenses
    ddfrrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateDeviceFromRoom' smart constructor.
newtype DisassociateDeviceFromRoom = DisassociateDeviceFromRoom'
  { -- | The ARN of the device to disassociate from a room. Required.
    deviceArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDeviceFromRoom' value with any optional fields omitted.
mkDisassociateDeviceFromRoom ::
  DisassociateDeviceFromRoom
mkDisassociateDeviceFromRoom =
  DisassociateDeviceFromRoom' {deviceArn = Core.Nothing}

-- | The ARN of the device to disassociate from a room. Required.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrDeviceArn :: Lens.Lens' DisassociateDeviceFromRoom (Core.Maybe Types.Arn)
ddfrDeviceArn = Lens.field @"deviceArn"
{-# DEPRECATED ddfrDeviceArn "Use generic-lens or generic-optics with 'deviceArn' instead." #-}

instance Core.FromJSON DisassociateDeviceFromRoom where
  toJSON DisassociateDeviceFromRoom {..} =
    Core.object
      (Core.catMaybes [("DeviceArn" Core..=) Core.<$> deviceArn])

instance Core.AWSRequest DisassociateDeviceFromRoom where
  type
    Rs DisassociateDeviceFromRoom =
      DisassociateDeviceFromRoomResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.DisassociateDeviceFromRoom")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDeviceFromRoomResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateDeviceFromRoomResponse' smart constructor.
newtype DisassociateDeviceFromRoomResponse = DisassociateDeviceFromRoomResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDeviceFromRoomResponse' value with any optional fields omitted.
mkDisassociateDeviceFromRoomResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateDeviceFromRoomResponse
mkDisassociateDeviceFromRoomResponse responseStatus =
  DisassociateDeviceFromRoomResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfrrrsResponseStatus :: Lens.Lens' DisassociateDeviceFromRoomResponse Core.Int
ddfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
