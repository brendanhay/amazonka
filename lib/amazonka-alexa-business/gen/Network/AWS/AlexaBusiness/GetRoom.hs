{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets room details by room ARN.
module Network.AWS.AlexaBusiness.GetRoom
  ( -- * Creating a request
    GetRoom (..),
    mkGetRoom,

    -- ** Request lenses
    grRoomArn,

    -- * Destructuring the response
    GetRoomResponse (..),
    mkGetRoomResponse,

    -- ** Response lenses
    grrrsRoom,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRoom' smart constructor.
newtype GetRoom = GetRoom'
  { -- | The ARN of the room for which to request details. Required.
    roomArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRoom' value with any optional fields omitted.
mkGetRoom ::
  GetRoom
mkGetRoom = GetRoom' {roomArn = Core.Nothing}

-- | The ARN of the room for which to request details. Required.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRoomArn :: Lens.Lens' GetRoom (Core.Maybe Types.Arn)
grRoomArn = Lens.field @"roomArn"
{-# DEPRECATED grRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

instance Core.FromJSON GetRoom where
  toJSON GetRoom {..} =
    Core.object
      (Core.catMaybes [("RoomArn" Core..=) Core.<$> roomArn])

instance Core.AWSRequest GetRoom where
  type Rs GetRoom = GetRoomResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.GetRoom")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoomResponse'
            Core.<$> (x Core..:? "Room") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRoomResponse' smart constructor.
data GetRoomResponse = GetRoomResponse'
  { -- | The details of the room requested.
    room :: Core.Maybe Types.Room,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRoomResponse' value with any optional fields omitted.
mkGetRoomResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRoomResponse
mkGetRoomResponse responseStatus =
  GetRoomResponse' {room = Core.Nothing, responseStatus}

-- | The details of the room requested.
--
-- /Note:/ Consider using 'room' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRoom :: Lens.Lens' GetRoomResponse (Core.Maybe Types.Room)
grrrsRoom = Lens.field @"room"
{-# DEPRECATED grrrsRoom "Use generic-lens or generic-optics with 'room' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRoomResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
