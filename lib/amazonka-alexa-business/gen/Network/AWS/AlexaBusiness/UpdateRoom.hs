{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room details by room ARN.
module Network.AWS.AlexaBusiness.UpdateRoom
  ( -- * Creating a request
    UpdateRoom (..),
    mkUpdateRoom,

    -- ** Request lenses
    urDescription,
    urProfileArn,
    urProviderCalendarId,
    urRoomArn,
    urRoomName,

    -- * Destructuring the response
    UpdateRoomResponse (..),
    mkUpdateRoomResponse,

    -- ** Response lenses
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRoom' smart constructor.
data UpdateRoom = UpdateRoom'
  { -- | The updated description for the room.
    description :: Core.Maybe Types.Description,
    -- | The updated profile ARN for the room.
    profileArn :: Core.Maybe Types.ProfileArn,
    -- | The updated provider calendar ARN for the room.
    providerCalendarId :: Core.Maybe Types.ProviderCalendarId,
    -- | The ARN of the room to update.
    roomArn :: Core.Maybe Types.RoomArn,
    -- | The updated name for the room.
    roomName :: Core.Maybe Types.RoomName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoom' value with any optional fields omitted.
mkUpdateRoom ::
  UpdateRoom
mkUpdateRoom =
  UpdateRoom'
    { description = Core.Nothing,
      profileArn = Core.Nothing,
      providerCalendarId = Core.Nothing,
      roomArn = Core.Nothing,
      roomName = Core.Nothing
    }

-- | The updated description for the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDescription :: Lens.Lens' UpdateRoom (Core.Maybe Types.Description)
urDescription = Lens.field @"description"
{-# DEPRECATED urDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated profile ARN for the room.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urProfileArn :: Lens.Lens' UpdateRoom (Core.Maybe Types.ProfileArn)
urProfileArn = Lens.field @"profileArn"
{-# DEPRECATED urProfileArn "Use generic-lens or generic-optics with 'profileArn' instead." #-}

-- | The updated provider calendar ARN for the room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urProviderCalendarId :: Lens.Lens' UpdateRoom (Core.Maybe Types.ProviderCalendarId)
urProviderCalendarId = Lens.field @"providerCalendarId"
{-# DEPRECATED urProviderCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead." #-}

-- | The ARN of the room to update.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRoomArn :: Lens.Lens' UpdateRoom (Core.Maybe Types.RoomArn)
urRoomArn = Lens.field @"roomArn"
{-# DEPRECATED urRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

-- | The updated name for the room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRoomName :: Lens.Lens' UpdateRoom (Core.Maybe Types.RoomName)
urRoomName = Lens.field @"roomName"
{-# DEPRECATED urRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

instance Core.FromJSON UpdateRoom where
  toJSON UpdateRoom {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            ("ProfileArn" Core..=) Core.<$> profileArn,
            ("ProviderCalendarId" Core..=) Core.<$> providerCalendarId,
            ("RoomArn" Core..=) Core.<$> roomArn,
            ("RoomName" Core..=) Core.<$> roomName
          ]
      )

instance Core.AWSRequest UpdateRoom where
  type Rs UpdateRoom = UpdateRoomResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateRoom")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRoomResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateRoomResponse' smart constructor.
newtype UpdateRoomResponse = UpdateRoomResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoomResponse' value with any optional fields omitted.
mkUpdateRoomResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRoomResponse
mkUpdateRoomResponse responseStatus =
  UpdateRoomResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateRoomResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
