{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a room with the specified details.
module Network.AWS.AlexaBusiness.CreateRoom
  ( -- * Creating a request
    CreateRoom (..),
    mkCreateRoom,

    -- ** Request lenses
    crRoomName,
    crClientRequestToken,
    crDescription,
    crProfileArn,
    crProviderCalendarId,
    crTags,

    -- * Destructuring the response
    CreateRoomResponse (..),
    mkCreateRoomResponse,

    -- ** Response lenses
    crrrsRoomArn,
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRoom' smart constructor.
data CreateRoom = CreateRoom'
  { -- | The name for the room.
    roomName :: Types.RoomName,
    -- | A unique, user-specified identifier for this request that ensures idempotency.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | The description for the room.
    description :: Core.Maybe Types.Description,
    -- | The profile ARN for the room. This is required.
    profileArn :: Core.Maybe Types.Arn,
    -- | The calendar ARN for the room.
    providerCalendarId :: Core.Maybe Types.ProviderCalendarId,
    -- | The tags for the room.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoom' value with any optional fields omitted.
mkCreateRoom ::
  -- | 'roomName'
  Types.RoomName ->
  CreateRoom
mkCreateRoom roomName =
  CreateRoom'
    { roomName,
      clientRequestToken = Core.Nothing,
      description = Core.Nothing,
      profileArn = Core.Nothing,
      providerCalendarId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name for the room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRoomName :: Lens.Lens' CreateRoom Types.RoomName
crRoomName = Lens.field @"roomName"
{-# DEPRECATED crRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

-- | A unique, user-specified identifier for this request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crClientRequestToken :: Lens.Lens' CreateRoom (Core.Maybe Types.ClientRequestToken)
crClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED crClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The description for the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' CreateRoom (Core.Maybe Types.Description)
crDescription = Lens.field @"description"
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The profile ARN for the room. This is required.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crProfileArn :: Lens.Lens' CreateRoom (Core.Maybe Types.Arn)
crProfileArn = Lens.field @"profileArn"
{-# DEPRECATED crProfileArn "Use generic-lens or generic-optics with 'profileArn' instead." #-}

-- | The calendar ARN for the room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crProviderCalendarId :: Lens.Lens' CreateRoom (Core.Maybe Types.ProviderCalendarId)
crProviderCalendarId = Lens.field @"providerCalendarId"
{-# DEPRECATED crProviderCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead." #-}

-- | The tags for the room.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRoom (Core.Maybe [Types.Tag])
crTags = Lens.field @"tags"
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateRoom where
  toJSON CreateRoom {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RoomName" Core..= roomName),
            ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("Description" Core..=) Core.<$> description,
            ("ProfileArn" Core..=) Core.<$> profileArn,
            ("ProviderCalendarId" Core..=) Core.<$> providerCalendarId,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateRoom where
  type Rs CreateRoom = CreateRoomResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateRoom")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoomResponse'
            Core.<$> (x Core..:? "RoomArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRoomResponse' smart constructor.
data CreateRoomResponse = CreateRoomResponse'
  { -- | The ARN of the newly created room in the response.
    roomArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoomResponse' value with any optional fields omitted.
mkCreateRoomResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRoomResponse
mkCreateRoomResponse responseStatus =
  CreateRoomResponse' {roomArn = Core.Nothing, responseStatus}

-- | The ARN of the newly created room in the response.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRoomArn :: Lens.Lens' CreateRoomResponse (Core.Maybe Types.Arn)
crrrsRoomArn = Lens.field @"roomArn"
{-# DEPRECATED crrrsRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRoomResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
