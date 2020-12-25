{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ResolveRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the details for the room from which a skill request was invoked. This operation is used by skill developers.
module Network.AWS.AlexaBusiness.ResolveRoom
  ( -- * Creating a request
    ResolveRoom (..),
    mkResolveRoom,

    -- ** Request lenses
    rrUserId,
    rrSkillId,

    -- * Destructuring the response
    ResolveRoomResponse (..),
    mkResolveRoomResponse,

    -- ** Response lenses
    rrrrsRoomArn,
    rrrrsRoomName,
    rrrrsRoomSkillParameters,
    rrrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResolveRoom' smart constructor.
data ResolveRoom = ResolveRoom'
  { -- | The ARN of the user. Required.
    userId :: Types.UserId,
    -- | The ARN of the skill that was requested. Required.
    skillId :: Types.SkillId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResolveRoom' value with any optional fields omitted.
mkResolveRoom ::
  -- | 'userId'
  Types.UserId ->
  -- | 'skillId'
  Types.SkillId ->
  ResolveRoom
mkResolveRoom userId skillId = ResolveRoom' {userId, skillId}

-- | The ARN of the user. Required.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrUserId :: Lens.Lens' ResolveRoom Types.UserId
rrUserId = Lens.field @"userId"
{-# DEPRECATED rrUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The ARN of the skill that was requested. Required.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrSkillId :: Lens.Lens' ResolveRoom Types.SkillId
rrSkillId = Lens.field @"skillId"
{-# DEPRECATED rrSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Core.FromJSON ResolveRoom where
  toJSON ResolveRoom {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserId" Core..= userId),
            Core.Just ("SkillId" Core..= skillId)
          ]
      )

instance Core.AWSRequest ResolveRoom where
  type Rs ResolveRoom = ResolveRoomResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.ResolveRoom")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveRoomResponse'
            Core.<$> (x Core..:? "RoomArn")
            Core.<*> (x Core..:? "RoomName")
            Core.<*> (x Core..:? "RoomSkillParameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkResolveRoomResponse' smart constructor.
data ResolveRoomResponse = ResolveRoomResponse'
  { -- | The ARN of the room from which the skill request was invoked.
    roomArn :: Core.Maybe Types.Arn,
    -- | The name of the room from which the skill request was invoked.
    roomName :: Core.Maybe Types.RoomName,
    -- | Response to get the room profile request. Required.
    roomSkillParameters :: Core.Maybe [Types.RoomSkillParameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResolveRoomResponse' value with any optional fields omitted.
mkResolveRoomResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResolveRoomResponse
mkResolveRoomResponse responseStatus =
  ResolveRoomResponse'
    { roomArn = Core.Nothing,
      roomName = Core.Nothing,
      roomSkillParameters = Core.Nothing,
      responseStatus
    }

-- | The ARN of the room from which the skill request was invoked.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrrsRoomArn :: Lens.Lens' ResolveRoomResponse (Core.Maybe Types.Arn)
rrrrsRoomArn = Lens.field @"roomArn"
{-# DEPRECATED rrrrsRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

-- | The name of the room from which the skill request was invoked.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrrsRoomName :: Lens.Lens' ResolveRoomResponse (Core.Maybe Types.RoomName)
rrrrsRoomName = Lens.field @"roomName"
{-# DEPRECATED rrrrsRoomName "Use generic-lens or generic-optics with 'roomName' instead." #-}

-- | Response to get the room profile request. Required.
--
-- /Note:/ Consider using 'roomSkillParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrrsRoomSkillParameters :: Lens.Lens' ResolveRoomResponse (Core.Maybe [Types.RoomSkillParameter])
rrrrsRoomSkillParameters = Lens.field @"roomSkillParameters"
{-# DEPRECATED rrrrsRoomSkillParameters "Use generic-lens or generic-optics with 'roomSkillParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrrrsResponseStatus :: Lens.Lens' ResolveRoomResponse Core.Int
rrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
