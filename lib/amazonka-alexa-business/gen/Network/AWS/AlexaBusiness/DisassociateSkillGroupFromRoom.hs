{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill group from a specified room. This disables all skills in the skill group on all devices in the room.
module Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
  ( -- * Creating a request
    DisassociateSkillGroupFromRoom (..),
    mkDisassociateSkillGroupFromRoom,

    -- ** Request lenses
    dsgfrRoomArn,
    dsgfrSkillGroupArn,

    -- * Destructuring the response
    DisassociateSkillGroupFromRoomResponse (..),
    mkDisassociateSkillGroupFromRoomResponse,

    -- ** Response lenses
    dsgfrrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateSkillGroupFromRoom' smart constructor.
data DisassociateSkillGroupFromRoom = DisassociateSkillGroupFromRoom'
  { -- | The ARN of the room from which the skill group is to be disassociated. Required.
    roomArn :: Core.Maybe Types.Arn,
    -- | The ARN of the skill group to disassociate from a room. Required.
    skillGroupArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSkillGroupFromRoom' value with any optional fields omitted.
mkDisassociateSkillGroupFromRoom ::
  DisassociateSkillGroupFromRoom
mkDisassociateSkillGroupFromRoom =
  DisassociateSkillGroupFromRoom'
    { roomArn = Core.Nothing,
      skillGroupArn = Core.Nothing
    }

-- | The ARN of the room from which the skill group is to be disassociated. Required.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgfrRoomArn :: Lens.Lens' DisassociateSkillGroupFromRoom (Core.Maybe Types.Arn)
dsgfrRoomArn = Lens.field @"roomArn"
{-# DEPRECATED dsgfrRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

-- | The ARN of the skill group to disassociate from a room. Required.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgfrSkillGroupArn :: Lens.Lens' DisassociateSkillGroupFromRoom (Core.Maybe Types.Arn)
dsgfrSkillGroupArn = Lens.field @"skillGroupArn"
{-# DEPRECATED dsgfrSkillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead." #-}

instance Core.FromJSON DisassociateSkillGroupFromRoom where
  toJSON DisassociateSkillGroupFromRoom {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            ("SkillGroupArn" Core..=) Core.<$> skillGroupArn
          ]
      )

instance Core.AWSRequest DisassociateSkillGroupFromRoom where
  type
    Rs DisassociateSkillGroupFromRoom =
      DisassociateSkillGroupFromRoomResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.DisassociateSkillGroupFromRoom")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillGroupFromRoomResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateSkillGroupFromRoomResponse' smart constructor.
newtype DisassociateSkillGroupFromRoomResponse = DisassociateSkillGroupFromRoomResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSkillGroupFromRoomResponse' value with any optional fields omitted.
mkDisassociateSkillGroupFromRoomResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateSkillGroupFromRoomResponse
mkDisassociateSkillGroupFromRoomResponse responseStatus =
  DisassociateSkillGroupFromRoomResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgfrrrsResponseStatus :: Lens.Lens' DisassociateSkillGroupFromRoomResponse Core.Int
dsgfrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsgfrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
