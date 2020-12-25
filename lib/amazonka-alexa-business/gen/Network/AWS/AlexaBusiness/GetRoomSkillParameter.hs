{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetRoomSkillParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets room skill parameter details by room, skill, and parameter key ARN.
module Network.AWS.AlexaBusiness.GetRoomSkillParameter
  ( -- * Creating a request
    GetRoomSkillParameter (..),
    mkGetRoomSkillParameter,

    -- ** Request lenses
    grspSkillId,
    grspParameterKey,
    grspRoomArn,

    -- * Destructuring the response
    GetRoomSkillParameterResponse (..),
    mkGetRoomSkillParameterResponse,

    -- ** Response lenses
    grsprrsRoomSkillParameter,
    grsprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRoomSkillParameter' smart constructor.
data GetRoomSkillParameter = GetRoomSkillParameter'
  { -- | The ARN of the skill from which to get the room skill parameter details. Required.
    skillId :: Types.SkillId,
    -- | The room skill parameter key for which to get details. Required.
    parameterKey :: Types.ParameterKey,
    -- | The ARN of the room from which to get the room skill parameter details.
    roomArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRoomSkillParameter' value with any optional fields omitted.
mkGetRoomSkillParameter ::
  -- | 'skillId'
  Types.SkillId ->
  -- | 'parameterKey'
  Types.ParameterKey ->
  GetRoomSkillParameter
mkGetRoomSkillParameter skillId parameterKey =
  GetRoomSkillParameter'
    { skillId,
      parameterKey,
      roomArn = Core.Nothing
    }

-- | The ARN of the skill from which to get the room skill parameter details. Required.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grspSkillId :: Lens.Lens' GetRoomSkillParameter Types.SkillId
grspSkillId = Lens.field @"skillId"
{-# DEPRECATED grspSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

-- | The room skill parameter key for which to get details. Required.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grspParameterKey :: Lens.Lens' GetRoomSkillParameter Types.ParameterKey
grspParameterKey = Lens.field @"parameterKey"
{-# DEPRECATED grspParameterKey "Use generic-lens or generic-optics with 'parameterKey' instead." #-}

-- | The ARN of the room from which to get the room skill parameter details.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grspRoomArn :: Lens.Lens' GetRoomSkillParameter (Core.Maybe Types.Arn)
grspRoomArn = Lens.field @"roomArn"
{-# DEPRECATED grspRoomArn "Use generic-lens or generic-optics with 'roomArn' instead." #-}

instance Core.FromJSON GetRoomSkillParameter where
  toJSON GetRoomSkillParameter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SkillId" Core..= skillId),
            Core.Just ("ParameterKey" Core..= parameterKey),
            ("RoomArn" Core..=) Core.<$> roomArn
          ]
      )

instance Core.AWSRequest GetRoomSkillParameter where
  type Rs GetRoomSkillParameter = GetRoomSkillParameterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.GetRoomSkillParameter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoomSkillParameterResponse'
            Core.<$> (x Core..:? "RoomSkillParameter")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRoomSkillParameterResponse' smart constructor.
data GetRoomSkillParameterResponse = GetRoomSkillParameterResponse'
  { -- | The details of the room skill parameter requested. Required.
    roomSkillParameter :: Core.Maybe Types.RoomSkillParameter,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRoomSkillParameterResponse' value with any optional fields omitted.
mkGetRoomSkillParameterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRoomSkillParameterResponse
mkGetRoomSkillParameterResponse responseStatus =
  GetRoomSkillParameterResponse'
    { roomSkillParameter = Core.Nothing,
      responseStatus
    }

-- | The details of the room skill parameter requested. Required.
--
-- /Note:/ Consider using 'roomSkillParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsprrsRoomSkillParameter :: Lens.Lens' GetRoomSkillParameterResponse (Core.Maybe Types.RoomSkillParameter)
grsprrsRoomSkillParameter = Lens.field @"roomSkillParameter"
{-# DEPRECATED grsprrsRoomSkillParameter "Use generic-lens or generic-optics with 'roomSkillParameter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsprrsResponseStatus :: Lens.Lens' GetRoomSkillParameterResponse Core.Int
grsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
