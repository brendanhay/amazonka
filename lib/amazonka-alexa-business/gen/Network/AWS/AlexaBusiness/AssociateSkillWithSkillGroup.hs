{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with a skill group.
module Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
  ( -- * Creating a request
    AssociateSkillWithSkillGroup (..),
    mkAssociateSkillWithSkillGroup,

    -- ** Request lenses
    aswsgSkillId,
    aswsgSkillGroupArn,

    -- * Destructuring the response
    AssociateSkillWithSkillGroupResponse (..),
    mkAssociateSkillWithSkillGroupResponse,

    -- ** Response lenses
    aswsgrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateSkillWithSkillGroup' smart constructor.
data AssociateSkillWithSkillGroup = AssociateSkillWithSkillGroup'
  { -- | The unique identifier of the skill.
    skillId :: Types.SkillId,
    -- | The ARN of the skill group to associate the skill to. Required.
    skillGroupArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSkillWithSkillGroup' value with any optional fields omitted.
mkAssociateSkillWithSkillGroup ::
  -- | 'skillId'
  Types.SkillId ->
  AssociateSkillWithSkillGroup
mkAssociateSkillWithSkillGroup skillId =
  AssociateSkillWithSkillGroup'
    { skillId,
      skillGroupArn = Core.Nothing
    }

-- | The unique identifier of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgSkillId :: Lens.Lens' AssociateSkillWithSkillGroup Types.SkillId
aswsgSkillId = Lens.field @"skillId"
{-# DEPRECATED aswsgSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

-- | The ARN of the skill group to associate the skill to. Required.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgSkillGroupArn :: Lens.Lens' AssociateSkillWithSkillGroup (Core.Maybe Types.Arn)
aswsgSkillGroupArn = Lens.field @"skillGroupArn"
{-# DEPRECATED aswsgSkillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead." #-}

instance Core.FromJSON AssociateSkillWithSkillGroup where
  toJSON AssociateSkillWithSkillGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SkillId" Core..= skillId),
            ("SkillGroupArn" Core..=) Core.<$> skillGroupArn
          ]
      )

instance Core.AWSRequest AssociateSkillWithSkillGroup where
  type
    Rs AssociateSkillWithSkillGroup =
      AssociateSkillWithSkillGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.AssociateSkillWithSkillGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillWithSkillGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateSkillWithSkillGroupResponse' smart constructor.
newtype AssociateSkillWithSkillGroupResponse = AssociateSkillWithSkillGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSkillWithSkillGroupResponse' value with any optional fields omitted.
mkAssociateSkillWithSkillGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateSkillWithSkillGroupResponse
mkAssociateSkillWithSkillGroupResponse responseStatus =
  AssociateSkillWithSkillGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgrrsResponseStatus :: Lens.Lens' AssociateSkillWithSkillGroupResponse Core.Int
aswsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aswsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
