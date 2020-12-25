{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.RejectSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from the organization under a user's AWS account. If the skill is a private skill, it moves to an AcceptStatus of PENDING. Any private or public skill that is rejected can be added later by calling the ApproveSkill API.
module Network.AWS.AlexaBusiness.RejectSkill
  ( -- * Creating a request
    RejectSkill (..),
    mkRejectSkill,

    -- ** Request lenses
    rsSkillId,

    -- * Destructuring the response
    RejectSkillResponse (..),
    mkRejectSkillResponse,

    -- ** Response lenses
    rsrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectSkill' smart constructor.
newtype RejectSkill = RejectSkill'
  { -- | The unique identifier of the skill.
    skillId :: Types.SkillId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectSkill' value with any optional fields omitted.
mkRejectSkill ::
  -- | 'skillId'
  Types.SkillId ->
  RejectSkill
mkRejectSkill skillId = RejectSkill' {skillId}

-- | The unique identifier of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSkillId :: Lens.Lens' RejectSkill Types.SkillId
rsSkillId = Lens.field @"skillId"
{-# DEPRECATED rsSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Core.FromJSON RejectSkill where
  toJSON RejectSkill {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SkillId" Core..= skillId)])

instance Core.AWSRequest RejectSkill where
  type Rs RejectSkill = RejectSkillResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.RejectSkill")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectSkillResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRejectSkillResponse' smart constructor.
newtype RejectSkillResponse = RejectSkillResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectSkillResponse' value with any optional fields omitted.
mkRejectSkillResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RejectSkillResponse
mkRejectSkillResponse responseStatus =
  RejectSkillResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsResponseStatus :: Lens.Lens' RejectSkillResponse Core.Int
rsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
