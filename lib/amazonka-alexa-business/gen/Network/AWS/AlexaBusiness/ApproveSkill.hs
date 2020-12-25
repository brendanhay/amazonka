{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ApproveSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with the organization under the customer's AWS account. If a skill is private, the user implicitly accepts access to this skill during enablement.
module Network.AWS.AlexaBusiness.ApproveSkill
  ( -- * Creating a request
    ApproveSkill (..),
    mkApproveSkill,

    -- ** Request lenses
    asSkillId,

    -- * Destructuring the response
    ApproveSkillResponse (..),
    mkApproveSkillResponse,

    -- ** Response lenses
    asrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkApproveSkill' smart constructor.
newtype ApproveSkill = ApproveSkill'
  { -- | The unique identifier of the skill.
    skillId :: Types.SkillId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ApproveSkill' value with any optional fields omitted.
mkApproveSkill ::
  -- | 'skillId'
  Types.SkillId ->
  ApproveSkill
mkApproveSkill skillId = ApproveSkill' {skillId}

-- | The unique identifier of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSkillId :: Lens.Lens' ApproveSkill Types.SkillId
asSkillId = Lens.field @"skillId"
{-# DEPRECATED asSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Core.FromJSON ApproveSkill where
  toJSON ApproveSkill {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SkillId" Core..= skillId)])

instance Core.AWSRequest ApproveSkill where
  type Rs ApproveSkill = ApproveSkillResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.ApproveSkill")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ApproveSkillResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkApproveSkillResponse' smart constructor.
newtype ApproveSkillResponse = ApproveSkillResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ApproveSkillResponse' value with any optional fields omitted.
mkApproveSkillResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ApproveSkillResponse
mkApproveSkillResponse responseStatus =
  ApproveSkillResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrrsResponseStatus :: Lens.Lens' ApproveSkillResponse Core.Int
asrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED asrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
