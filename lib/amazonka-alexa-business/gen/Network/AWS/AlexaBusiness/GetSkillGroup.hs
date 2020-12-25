{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets skill group details by skill group ARN.
module Network.AWS.AlexaBusiness.GetSkillGroup
  ( -- * Creating a request
    GetSkillGroup (..),
    mkGetSkillGroup,

    -- ** Request lenses
    gsgSkillGroupArn,

    -- * Destructuring the response
    GetSkillGroupResponse (..),
    mkGetSkillGroupResponse,

    -- ** Response lenses
    gsgrrsSkillGroup,
    gsgrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSkillGroup' smart constructor.
newtype GetSkillGroup = GetSkillGroup'
  { -- | The ARN of the skill group for which to get details. Required.
    skillGroupArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSkillGroup' value with any optional fields omitted.
mkGetSkillGroup ::
  GetSkillGroup
mkGetSkillGroup = GetSkillGroup' {skillGroupArn = Core.Nothing}

-- | The ARN of the skill group for which to get details. Required.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgSkillGroupArn :: Lens.Lens' GetSkillGroup (Core.Maybe Types.Arn)
gsgSkillGroupArn = Lens.field @"skillGroupArn"
{-# DEPRECATED gsgSkillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead." #-}

instance Core.FromJSON GetSkillGroup where
  toJSON GetSkillGroup {..} =
    Core.object
      (Core.catMaybes [("SkillGroupArn" Core..=) Core.<$> skillGroupArn])

instance Core.AWSRequest GetSkillGroup where
  type Rs GetSkillGroup = GetSkillGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.GetSkillGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSkillGroupResponse'
            Core.<$> (x Core..:? "SkillGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSkillGroupResponse' smart constructor.
data GetSkillGroupResponse = GetSkillGroupResponse'
  { -- | The details of the skill group requested. Required.
    skillGroup :: Core.Maybe Types.SkillGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSkillGroupResponse' value with any optional fields omitted.
mkGetSkillGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSkillGroupResponse
mkGetSkillGroupResponse responseStatus =
  GetSkillGroupResponse' {skillGroup = Core.Nothing, responseStatus}

-- | The details of the skill group requested. Required.
--
-- /Note:/ Consider using 'skillGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrrsSkillGroup :: Lens.Lens' GetSkillGroupResponse (Core.Maybe Types.SkillGroup)
gsgrrsSkillGroup = Lens.field @"skillGroup"
{-# DEPRECATED gsgrrsSkillGroup "Use generic-lens or generic-optics with 'skillGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrrsResponseStatus :: Lens.Lens' GetSkillGroupResponse Core.Int
gsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
