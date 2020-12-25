{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a skill group by skill group ARN.
module Network.AWS.AlexaBusiness.DeleteSkillGroup
  ( -- * Creating a request
    DeleteSkillGroup (..),
    mkDeleteSkillGroup,

    -- ** Request lenses
    dsgSkillGroupArn,

    -- * Destructuring the response
    DeleteSkillGroupResponse (..),
    mkDeleteSkillGroupResponse,

    -- ** Response lenses
    dsgrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSkillGroup' smart constructor.
newtype DeleteSkillGroup = DeleteSkillGroup'
  { -- | The ARN of the skill group to delete. Required.
    skillGroupArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSkillGroup' value with any optional fields omitted.
mkDeleteSkillGroup ::
  DeleteSkillGroup
mkDeleteSkillGroup =
  DeleteSkillGroup' {skillGroupArn = Core.Nothing}

-- | The ARN of the skill group to delete. Required.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSkillGroupArn :: Lens.Lens' DeleteSkillGroup (Core.Maybe Types.Arn)
dsgSkillGroupArn = Lens.field @"skillGroupArn"
{-# DEPRECATED dsgSkillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead." #-}

instance Core.FromJSON DeleteSkillGroup where
  toJSON DeleteSkillGroup {..} =
    Core.object
      (Core.catMaybes [("SkillGroupArn" Core..=) Core.<$> skillGroupArn])

instance Core.AWSRequest DeleteSkillGroup where
  type Rs DeleteSkillGroup = DeleteSkillGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteSkillGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSkillGroupResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteSkillGroupResponse' smart constructor.
newtype DeleteSkillGroupResponse = DeleteSkillGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSkillGroupResponse' value with any optional fields omitted.
mkDeleteSkillGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSkillGroupResponse
mkDeleteSkillGroupResponse responseStatus =
  DeleteSkillGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsResponseStatus :: Lens.Lens' DeleteSkillGroupResponse Core.Int
dsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
