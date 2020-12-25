{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates skill group details by skill group ARN.
module Network.AWS.AlexaBusiness.UpdateSkillGroup
  ( -- * Creating a request
    UpdateSkillGroup (..),
    mkUpdateSkillGroup,

    -- ** Request lenses
    usgDescription,
    usgSkillGroupArn,
    usgSkillGroupName,

    -- * Destructuring the response
    UpdateSkillGroupResponse (..),
    mkUpdateSkillGroupResponse,

    -- ** Response lenses
    usgrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSkillGroup' smart constructor.
data UpdateSkillGroup = UpdateSkillGroup'
  { -- | The updated description for the skill group.
    description :: Core.Maybe Types.SkillGroupDescription,
    -- | The ARN of the skill group to update.
    skillGroupArn :: Core.Maybe Types.Arn,
    -- | The updated name for the skill group.
    skillGroupName :: Core.Maybe Types.SkillGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSkillGroup' value with any optional fields omitted.
mkUpdateSkillGroup ::
  UpdateSkillGroup
mkUpdateSkillGroup =
  UpdateSkillGroup'
    { description = Core.Nothing,
      skillGroupArn = Core.Nothing,
      skillGroupName = Core.Nothing
    }

-- | The updated description for the skill group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgDescription :: Lens.Lens' UpdateSkillGroup (Core.Maybe Types.SkillGroupDescription)
usgDescription = Lens.field @"description"
{-# DEPRECATED usgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the skill group to update.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSkillGroupArn :: Lens.Lens' UpdateSkillGroup (Core.Maybe Types.Arn)
usgSkillGroupArn = Lens.field @"skillGroupArn"
{-# DEPRECATED usgSkillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead." #-}

-- | The updated name for the skill group.
--
-- /Note:/ Consider using 'skillGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSkillGroupName :: Lens.Lens' UpdateSkillGroup (Core.Maybe Types.SkillGroupName)
usgSkillGroupName = Lens.field @"skillGroupName"
{-# DEPRECATED usgSkillGroupName "Use generic-lens or generic-optics with 'skillGroupName' instead." #-}

instance Core.FromJSON UpdateSkillGroup where
  toJSON UpdateSkillGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            ("SkillGroupArn" Core..=) Core.<$> skillGroupArn,
            ("SkillGroupName" Core..=) Core.<$> skillGroupName
          ]
      )

instance Core.AWSRequest UpdateSkillGroup where
  type Rs UpdateSkillGroup = UpdateSkillGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateSkillGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSkillGroupResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSkillGroupResponse' smart constructor.
newtype UpdateSkillGroupResponse = UpdateSkillGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSkillGroupResponse' value with any optional fields omitted.
mkUpdateSkillGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSkillGroupResponse
mkUpdateSkillGroupResponse responseStatus =
  UpdateSkillGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrrsResponseStatus :: Lens.Lens' UpdateSkillGroupResponse Core.Int
usgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
