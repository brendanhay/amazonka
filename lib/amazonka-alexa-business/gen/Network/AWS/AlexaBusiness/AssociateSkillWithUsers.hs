{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill available for enrolled users to enable on their devices.
module Network.AWS.AlexaBusiness.AssociateSkillWithUsers
  ( -- * Creating a request
    AssociateSkillWithUsers (..),
    mkAssociateSkillWithUsers,

    -- ** Request lenses
    aswuSkillId,

    -- * Destructuring the response
    AssociateSkillWithUsersResponse (..),
    mkAssociateSkillWithUsersResponse,

    -- ** Response lenses
    aswurrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateSkillWithUsers' smart constructor.
newtype AssociateSkillWithUsers = AssociateSkillWithUsers'
  { -- | The private skill ID you want to make available to enrolled users.
    skillId :: Types.SkillId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSkillWithUsers' value with any optional fields omitted.
mkAssociateSkillWithUsers ::
  -- | 'skillId'
  Types.SkillId ->
  AssociateSkillWithUsers
mkAssociateSkillWithUsers skillId =
  AssociateSkillWithUsers' {skillId}

-- | The private skill ID you want to make available to enrolled users.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswuSkillId :: Lens.Lens' AssociateSkillWithUsers Types.SkillId
aswuSkillId = Lens.field @"skillId"
{-# DEPRECATED aswuSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Core.FromJSON AssociateSkillWithUsers where
  toJSON AssociateSkillWithUsers {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SkillId" Core..= skillId)])

instance Core.AWSRequest AssociateSkillWithUsers where
  type Rs AssociateSkillWithUsers = AssociateSkillWithUsersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.AssociateSkillWithUsers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillWithUsersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateSkillWithUsersResponse' smart constructor.
newtype AssociateSkillWithUsersResponse = AssociateSkillWithUsersResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSkillWithUsersResponse' value with any optional fields omitted.
mkAssociateSkillWithUsersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateSkillWithUsersResponse
mkAssociateSkillWithUsersResponse responseStatus =
  AssociateSkillWithUsersResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswurrsResponseStatus :: Lens.Lens' AssociateSkillWithUsersResponse Core.Int
aswurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aswurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
