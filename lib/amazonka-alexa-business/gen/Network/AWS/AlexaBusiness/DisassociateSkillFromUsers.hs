{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill unavailable for enrolled users and prevents them from enabling it on their devices.
module Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
  ( -- * Creating a request
    DisassociateSkillFromUsers (..),
    mkDisassociateSkillFromUsers,

    -- ** Request lenses
    dsfuSkillId,

    -- * Destructuring the response
    DisassociateSkillFromUsersResponse (..),
    mkDisassociateSkillFromUsersResponse,

    -- ** Response lenses
    dsfurrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateSkillFromUsers' smart constructor.
newtype DisassociateSkillFromUsers = DisassociateSkillFromUsers'
  { -- | The private skill ID you want to make unavailable for enrolled users.
    skillId :: Types.SkillId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSkillFromUsers' value with any optional fields omitted.
mkDisassociateSkillFromUsers ::
  -- | 'skillId'
  Types.SkillId ->
  DisassociateSkillFromUsers
mkDisassociateSkillFromUsers skillId =
  DisassociateSkillFromUsers' {skillId}

-- | The private skill ID you want to make unavailable for enrolled users.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfuSkillId :: Lens.Lens' DisassociateSkillFromUsers Types.SkillId
dsfuSkillId = Lens.field @"skillId"
{-# DEPRECATED dsfuSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Core.FromJSON DisassociateSkillFromUsers where
  toJSON DisassociateSkillFromUsers {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SkillId" Core..= skillId)])

instance Core.AWSRequest DisassociateSkillFromUsers where
  type
    Rs DisassociateSkillFromUsers =
      DisassociateSkillFromUsersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.DisassociateSkillFromUsers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillFromUsersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateSkillFromUsersResponse' smart constructor.
newtype DisassociateSkillFromUsersResponse = DisassociateSkillFromUsersResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSkillFromUsersResponse' value with any optional fields omitted.
mkDisassociateSkillFromUsersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateSkillFromUsersResponse
mkDisassociateSkillFromUsersResponse responseStatus =
  DisassociateSkillFromUsersResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfurrsResponseStatus :: Lens.Lens' DisassociateSkillFromUsersResponse Core.Int
dsfurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsfurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
