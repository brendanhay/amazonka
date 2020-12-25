{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetInvitationsCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the count of all GuardDuty membership invitations that were sent to the current member account except the currently accepted invitation.
module Network.AWS.GuardDuty.GetInvitationsCount
  ( -- * Creating a request
    GetInvitationsCount (..),
    mkGetInvitationsCount,

    -- * Destructuring the response
    GetInvitationsCountResponse (..),
    mkGetInvitationsCountResponse,

    -- ** Response lenses
    gicrrsInvitationsCount,
    gicrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInvitationsCount' smart constructor.
data GetInvitationsCount = GetInvitationsCount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInvitationsCount' value with any optional fields omitted.
mkGetInvitationsCount ::
  GetInvitationsCount
mkGetInvitationsCount = GetInvitationsCount'

instance Core.AWSRequest GetInvitationsCount where
  type Rs GetInvitationsCount = GetInvitationsCountResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/invitation/count",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInvitationsCountResponse'
            Core.<$> (x Core..:? "invitationsCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInvitationsCountResponse' smart constructor.
data GetInvitationsCountResponse = GetInvitationsCountResponse'
  { -- | The number of received invitations.
    invitationsCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInvitationsCountResponse' value with any optional fields omitted.
mkGetInvitationsCountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInvitationsCountResponse
mkGetInvitationsCountResponse responseStatus =
  GetInvitationsCountResponse'
    { invitationsCount = Core.Nothing,
      responseStatus
    }

-- | The number of received invitations.
--
-- /Note:/ Consider using 'invitationsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsInvitationsCount :: Lens.Lens' GetInvitationsCountResponse (Core.Maybe Core.Int)
gicrrsInvitationsCount = Lens.field @"invitationsCount"
{-# DEPRECATED gicrrsInvitationsCount "Use generic-lens or generic-optics with 'invitationsCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gicrrsResponseStatus :: Lens.Lens' GetInvitationsCountResponse Core.Int
gicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
