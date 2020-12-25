{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.GetMailboxDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a user's mailbox details for a specified organization and user.
module Network.AWS.WorkMail.GetMailboxDetails
  ( -- * Creating a request
    GetMailboxDetails (..),
    mkGetMailboxDetails,

    -- ** Request lenses
    gmdOrganizationId,
    gmdUserId,

    -- * Destructuring the response
    GetMailboxDetailsResponse (..),
    mkGetMailboxDetailsResponse,

    -- ** Response lenses
    gmdrrsMailboxQuota,
    gmdrrsMailboxSize,
    gmdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkGetMailboxDetails' smart constructor.
data GetMailboxDetails = GetMailboxDetails'
  { -- | The identifier for the organization that contains the user whose mailbox details are being requested.
    organizationId :: Types.OrganizationId,
    -- | The identifier for the user whose mailbox details are being requested.
    userId :: Types.WorkMailIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMailboxDetails' value with any optional fields omitted.
mkGetMailboxDetails ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'userId'
  Types.WorkMailIdentifier ->
  GetMailboxDetails
mkGetMailboxDetails organizationId userId =
  GetMailboxDetails' {organizationId, userId}

-- | The identifier for the organization that contains the user whose mailbox details are being requested.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdOrganizationId :: Lens.Lens' GetMailboxDetails Types.OrganizationId
gmdOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED gmdOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the user whose mailbox details are being requested.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdUserId :: Lens.Lens' GetMailboxDetails Types.WorkMailIdentifier
gmdUserId = Lens.field @"userId"
{-# DEPRECATED gmdUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.FromJSON GetMailboxDetails where
  toJSON GetMailboxDetails {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("UserId" Core..= userId)
          ]
      )

instance Core.AWSRequest GetMailboxDetails where
  type Rs GetMailboxDetails = GetMailboxDetailsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.GetMailboxDetails")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMailboxDetailsResponse'
            Core.<$> (x Core..:? "MailboxQuota")
            Core.<*> (x Core..:? "MailboxSize")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMailboxDetailsResponse' smart constructor.
data GetMailboxDetailsResponse = GetMailboxDetailsResponse'
  { -- | The maximum allowed mailbox size, in MB, for the specified user.
    mailboxQuota :: Core.Maybe Core.Natural,
    -- | The current mailbox size, in MB, for the specified user.
    mailboxSize :: Core.Maybe Core.Double,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMailboxDetailsResponse' value with any optional fields omitted.
mkGetMailboxDetailsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMailboxDetailsResponse
mkGetMailboxDetailsResponse responseStatus =
  GetMailboxDetailsResponse'
    { mailboxQuota = Core.Nothing,
      mailboxSize = Core.Nothing,
      responseStatus
    }

-- | The maximum allowed mailbox size, in MB, for the specified user.
--
-- /Note:/ Consider using 'mailboxQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsMailboxQuota :: Lens.Lens' GetMailboxDetailsResponse (Core.Maybe Core.Natural)
gmdrrsMailboxQuota = Lens.field @"mailboxQuota"
{-# DEPRECATED gmdrrsMailboxQuota "Use generic-lens or generic-optics with 'mailboxQuota' instead." #-}

-- | The current mailbox size, in MB, for the specified user.
--
-- /Note:/ Consider using 'mailboxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsMailboxSize :: Lens.Lens' GetMailboxDetailsResponse (Core.Maybe Core.Double)
gmdrrsMailboxSize = Lens.field @"mailboxSize"
{-# DEPRECATED gmdrrsMailboxSize "Use generic-lens or generic-optics with 'mailboxSize' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsResponseStatus :: Lens.Lens' GetMailboxDetailsResponse Core.Int
gmdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
