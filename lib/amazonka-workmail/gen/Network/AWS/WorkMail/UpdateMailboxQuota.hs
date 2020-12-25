{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.UpdateMailboxQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user's current mailbox quota for a specified organization and user.
module Network.AWS.WorkMail.UpdateMailboxQuota
  ( -- * Creating a request
    UpdateMailboxQuota (..),
    mkUpdateMailboxQuota,

    -- ** Request lenses
    umqOrganizationId,
    umqUserId,
    umqMailboxQuota,

    -- * Destructuring the response
    UpdateMailboxQuotaResponse (..),
    mkUpdateMailboxQuotaResponse,

    -- ** Response lenses
    umqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkUpdateMailboxQuota' smart constructor.
data UpdateMailboxQuota = UpdateMailboxQuota'
  { -- | The identifier for the organization that contains the user for whom to update the mailbox quota.
    organizationId :: Types.OrganizationId,
    -- | The identifer for the user for whom to update the mailbox quota.
    userId :: Types.WorkMailIdentifier,
    -- | The updated mailbox quota, in MB, for the specified user.
    mailboxQuota :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMailboxQuota' value with any optional fields omitted.
mkUpdateMailboxQuota ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'userId'
  Types.WorkMailIdentifier ->
  -- | 'mailboxQuota'
  Core.Natural ->
  UpdateMailboxQuota
mkUpdateMailboxQuota organizationId userId mailboxQuota =
  UpdateMailboxQuota' {organizationId, userId, mailboxQuota}

-- | The identifier for the organization that contains the user for whom to update the mailbox quota.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqOrganizationId :: Lens.Lens' UpdateMailboxQuota Types.OrganizationId
umqOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED umqOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifer for the user for whom to update the mailbox quota.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqUserId :: Lens.Lens' UpdateMailboxQuota Types.WorkMailIdentifier
umqUserId = Lens.field @"userId"
{-# DEPRECATED umqUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The updated mailbox quota, in MB, for the specified user.
--
-- /Note:/ Consider using 'mailboxQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqMailboxQuota :: Lens.Lens' UpdateMailboxQuota Core.Natural
umqMailboxQuota = Lens.field @"mailboxQuota"
{-# DEPRECATED umqMailboxQuota "Use generic-lens or generic-optics with 'mailboxQuota' instead." #-}

instance Core.FromJSON UpdateMailboxQuota where
  toJSON UpdateMailboxQuota {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("UserId" Core..= userId),
            Core.Just ("MailboxQuota" Core..= mailboxQuota)
          ]
      )

instance Core.AWSRequest UpdateMailboxQuota where
  type Rs UpdateMailboxQuota = UpdateMailboxQuotaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.UpdateMailboxQuota")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMailboxQuotaResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateMailboxQuotaResponse' smart constructor.
newtype UpdateMailboxQuotaResponse = UpdateMailboxQuotaResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMailboxQuotaResponse' value with any optional fields omitted.
mkUpdateMailboxQuotaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMailboxQuotaResponse
mkUpdateMailboxQuotaResponse responseStatus =
  UpdateMailboxQuotaResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umqrrsResponseStatus :: Lens.Lens' UpdateMailboxQuotaResponse Core.Int
umqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
