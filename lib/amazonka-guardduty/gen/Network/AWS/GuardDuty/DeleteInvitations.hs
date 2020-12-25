{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteInvitations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes invitations sent to the current member account by AWS accounts specified by their account IDs.
module Network.AWS.GuardDuty.DeleteInvitations
  ( -- * Creating a request
    DeleteInvitations (..),
    mkDeleteInvitations,

    -- ** Request lenses
    diAccountIds,

    -- * Destructuring the response
    DeleteInvitationsResponse (..),
    mkDeleteInvitationsResponse,

    -- ** Response lenses
    dirrsUnprocessedAccounts,
    dirrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInvitations' smart constructor.
newtype DeleteInvitations = DeleteInvitations'
  { -- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
    accountIds :: Core.NonEmpty Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInvitations' value with any optional fields omitted.
mkDeleteInvitations ::
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  DeleteInvitations
mkDeleteInvitations accountIds = DeleteInvitations' {accountIds}

-- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAccountIds :: Lens.Lens' DeleteInvitations (Core.NonEmpty Types.AccountId)
diAccountIds = Lens.field @"accountIds"
{-# DEPRECATED diAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Core.FromJSON DeleteInvitations where
  toJSON DeleteInvitations {..} =
    Core.object
      (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest DeleteInvitations where
  type Rs DeleteInvitations = DeleteInvitationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/invitation/delete",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInvitationsResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteInvitationsResponse' smart constructor.
data DeleteInvitationsResponse = DeleteInvitationsResponse'
  { -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInvitationsResponse' value with any optional fields omitted.
mkDeleteInvitationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteInvitationsResponse
mkDeleteInvitationsResponse responseStatus =
  DeleteInvitationsResponse'
    { unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsUnprocessedAccounts :: Lens.Lens' DeleteInvitationsResponse [Types.UnprocessedAccount]
dirrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED dirrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteInvitationsResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
