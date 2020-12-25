{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeclineInvitations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Declines invitations sent to the current member account by AWS accounts specified by their account IDs.
module Network.AWS.GuardDuty.DeclineInvitations
  ( -- * Creating a request
    DeclineInvitations (..),
    mkDeclineInvitations,

    -- ** Request lenses
    dAccountIds,

    -- * Destructuring the response
    DeclineInvitationsResponse (..),
    mkDeclineInvitationsResponse,

    -- ** Response lenses
    dirfrsUnprocessedAccounts,
    dirfrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeclineInvitations' smart constructor.
newtype DeclineInvitations = DeclineInvitations'
  { -- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
    accountIds :: Core.NonEmpty Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeclineInvitations' value with any optional fields omitted.
mkDeclineInvitations ::
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  DeclineInvitations
mkDeclineInvitations accountIds = DeclineInvitations' {accountIds}

-- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccountIds :: Lens.Lens' DeclineInvitations (Core.NonEmpty Types.AccountId)
dAccountIds = Lens.field @"accountIds"
{-# DEPRECATED dAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Core.FromJSON DeclineInvitations where
  toJSON DeclineInvitations {..} =
    Core.object
      (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest DeclineInvitations where
  type Rs DeclineInvitations = DeclineInvitationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/invitation/decline",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeclineInvitationsResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeclineInvitationsResponse' smart constructor.
data DeclineInvitationsResponse = DeclineInvitationsResponse'
  { -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeclineInvitationsResponse' value with any optional fields omitted.
mkDeclineInvitationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeclineInvitationsResponse
mkDeclineInvitationsResponse responseStatus =
  DeclineInvitationsResponse'
    { unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsUnprocessedAccounts :: Lens.Lens' DeclineInvitationsResponse [Types.UnprocessedAccount]
dirfrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED dirfrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsResponseStatus :: Lens.Lens' DeclineInvitationsResponse Core.Int
dirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
