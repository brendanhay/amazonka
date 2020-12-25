{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.InviteMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invites other AWS accounts (created as members of the current AWS account by CreateMembers) to enable GuardDuty, and allow the current AWS account to view and manage these accounts' GuardDuty findings on their behalf as the master account.
module Network.AWS.GuardDuty.InviteMembers
  ( -- * Creating a request
    InviteMembers (..),
    mkInviteMembers,

    -- ** Request lenses
    imDetectorId,
    imAccountIds,
    imDisableEmailNotification,
    imMessage,

    -- * Destructuring the response
    InviteMembersResponse (..),
    mkInviteMembersResponse,

    -- ** Response lenses
    imrrsUnprocessedAccounts,
    imrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkInviteMembers' smart constructor.
data InviteMembers = InviteMembers'
  { -- | The unique ID of the detector of the GuardDuty account that you want to invite members with.
    detectorId :: Types.DetectorId,
    -- | A list of account IDs of the accounts that you want to invite to GuardDuty as members.
    accountIds :: Core.NonEmpty Types.AccountId,
    -- | A Boolean value that specifies whether you want to disable email notification to the accounts that you are inviting to GuardDuty as members.
    disableEmailNotification :: Core.Maybe Core.Bool,
    -- | The invitation message that you want to send to the accounts that you're inviting to GuardDuty as members.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InviteMembers' value with any optional fields omitted.
mkInviteMembers ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  InviteMembers
mkInviteMembers detectorId accountIds =
  InviteMembers'
    { detectorId,
      accountIds,
      disableEmailNotification = Core.Nothing,
      message = Core.Nothing
    }

-- | The unique ID of the detector of the GuardDuty account that you want to invite members with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imDetectorId :: Lens.Lens' InviteMembers Types.DetectorId
imDetectorId = Lens.field @"detectorId"
{-# DEPRECATED imDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the accounts that you want to invite to GuardDuty as members.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imAccountIds :: Lens.Lens' InviteMembers (Core.NonEmpty Types.AccountId)
imAccountIds = Lens.field @"accountIds"
{-# DEPRECATED imAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | A Boolean value that specifies whether you want to disable email notification to the accounts that you are inviting to GuardDuty as members.
--
-- /Note:/ Consider using 'disableEmailNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imDisableEmailNotification :: Lens.Lens' InviteMembers (Core.Maybe Core.Bool)
imDisableEmailNotification = Lens.field @"disableEmailNotification"
{-# DEPRECATED imDisableEmailNotification "Use generic-lens or generic-optics with 'disableEmailNotification' instead." #-}

-- | The invitation message that you want to send to the accounts that you're inviting to GuardDuty as members.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imMessage :: Lens.Lens' InviteMembers (Core.Maybe Types.String)
imMessage = Lens.field @"message"
{-# DEPRECATED imMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON InviteMembers where
  toJSON InviteMembers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("accountIds" Core..= accountIds),
            ("disableEmailNotification" Core..=)
              Core.<$> disableEmailNotification,
            ("message" Core..=) Core.<$> message
          ]
      )

instance Core.AWSRequest InviteMembers where
  type Rs InviteMembers = InviteMembersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/member/invite")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          InviteMembersResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkInviteMembersResponse' smart constructor.
data InviteMembersResponse = InviteMembersResponse'
  { -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InviteMembersResponse' value with any optional fields omitted.
mkInviteMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  InviteMembersResponse
mkInviteMembersResponse responseStatus =
  InviteMembersResponse'
    { unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imrrsUnprocessedAccounts :: Lens.Lens' InviteMembersResponse [Types.UnprocessedAccount]
imrrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED imrrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imrrsResponseStatus :: Lens.Lens' InviteMembersResponse Core.Int
imrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED imrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
