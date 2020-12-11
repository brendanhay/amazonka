{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    imDisableEmailNotification,
    imMessage,
    imDetectorId,
    imAccountIds,

    -- * Destructuring the response
    InviteMembersResponse (..),
    mkInviteMembersResponse,

    -- ** Response lenses
    imrsResponseStatus,
    imrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkInviteMembers' smart constructor.
data InviteMembers = InviteMembers'
  { disableEmailNotification ::
      Lude.Maybe Lude.Bool,
    message :: Lude.Maybe Lude.Text,
    detectorId :: Lude.Text,
    accountIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InviteMembers' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the accounts that you want to invite to GuardDuty as members.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty account that you want to invite members with.
-- * 'disableEmailNotification' - A Boolean value that specifies whether you want to disable email notification to the accounts that you are inviting to GuardDuty as members.
-- * 'message' - The invitation message that you want to send to the accounts that you're inviting to GuardDuty as members.
mkInviteMembers ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  InviteMembers
mkInviteMembers pDetectorId_ pAccountIds_ =
  InviteMembers'
    { disableEmailNotification = Lude.Nothing,
      message = Lude.Nothing,
      detectorId = pDetectorId_,
      accountIds = pAccountIds_
    }

-- | A Boolean value that specifies whether you want to disable email notification to the accounts that you are inviting to GuardDuty as members.
--
-- /Note:/ Consider using 'disableEmailNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imDisableEmailNotification :: Lens.Lens' InviteMembers (Lude.Maybe Lude.Bool)
imDisableEmailNotification = Lens.lens (disableEmailNotification :: InviteMembers -> Lude.Maybe Lude.Bool) (\s a -> s {disableEmailNotification = a} :: InviteMembers)
{-# DEPRECATED imDisableEmailNotification "Use generic-lens or generic-optics with 'disableEmailNotification' instead." #-}

-- | The invitation message that you want to send to the accounts that you're inviting to GuardDuty as members.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imMessage :: Lens.Lens' InviteMembers (Lude.Maybe Lude.Text)
imMessage = Lens.lens (message :: InviteMembers -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: InviteMembers)
{-# DEPRECATED imMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The unique ID of the detector of the GuardDuty account that you want to invite members with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imDetectorId :: Lens.Lens' InviteMembers Lude.Text
imDetectorId = Lens.lens (detectorId :: InviteMembers -> Lude.Text) (\s a -> s {detectorId = a} :: InviteMembers)
{-# DEPRECATED imDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the accounts that you want to invite to GuardDuty as members.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imAccountIds :: Lens.Lens' InviteMembers (Lude.NonEmpty Lude.Text)
imAccountIds = Lens.lens (accountIds :: InviteMembers -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: InviteMembers)
{-# DEPRECATED imAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest InviteMembers where
  type Rs InviteMembers = InviteMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          InviteMembersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders InviteMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON InviteMembers where
  toJSON InviteMembers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("disableEmailNotification" Lude..=)
              Lude.<$> disableEmailNotification,
            ("message" Lude..=) Lude.<$> message,
            Lude.Just ("accountIds" Lude..= accountIds)
          ]
      )

instance Lude.ToPath InviteMembers where
  toPath InviteMembers' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/member/invite"]

instance Lude.ToQuery InviteMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkInviteMembersResponse' smart constructor.
data InviteMembersResponse = InviteMembersResponse'
  { responseStatus ::
      Lude.Int,
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InviteMembersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'unprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
mkInviteMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InviteMembersResponse
mkInviteMembersResponse pResponseStatus_ =
  InviteMembersResponse'
    { responseStatus = pResponseStatus_,
      unprocessedAccounts = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imrsResponseStatus :: Lens.Lens' InviteMembersResponse Lude.Int
imrsResponseStatus = Lens.lens (responseStatus :: InviteMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InviteMembersResponse)
{-# DEPRECATED imrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imrsUnprocessedAccounts :: Lens.Lens' InviteMembersResponse [UnprocessedAccount]
imrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: InviteMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: InviteMembersResponse)
{-# DEPRECATED imrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}
