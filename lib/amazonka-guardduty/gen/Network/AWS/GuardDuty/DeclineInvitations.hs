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
    diAccountIds,

    -- * Destructuring the response
    DeclineInvitationsResponse (..),
    mkDeclineInvitationsResponse,

    -- ** Response lenses
    disrsUnprocessedAccounts,
    disrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeclineInvitations' smart constructor.
newtype DeclineInvitations = DeclineInvitations'
  { -- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
    accountIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeclineInvitations' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
mkDeclineInvitations ::
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  DeclineInvitations
mkDeclineInvitations pAccountIds_ =
  DeclineInvitations' {accountIds = pAccountIds_}

-- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAccountIds :: Lens.Lens' DeclineInvitations (Lude.NonEmpty Lude.Text)
diAccountIds = Lens.lens (accountIds :: DeclineInvitations -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: DeclineInvitations)
{-# DEPRECATED diAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest DeclineInvitations where
  type Rs DeclineInvitations = DeclineInvitationsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeclineInvitationsResponse'
            Lude.<$> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeclineInvitations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeclineInvitations where
  toJSON DeclineInvitations' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("accountIds" Lude..= accountIds)])

instance Lude.ToPath DeclineInvitations where
  toPath = Lude.const "/invitation/decline"

instance Lude.ToQuery DeclineInvitations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeclineInvitationsResponse' smart constructor.
data DeclineInvitationsResponse = DeclineInvitationsResponse'
  { -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeclineInvitationsResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
-- * 'responseStatus' - The response status code.
mkDeclineInvitationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeclineInvitationsResponse
mkDeclineInvitationsResponse pResponseStatus_ =
  DeclineInvitationsResponse'
    { unprocessedAccounts = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsUnprocessedAccounts :: Lens.Lens' DeclineInvitationsResponse [UnprocessedAccount]
disrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: DeclineInvitationsResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: DeclineInvitationsResponse)
{-# DEPRECATED disrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsResponseStatus :: Lens.Lens' DeclineInvitationsResponse Lude.Int
disrsResponseStatus = Lens.lens (responseStatus :: DeclineInvitationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeclineInvitationsResponse)
{-# DEPRECATED disrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
