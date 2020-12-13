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
    dAccountIds,

    -- * Destructuring the response
    DeleteInvitationsResponse (..),
    mkDeleteInvitationsResponse,

    -- ** Response lenses
    dirsUnprocessedAccounts,
    dirsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInvitations' smart constructor.
newtype DeleteInvitations = DeleteInvitations'
  { -- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
    accountIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInvitations' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
mkDeleteInvitations ::
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  DeleteInvitations
mkDeleteInvitations pAccountIds_ =
  DeleteInvitations' {accountIds = pAccountIds_}

-- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccountIds :: Lens.Lens' DeleteInvitations (Lude.NonEmpty Lude.Text)
dAccountIds = Lens.lens (accountIds :: DeleteInvitations -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: DeleteInvitations)
{-# DEPRECATED dAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest DeleteInvitations where
  type Rs DeleteInvitations = DeleteInvitationsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteInvitationsResponse'
            Lude.<$> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInvitations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteInvitations where
  toJSON DeleteInvitations' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("accountIds" Lude..= accountIds)])

instance Lude.ToPath DeleteInvitations where
  toPath = Lude.const "/invitation/delete"

instance Lude.ToQuery DeleteInvitations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteInvitationsResponse' smart constructor.
data DeleteInvitationsResponse = DeleteInvitationsResponse'
  { -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInvitationsResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
-- * 'responseStatus' - The response status code.
mkDeleteInvitationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInvitationsResponse
mkDeleteInvitationsResponse pResponseStatus_ =
  DeleteInvitationsResponse'
    { unprocessedAccounts = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsUnprocessedAccounts :: Lens.Lens' DeleteInvitationsResponse [UnprocessedAccount]
dirsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: DeleteInvitationsResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: DeleteInvitationsResponse)
{-# DEPRECATED dirsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteInvitationsResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteInvitationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInvitationsResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
