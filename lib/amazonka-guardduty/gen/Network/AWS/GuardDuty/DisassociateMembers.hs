{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DisassociateMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.DisassociateMembers
  ( -- * Creating a request
    DisassociateMembers (..),
    mkDisassociateMembers,

    -- ** Request lenses
    dmsDetectorId,
    dmsAccountIds,

    -- * Destructuring the response
    DisassociateMembersResponse (..),
    mkDisassociateMembersResponse,

    -- ** Response lenses
    dmrsResponseStatus,
    dmrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateMembers' smart constructor.
data DisassociateMembers = DisassociateMembers'
  { detectorId ::
      Lude.Text,
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

-- | Creates a value of 'DisassociateMembers' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the GuardDuty member accounts that you want to disassociate from the master account.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty account whose members you want to disassociate from the master account.
mkDisassociateMembers ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  DisassociateMembers
mkDisassociateMembers pDetectorId_ pAccountIds_ =
  DisassociateMembers'
    { detectorId = pDetectorId_,
      accountIds = pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you want to disassociate from the master account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsDetectorId :: Lens.Lens' DisassociateMembers Lude.Text
dmsDetectorId = Lens.lens (detectorId :: DisassociateMembers -> Lude.Text) (\s a -> s {detectorId = a} :: DisassociateMembers)
{-# DEPRECATED dmsDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the GuardDuty member accounts that you want to disassociate from the master account.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsAccountIds :: Lens.Lens' DisassociateMembers (Lude.NonEmpty Lude.Text)
dmsAccountIds = Lens.lens (accountIds :: DisassociateMembers -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: DisassociateMembers)
{-# DEPRECATED dmsAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest DisassociateMembers where
  type Rs DisassociateMembers = DisassociateMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisassociateMembersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DisassociateMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateMembers where
  toJSON DisassociateMembers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("accountIds" Lude..= accountIds)])

instance Lude.ToPath DisassociateMembers where
  toPath DisassociateMembers' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/member/disassociate"]

instance Lude.ToQuery DisassociateMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateMembersResponse' smart constructor.
data DisassociateMembersResponse = DisassociateMembersResponse'
  { responseStatus ::
      Lude.Int,
    unprocessedAccounts ::
      [UnprocessedAccount]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateMembersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'unprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
mkDisassociateMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateMembersResponse
mkDisassociateMembersResponse pResponseStatus_ =
  DisassociateMembersResponse'
    { responseStatus = pResponseStatus_,
      unprocessedAccounts = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsResponseStatus :: Lens.Lens' DisassociateMembersResponse Lude.Int
dmrsResponseStatus = Lens.lens (responseStatus :: DisassociateMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateMembersResponse)
{-# DEPRECATED dmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsUnprocessedAccounts :: Lens.Lens' DisassociateMembersResponse [UnprocessedAccount]
dmrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: DisassociateMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: DisassociateMembersResponse)
{-# DEPRECATED dmrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}
