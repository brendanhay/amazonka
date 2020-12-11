{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.DeleteMembers
  ( -- * Creating a request
    DeleteMembers (..),
    mkDeleteMembers,

    -- ** Request lenses
    dmDetectorId,
    dmAccountIds,

    -- * Destructuring the response
    DeleteMembersResponse (..),
    mkDeleteMembersResponse,

    -- ** Response lenses
    drsResponseStatus,
    drsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { detectorId :: Lude.Text,
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

-- | Creates a value of 'DeleteMembers' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the GuardDuty member accounts that you want to delete.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty account whose members you want to delete.
mkDeleteMembers ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  DeleteMembers
mkDeleteMembers pDetectorId_ pAccountIds_ =
  DeleteMembers'
    { detectorId = pDetectorId_,
      accountIds = pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you want to delete.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDetectorId :: Lens.Lens' DeleteMembers Lude.Text
dmDetectorId = Lens.lens (detectorId :: DeleteMembers -> Lude.Text) (\s a -> s {detectorId = a} :: DeleteMembers)
{-# DEPRECATED dmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the GuardDuty member accounts that you want to delete.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmAccountIds :: Lens.Lens' DeleteMembers (Lude.NonEmpty Lude.Text)
dmAccountIds = Lens.lens (accountIds :: DeleteMembers -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: DeleteMembers)
{-# DEPRECATED dmAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest DeleteMembers where
  type Rs DeleteMembers = DeleteMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteMembersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders DeleteMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMembers where
  toJSON DeleteMembers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("accountIds" Lude..= accountIds)])

instance Lude.ToPath DeleteMembers where
  toPath DeleteMembers' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/member/delete"]

instance Lude.ToQuery DeleteMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMembersResponse' smart constructor.
data DeleteMembersResponse = DeleteMembersResponse'
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

-- | Creates a value of 'DeleteMembersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'unprocessedAccounts' - The accounts that could not be processed.
mkDeleteMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMembersResponse
mkDeleteMembersResponse pResponseStatus_ =
  DeleteMembersResponse'
    { responseStatus = pResponseStatus_,
      unprocessedAccounts = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteMembersResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMembersResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The accounts that could not be processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsUnprocessedAccounts :: Lens.Lens' DeleteMembersResponse [UnprocessedAccount]
drsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: DeleteMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: DeleteMembersResponse)
{-# DEPRECATED drsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}
