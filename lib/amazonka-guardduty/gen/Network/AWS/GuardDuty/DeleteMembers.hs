{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dmsAccountIds,
    dmsDetectorId,

    -- * Destructuring the response
    DeleteMembersResponse (..),
    mkDeleteMembersResponse,

    -- ** Response lenses
    drsUnprocessedAccounts,
    drsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { -- | A list of account IDs of the GuardDuty member accounts that you want to delete.
    accountIds :: Lude.NonEmpty Lude.Text,
    -- | The unique ID of the detector of the GuardDuty account whose members you want to delete.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMembers' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the GuardDuty member accounts that you want to delete.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty account whose members you want to delete.
mkDeleteMembers ::
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'detectorId'
  Lude.Text ->
  DeleteMembers
mkDeleteMembers pAccountIds_ pDetectorId_ =
  DeleteMembers'
    { accountIds = pAccountIds_,
      detectorId = pDetectorId_
    }

-- | A list of account IDs of the GuardDuty member accounts that you want to delete.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsAccountIds :: Lens.Lens' DeleteMembers (Lude.NonEmpty Lude.Text)
dmsAccountIds = Lens.lens (accountIds :: DeleteMembers -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: DeleteMembers)
{-# DEPRECATED dmsAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | The unique ID of the detector of the GuardDuty account whose members you want to delete.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsDetectorId :: Lens.Lens' DeleteMembers Lude.Text
dmsDetectorId = Lens.lens (detectorId :: DeleteMembers -> Lude.Text) (\s a -> s {detectorId = a} :: DeleteMembers)
{-# DEPRECATED dmsDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest DeleteMembers where
  type Rs DeleteMembers = DeleteMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteMembersResponse'
            Lude.<$> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | The accounts that could not be processed.
    unprocessedAccounts :: [UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMembersResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedAccounts' - The accounts that could not be processed.
-- * 'responseStatus' - The response status code.
mkDeleteMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMembersResponse
mkDeleteMembersResponse pResponseStatus_ =
  DeleteMembersResponse'
    { unprocessedAccounts = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The accounts that could not be processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsUnprocessedAccounts :: Lens.Lens' DeleteMembersResponse [UnprocessedAccount]
drsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: DeleteMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: DeleteMembersResponse)
{-# DEPRECATED drsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteMembersResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMembersResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
