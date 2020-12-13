{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.GetMembers
  ( -- * Creating a request
    GetMembers (..),
    mkGetMembers,

    -- ** Request lenses
    gmAccountIds,
    gmDetectorId,

    -- * Destructuring the response
    GetMembersResponse (..),
    mkGetMembersResponse,

    -- ** Response lenses
    gmrsMembers,
    gmrsUnprocessedAccounts,
    gmrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMembers' smart constructor.
data GetMembers = GetMembers'
  { -- | A list of account IDs of the GuardDuty member accounts that you want to describe.
    accountIds :: Lude.NonEmpty Lude.Text,
    -- | The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMembers' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the GuardDuty member accounts that you want to describe.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
mkGetMembers ::
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'detectorId'
  Lude.Text ->
  GetMembers
mkGetMembers pAccountIds_ pDetectorId_ =
  GetMembers' {accountIds = pAccountIds_, detectorId = pDetectorId_}

-- | A list of account IDs of the GuardDuty member accounts that you want to describe.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmAccountIds :: Lens.Lens' GetMembers (Lude.NonEmpty Lude.Text)
gmAccountIds = Lens.lens (accountIds :: GetMembers -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: GetMembers)
{-# DEPRECATED gmAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmDetectorId :: Lens.Lens' GetMembers Lude.Text
gmDetectorId = Lens.lens (detectorId :: GetMembers -> Lude.Text) (\s a -> s {detectorId = a} :: GetMembers)
{-# DEPRECATED gmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest GetMembers where
  type Rs GetMembers = GetMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMembersResponse'
            Lude.<$> (x Lude..?> "members" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMembers where
  toJSON GetMembers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("accountIds" Lude..= accountIds)])

instance Lude.ToPath GetMembers where
  toPath GetMembers' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/member/get"]

instance Lude.ToQuery GetMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMembersResponse' smart constructor.
data GetMembersResponse = GetMembersResponse'
  { -- | A list of members.
    members :: [Member],
    -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMembersResponse' with the minimum fields required to make a request.
--
-- * 'members' - A list of members.
-- * 'unprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
-- * 'responseStatus' - The response status code.
mkGetMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMembersResponse
mkGetMembersResponse pResponseStatus_ =
  GetMembersResponse'
    { members = Lude.mempty,
      unprocessedAccounts = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of members.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsMembers :: Lens.Lens' GetMembersResponse [Member]
gmrsMembers = Lens.lens (members :: GetMembersResponse -> [Member]) (\s a -> s {members = a} :: GetMembersResponse)
{-# DEPRECATED gmrsMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsUnprocessedAccounts :: Lens.Lens' GetMembersResponse [UnprocessedAccount]
gmrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: GetMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: GetMembersResponse)
{-# DEPRECATED gmrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsResponseStatus :: Lens.Lens' GetMembersResponse Lude.Int
gmrsResponseStatus = Lens.lens (responseStatus :: GetMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMembersResponse)
{-# DEPRECATED gmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
