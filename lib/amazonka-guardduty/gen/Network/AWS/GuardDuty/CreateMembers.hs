{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates member accounts of the current AWS account by specifying a list of AWS account IDs. This step is a prerequisite for managing the associated member accounts either by invitation or through an organization.
--
-- When using @Create Members@ as an organizations delegated administrator this action will enable GuardDuty in the added member accounts, with the exception of the organization master account, which must enable GuardDuty prior to being added as a member.
-- If you are adding accounts by invitation use this action after GuardDuty has been enabled in potential member accounts and before using <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_InviteMembers.html @Invite Members@ > .
module Network.AWS.GuardDuty.CreateMembers
  ( -- * Creating a request
    CreateMembers (..),
    mkCreateMembers,

    -- ** Request lenses
    cmDetectorId,
    cmAccountDetails,

    -- * Destructuring the response
    CreateMembersResponse (..),
    mkCreateMembersResponse,

    -- ** Response lenses
    cmrsResponseStatus,
    cmrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateMembers' smart constructor.
data CreateMembers = CreateMembers'
  { detectorId :: Lude.Text,
    accountDetails :: Lude.NonEmpty AccountDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMembers' with the minimum fields required to make a request.
--
-- * 'accountDetails' - A list of account ID and email address pairs of the accounts that you want to associate with the master GuardDuty account.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty account that you want to associate member accounts with.
mkCreateMembers ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'accountDetails'
  Lude.NonEmpty AccountDetail ->
  CreateMembers
mkCreateMembers pDetectorId_ pAccountDetails_ =
  CreateMembers'
    { detectorId = pDetectorId_,
      accountDetails = pAccountDetails_
    }

-- | The unique ID of the detector of the GuardDuty account that you want to associate member accounts with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmDetectorId :: Lens.Lens' CreateMembers Lude.Text
cmDetectorId = Lens.lens (detectorId :: CreateMembers -> Lude.Text) (\s a -> s {detectorId = a} :: CreateMembers)
{-# DEPRECATED cmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account ID and email address pairs of the accounts that you want to associate with the master GuardDuty account.
--
-- /Note:/ Consider using 'accountDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmAccountDetails :: Lens.Lens' CreateMembers (Lude.NonEmpty AccountDetail)
cmAccountDetails = Lens.lens (accountDetails :: CreateMembers -> Lude.NonEmpty AccountDetail) (\s a -> s {accountDetails = a} :: CreateMembers)
{-# DEPRECATED cmAccountDetails "Use generic-lens or generic-optics with 'accountDetails' instead." #-}

instance Lude.AWSRequest CreateMembers where
  type Rs CreateMembers = CreateMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMembersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders CreateMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMembers where
  toJSON CreateMembers' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("accountDetails" Lude..= accountDetails)]
      )

instance Lude.ToPath CreateMembers where
  toPath CreateMembers' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/member"]

instance Lude.ToQuery CreateMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateMembersResponse' smart constructor.
data CreateMembersResponse = CreateMembersResponse'
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

-- | Creates a value of 'CreateMembersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'unprocessedAccounts' - A list of objects that include the @accountIds@ of the unprocessed accounts and a result string that explains why each was unprocessed.
mkCreateMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMembersResponse
mkCreateMembersResponse pResponseStatus_ =
  CreateMembersResponse'
    { responseStatus = pResponseStatus_,
      unprocessedAccounts = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsResponseStatus :: Lens.Lens' CreateMembersResponse Lude.Int
cmrsResponseStatus = Lens.lens (responseStatus :: CreateMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMembersResponse)
{-# DEPRECATED cmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of objects that include the @accountIds@ of the unprocessed accounts and a result string that explains why each was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsUnprocessedAccounts :: Lens.Lens' CreateMembersResponse [UnprocessedAccount]
cmrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: CreateMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: CreateMembersResponse)
{-# DEPRECATED cmrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}
