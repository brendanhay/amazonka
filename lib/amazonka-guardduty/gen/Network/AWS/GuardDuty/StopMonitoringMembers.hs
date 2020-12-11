{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.StopMonitoringMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops GuardDuty monitoring for the specified member accounts. Use the @StartMonitoringMembers@ operation to restart monitoring for those accounts.
module Network.AWS.GuardDuty.StopMonitoringMembers
  ( -- * Creating a request
    StopMonitoringMembers (..),
    mkStopMonitoringMembers,

    -- ** Request lenses
    smmDetectorId,
    smmAccountIds,

    -- * Destructuring the response
    StopMonitoringMembersResponse (..),
    mkStopMonitoringMembersResponse,

    -- ** Response lenses
    smmrsResponseStatus,
    smmrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopMonitoringMembers' smart constructor.
data StopMonitoringMembers = StopMonitoringMembers'
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

-- | Creates a value of 'StopMonitoringMembers' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs for the member accounts to stop monitoring.
-- * 'detectorId' - The unique ID of the detector associated with the GuardDuty master account that is monitoring member accounts.
mkStopMonitoringMembers ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  StopMonitoringMembers
mkStopMonitoringMembers pDetectorId_ pAccountIds_ =
  StopMonitoringMembers'
    { detectorId = pDetectorId_,
      accountIds = pAccountIds_
    }

-- | The unique ID of the detector associated with the GuardDuty master account that is monitoring member accounts.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmDetectorId :: Lens.Lens' StopMonitoringMembers Lude.Text
smmDetectorId = Lens.lens (detectorId :: StopMonitoringMembers -> Lude.Text) (\s a -> s {detectorId = a} :: StopMonitoringMembers)
{-# DEPRECATED smmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs for the member accounts to stop monitoring.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmAccountIds :: Lens.Lens' StopMonitoringMembers (Lude.NonEmpty Lude.Text)
smmAccountIds = Lens.lens (accountIds :: StopMonitoringMembers -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: StopMonitoringMembers)
{-# DEPRECATED smmAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest StopMonitoringMembers where
  type Rs StopMonitoringMembers = StopMonitoringMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopMonitoringMembersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders StopMonitoringMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopMonitoringMembers where
  toJSON StopMonitoringMembers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("accountIds" Lude..= accountIds)])

instance Lude.ToPath StopMonitoringMembers where
  toPath StopMonitoringMembers' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/member/stop"]

instance Lude.ToQuery StopMonitoringMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopMonitoringMembersResponse' smart constructor.
data StopMonitoringMembersResponse = StopMonitoringMembersResponse'
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

-- | Creates a value of 'StopMonitoringMembersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'unprocessedAccounts' - A list of objects that contain an accountId for each account that could not be processed, and a result string that indicates why the account was not processed.
mkStopMonitoringMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopMonitoringMembersResponse
mkStopMonitoringMembersResponse pResponseStatus_ =
  StopMonitoringMembersResponse'
    { responseStatus = pResponseStatus_,
      unprocessedAccounts = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmrsResponseStatus :: Lens.Lens' StopMonitoringMembersResponse Lude.Int
smmrsResponseStatus = Lens.lens (responseStatus :: StopMonitoringMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopMonitoringMembersResponse)
{-# DEPRECATED smmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of objects that contain an accountId for each account that could not be processed, and a result string that indicates why the account was not processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmrsUnprocessedAccounts :: Lens.Lens' StopMonitoringMembersResponse [UnprocessedAccount]
smmrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: StopMonitoringMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: StopMonitoringMembersResponse)
{-# DEPRECATED smmrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}
