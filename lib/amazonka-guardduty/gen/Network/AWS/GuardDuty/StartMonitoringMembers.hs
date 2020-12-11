{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.StartMonitoringMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns on GuardDuty monitoring of the specified member accounts. Use this operation to restart monitoring of accounts that you stopped monitoring with the @StopMonitoringMembers@ operation.
module Network.AWS.GuardDuty.StartMonitoringMembers
  ( -- * Creating a request
    StartMonitoringMembers (..),
    mkStartMonitoringMembers,

    -- ** Request lenses
    sDetectorId,
    sAccountIds,

    -- * Destructuring the response
    StartMonitoringMembersResponse (..),
    mkStartMonitoringMembersResponse,

    -- ** Response lenses
    srsResponseStatus,
    srsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartMonitoringMembers' smart constructor.
data StartMonitoringMembers = StartMonitoringMembers'
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

-- | Creates a value of 'StartMonitoringMembers' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the GuardDuty member accounts to start monitoring.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
mkStartMonitoringMembers ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  StartMonitoringMembers
mkStartMonitoringMembers pDetectorId_ pAccountIds_ =
  StartMonitoringMembers'
    { detectorId = pDetectorId_,
      accountIds = pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDetectorId :: Lens.Lens' StartMonitoringMembers Lude.Text
sDetectorId = Lens.lens (detectorId :: StartMonitoringMembers -> Lude.Text) (\s a -> s {detectorId = a} :: StartMonitoringMembers)
{-# DEPRECATED sDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the GuardDuty member accounts to start monitoring.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccountIds :: Lens.Lens' StartMonitoringMembers (Lude.NonEmpty Lude.Text)
sAccountIds = Lens.lens (accountIds :: StartMonitoringMembers -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: StartMonitoringMembers)
{-# DEPRECATED sAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest StartMonitoringMembers where
  type Rs StartMonitoringMembers = StartMonitoringMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMonitoringMembersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders StartMonitoringMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartMonitoringMembers where
  toJSON StartMonitoringMembers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("accountIds" Lude..= accountIds)])

instance Lude.ToPath StartMonitoringMembers where
  toPath StartMonitoringMembers' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/member/start"]

instance Lude.ToQuery StartMonitoringMembers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartMonitoringMembersResponse' smart constructor.
data StartMonitoringMembersResponse = StartMonitoringMembersResponse'
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

-- | Creates a value of 'StartMonitoringMembersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'unprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
mkStartMonitoringMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMonitoringMembersResponse
mkStartMonitoringMembersResponse pResponseStatus_ =
  StartMonitoringMembersResponse'
    { responseStatus =
        pResponseStatus_,
      unprocessedAccounts = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartMonitoringMembersResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartMonitoringMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMonitoringMembersResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsUnprocessedAccounts :: Lens.Lens' StartMonitoringMembersResponse [UnprocessedAccount]
srsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: StartMonitoringMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: StartMonitoringMembersResponse)
{-# DEPRECATED srsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}
