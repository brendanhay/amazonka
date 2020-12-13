{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    smmAccountIds,
    smmDetectorId,

    -- * Destructuring the response
    StartMonitoringMembersResponse (..),
    mkStartMonitoringMembersResponse,

    -- ** Response lenses
    srsUnprocessedAccounts,
    srsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartMonitoringMembers' smart constructor.
data StartMonitoringMembers = StartMonitoringMembers'
  { -- | A list of account IDs of the GuardDuty member accounts to start monitoring.
    accountIds :: Lude.NonEmpty Lude.Text,
    -- | The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMonitoringMembers' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of account IDs of the GuardDuty member accounts to start monitoring.
-- * 'detectorId' - The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
mkStartMonitoringMembers ::
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'detectorId'
  Lude.Text ->
  StartMonitoringMembers
mkStartMonitoringMembers pAccountIds_ pDetectorId_ =
  StartMonitoringMembers'
    { accountIds = pAccountIds_,
      detectorId = pDetectorId_
    }

-- | A list of account IDs of the GuardDuty member accounts to start monitoring.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmAccountIds :: Lens.Lens' StartMonitoringMembers (Lude.NonEmpty Lude.Text)
smmAccountIds = Lens.lens (accountIds :: StartMonitoringMembers -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: StartMonitoringMembers)
{-# DEPRECATED smmAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmDetectorId :: Lens.Lens' StartMonitoringMembers Lude.Text
smmDetectorId = Lens.lens (detectorId :: StartMonitoringMembers -> Lude.Text) (\s a -> s {detectorId = a} :: StartMonitoringMembers)
{-# DEPRECATED smmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest StartMonitoringMembers where
  type Rs StartMonitoringMembers = StartMonitoringMembersResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartMonitoringMembersResponse'
            Lude.<$> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMonitoringMembersResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
-- * 'responseStatus' - The response status code.
mkStartMonitoringMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMonitoringMembersResponse
mkStartMonitoringMembersResponse pResponseStatus_ =
  StartMonitoringMembersResponse'
    { unprocessedAccounts =
        Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsUnprocessedAccounts :: Lens.Lens' StartMonitoringMembersResponse [UnprocessedAccount]
srsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: StartMonitoringMembersResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: StartMonitoringMembersResponse)
{-# DEPRECATED srsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartMonitoringMembersResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartMonitoringMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMonitoringMembersResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
