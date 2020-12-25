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
    sDetectorId,
    sAccountIds,

    -- * Destructuring the response
    StartMonitoringMembersResponse (..),
    mkStartMonitoringMembersResponse,

    -- ** Response lenses
    srsUnprocessedAccounts,
    srsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartMonitoringMembers' smart constructor.
data StartMonitoringMembers = StartMonitoringMembers'
  { -- | The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
    detectorId :: Types.DetectorId,
    -- | A list of account IDs of the GuardDuty member accounts to start monitoring.
    accountIds :: Core.NonEmpty Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMonitoringMembers' value with any optional fields omitted.
mkStartMonitoringMembers ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  StartMonitoringMembers
mkStartMonitoringMembers detectorId accountIds =
  StartMonitoringMembers' {detectorId, accountIds}

-- | The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDetectorId :: Lens.Lens' StartMonitoringMembers Types.DetectorId
sDetectorId = Lens.field @"detectorId"
{-# DEPRECATED sDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the GuardDuty member accounts to start monitoring.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAccountIds :: Lens.Lens' StartMonitoringMembers (Core.NonEmpty Types.AccountId)
sAccountIds = Lens.field @"accountIds"
{-# DEPRECATED sAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Core.FromJSON StartMonitoringMembers where
  toJSON StartMonitoringMembers {..} =
    Core.object
      (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest StartMonitoringMembers where
  type Rs StartMonitoringMembers = StartMonitoringMembersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/member/start")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMonitoringMembersResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartMonitoringMembersResponse' smart constructor.
data StartMonitoringMembersResponse = StartMonitoringMembersResponse'
  { -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMonitoringMembersResponse' value with any optional fields omitted.
mkStartMonitoringMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartMonitoringMembersResponse
mkStartMonitoringMembersResponse responseStatus =
  StartMonitoringMembersResponse'
    { unprocessedAccounts =
        Core.mempty,
      responseStatus
    }

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsUnprocessedAccounts :: Lens.Lens' StartMonitoringMembersResponse [Types.UnprocessedAccount]
srsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED srsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartMonitoringMembersResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
