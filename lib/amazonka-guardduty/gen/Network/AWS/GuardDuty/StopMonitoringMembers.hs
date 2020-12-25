{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    smmrrsUnprocessedAccounts,
    smmrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopMonitoringMembers' smart constructor.
data StopMonitoringMembers = StopMonitoringMembers'
  { -- | The unique ID of the detector associated with the GuardDuty master account that is monitoring member accounts.
    detectorId :: Types.DetectorId,
    -- | A list of account IDs for the member accounts to stop monitoring.
    accountIds :: Core.NonEmpty Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopMonitoringMembers' value with any optional fields omitted.
mkStopMonitoringMembers ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  StopMonitoringMembers
mkStopMonitoringMembers detectorId accountIds =
  StopMonitoringMembers' {detectorId, accountIds}

-- | The unique ID of the detector associated with the GuardDuty master account that is monitoring member accounts.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmDetectorId :: Lens.Lens' StopMonitoringMembers Types.DetectorId
smmDetectorId = Lens.field @"detectorId"
{-# DEPRECATED smmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs for the member accounts to stop monitoring.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmAccountIds :: Lens.Lens' StopMonitoringMembers (Core.NonEmpty Types.AccountId)
smmAccountIds = Lens.field @"accountIds"
{-# DEPRECATED smmAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Core.FromJSON StopMonitoringMembers where
  toJSON StopMonitoringMembers {..} =
    Core.object
      (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest StopMonitoringMembers where
  type Rs StopMonitoringMembers = StopMonitoringMembersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/member/stop")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopMonitoringMembersResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopMonitoringMembersResponse' smart constructor.
data StopMonitoringMembersResponse = StopMonitoringMembersResponse'
  { -- | A list of objects that contain an accountId for each account that could not be processed, and a result string that indicates why the account was not processed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopMonitoringMembersResponse' value with any optional fields omitted.
mkStopMonitoringMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopMonitoringMembersResponse
mkStopMonitoringMembersResponse responseStatus =
  StopMonitoringMembersResponse'
    { unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | A list of objects that contain an accountId for each account that could not be processed, and a result string that indicates why the account was not processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmrrsUnprocessedAccounts :: Lens.Lens' StopMonitoringMembersResponse [Types.UnprocessedAccount]
smmrrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED smmrrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmrrsResponseStatus :: Lens.Lens' StopMonitoringMembersResponse Core.Int
smmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED smmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
