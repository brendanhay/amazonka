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
    dmDetectorId,
    dmAccountIds,

    -- * Destructuring the response
    DeleteMembersResponse (..),
    mkDeleteMembersResponse,

    -- ** Response lenses
    drsUnprocessedAccounts,
    drsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { -- | The unique ID of the detector of the GuardDuty account whose members you want to delete.
    detectorId :: Types.DetectorId,
    -- | A list of account IDs of the GuardDuty member accounts that you want to delete.
    accountIds :: Core.NonEmpty Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMembers' value with any optional fields omitted.
mkDeleteMembers ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  DeleteMembers
mkDeleteMembers detectorId accountIds =
  DeleteMembers' {detectorId, accountIds}

-- | The unique ID of the detector of the GuardDuty account whose members you want to delete.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDetectorId :: Lens.Lens' DeleteMembers Types.DetectorId
dmDetectorId = Lens.field @"detectorId"
{-# DEPRECATED dmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the GuardDuty member accounts that you want to delete.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmAccountIds :: Lens.Lens' DeleteMembers (Core.NonEmpty Types.AccountId)
dmAccountIds = Lens.field @"accountIds"
{-# DEPRECATED dmAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Core.FromJSON DeleteMembers where
  toJSON DeleteMembers {..} =
    Core.object
      (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest DeleteMembers where
  type Rs DeleteMembers = DeleteMembersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/member/delete")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMembersResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteMembersResponse' smart constructor.
data DeleteMembersResponse = DeleteMembersResponse'
  { -- | The accounts that could not be processed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMembersResponse' value with any optional fields omitted.
mkDeleteMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMembersResponse
mkDeleteMembersResponse responseStatus =
  DeleteMembersResponse'
    { unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | The accounts that could not be processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsUnprocessedAccounts :: Lens.Lens' DeleteMembersResponse [Types.UnprocessedAccount]
drsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED drsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteMembersResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
