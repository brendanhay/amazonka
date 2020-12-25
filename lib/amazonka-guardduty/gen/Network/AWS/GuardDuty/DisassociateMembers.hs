{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DisassociateMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.DisassociateMembers
  ( -- * Creating a request
    DisassociateMembers (..),
    mkDisassociateMembers,

    -- ** Request lenses
    dmsDetectorId,
    dmsAccountIds,

    -- * Destructuring the response
    DisassociateMembersResponse (..),
    mkDisassociateMembersResponse,

    -- ** Response lenses
    dmrrsUnprocessedAccounts,
    dmrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateMembers' smart constructor.
data DisassociateMembers = DisassociateMembers'
  { -- | The unique ID of the detector of the GuardDuty account whose members you want to disassociate from the master account.
    detectorId :: Types.DetectorId,
    -- | A list of account IDs of the GuardDuty member accounts that you want to disassociate from the master account.
    accountIds :: Core.NonEmpty Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateMembers' value with any optional fields omitted.
mkDisassociateMembers ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  DisassociateMembers
mkDisassociateMembers detectorId accountIds =
  DisassociateMembers' {detectorId, accountIds}

-- | The unique ID of the detector of the GuardDuty account whose members you want to disassociate from the master account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsDetectorId :: Lens.Lens' DisassociateMembers Types.DetectorId
dmsDetectorId = Lens.field @"detectorId"
{-# DEPRECATED dmsDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the GuardDuty member accounts that you want to disassociate from the master account.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsAccountIds :: Lens.Lens' DisassociateMembers (Core.NonEmpty Types.AccountId)
dmsAccountIds = Lens.field @"accountIds"
{-# DEPRECATED dmsAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Core.FromJSON DisassociateMembers where
  toJSON DisassociateMembers {..} =
    Core.object
      (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest DisassociateMembers where
  type Rs DisassociateMembers = DisassociateMembersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/member/disassociate")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateMembersResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateMembersResponse' smart constructor.
data DisassociateMembersResponse = DisassociateMembersResponse'
  { -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateMembersResponse' value with any optional fields omitted.
mkDisassociateMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateMembersResponse
mkDisassociateMembersResponse responseStatus =
  DisassociateMembersResponse'
    { unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsUnprocessedAccounts :: Lens.Lens' DisassociateMembersResponse [Types.UnprocessedAccount]
dmrrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED dmrrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrrsResponseStatus :: Lens.Lens' DisassociateMembersResponse Core.Int
dmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
