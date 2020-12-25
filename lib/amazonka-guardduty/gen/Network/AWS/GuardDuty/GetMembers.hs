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
    gmDetectorId,
    gmAccountIds,

    -- * Destructuring the response
    GetMembersResponse (..),
    mkGetMembersResponse,

    -- ** Response lenses
    gmrrsMembers,
    gmrrsUnprocessedAccounts,
    gmrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMembers' smart constructor.
data GetMembers = GetMembers'
  { -- | The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
    detectorId :: Types.DetectorId,
    -- | A list of account IDs of the GuardDuty member accounts that you want to describe.
    accountIds :: Core.NonEmpty Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMembers' value with any optional fields omitted.
mkGetMembers ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  GetMembers
mkGetMembers detectorId accountIds =
  GetMembers' {detectorId, accountIds}

-- | The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmDetectorId :: Lens.Lens' GetMembers Types.DetectorId
gmDetectorId = Lens.field @"detectorId"
{-# DEPRECATED gmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account IDs of the GuardDuty member accounts that you want to describe.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmAccountIds :: Lens.Lens' GetMembers (Core.NonEmpty Types.AccountId)
gmAccountIds = Lens.field @"accountIds"
{-# DEPRECATED gmAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Core.FromJSON GetMembers where
  toJSON GetMembers {..} =
    Core.object
      (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest GetMembers where
  type Rs GetMembers = GetMembersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/member/get")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMembersResponse'
            Core.<$> (x Core..:? "members" Core..!= Core.mempty)
            Core.<*> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMembersResponse' smart constructor.
data GetMembersResponse = GetMembersResponse'
  { -- | A list of members.
    members :: [Types.Member],
    -- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMembersResponse' value with any optional fields omitted.
mkGetMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMembersResponse
mkGetMembersResponse responseStatus =
  GetMembersResponse'
    { members = Core.mempty,
      unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | A list of members.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsMembers :: Lens.Lens' GetMembersResponse [Types.Member]
gmrrsMembers = Lens.field @"members"
{-# DEPRECATED gmrrsMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsUnprocessedAccounts :: Lens.Lens' GetMembersResponse [Types.UnprocessedAccount]
gmrrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED gmrrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsResponseStatus :: Lens.Lens' GetMembersResponse Core.Int
gmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
