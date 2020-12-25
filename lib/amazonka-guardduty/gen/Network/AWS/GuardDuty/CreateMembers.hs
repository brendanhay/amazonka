{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cmrrsUnprocessedAccounts,
    cmrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateMembers' smart constructor.
data CreateMembers = CreateMembers'
  { -- | The unique ID of the detector of the GuardDuty account that you want to associate member accounts with.
    detectorId :: Types.DetectorId,
    -- | A list of account ID and email address pairs of the accounts that you want to associate with the master GuardDuty account.
    accountDetails :: Core.NonEmpty Types.AccountDetail
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMembers' value with any optional fields omitted.
mkCreateMembers ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'accountDetails'
  Core.NonEmpty Types.AccountDetail ->
  CreateMembers
mkCreateMembers detectorId accountDetails =
  CreateMembers' {detectorId, accountDetails}

-- | The unique ID of the detector of the GuardDuty account that you want to associate member accounts with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmDetectorId :: Lens.Lens' CreateMembers Types.DetectorId
cmDetectorId = Lens.field @"detectorId"
{-# DEPRECATED cmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of account ID and email address pairs of the accounts that you want to associate with the master GuardDuty account.
--
-- /Note:/ Consider using 'accountDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmAccountDetails :: Lens.Lens' CreateMembers (Core.NonEmpty Types.AccountDetail)
cmAccountDetails = Lens.field @"accountDetails"
{-# DEPRECATED cmAccountDetails "Use generic-lens or generic-optics with 'accountDetails' instead." #-}

instance Core.FromJSON CreateMembers where
  toJSON CreateMembers {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("accountDetails" Core..= accountDetails)]
      )

instance Core.AWSRequest CreateMembers where
  type Rs CreateMembers = CreateMembersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/member")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMembersResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateMembersResponse' smart constructor.
data CreateMembersResponse = CreateMembersResponse'
  { -- | A list of objects that include the @accountIds@ of the unprocessed accounts and a result string that explains why each was unprocessed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMembersResponse' value with any optional fields omitted.
mkCreateMembersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateMembersResponse
mkCreateMembersResponse responseStatus =
  CreateMembersResponse'
    { unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | A list of objects that include the @accountIds@ of the unprocessed accounts and a result string that explains why each was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsUnprocessedAccounts :: Lens.Lens' CreateMembersResponse [Types.UnprocessedAccount]
cmrrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED cmrrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrrsResponseStatus :: Lens.Lens' CreateMembersResponse Core.Int
cmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
