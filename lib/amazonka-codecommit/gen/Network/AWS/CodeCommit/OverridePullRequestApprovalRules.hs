{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.OverridePullRequestApprovalRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets aside (overrides) all approval rule requirements for a specified pull request.
module Network.AWS.CodeCommit.OverridePullRequestApprovalRules
  ( -- * Creating a request
    OverridePullRequestApprovalRules (..),
    mkOverridePullRequestApprovalRules,

    -- ** Request lenses
    oprarPullRequestId,
    oprarRevisionId,
    oprarOverrideStatus,

    -- * Destructuring the response
    OverridePullRequestApprovalRulesResponse (..),
    mkOverridePullRequestApprovalRulesResponse,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkOverridePullRequestApprovalRules' smart constructor.
data OverridePullRequestApprovalRules = OverridePullRequestApprovalRules'
  { -- | The system-generated ID of the pull request for which you want to override all approval rule requirements. To get this information, use 'GetPullRequest' .
    pullRequestId :: Types.PullRequestId,
    -- | The system-generated ID of the most recent revision of the pull request. You cannot override approval rules for anything but the most recent revision of a pull request. To get the revision ID, use GetPullRequest.
    revisionId :: Types.RevisionId,
    -- | Whether you want to set aside approval rule requirements for the pull request (OVERRIDE) or revoke a previous override and apply approval rule requirements (REVOKE). REVOKE status is not stored.
    overrideStatus :: Types.OverrideStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OverridePullRequestApprovalRules' value with any optional fields omitted.
mkOverridePullRequestApprovalRules ::
  -- | 'pullRequestId'
  Types.PullRequestId ->
  -- | 'revisionId'
  Types.RevisionId ->
  -- | 'overrideStatus'
  Types.OverrideStatus ->
  OverridePullRequestApprovalRules
mkOverridePullRequestApprovalRules
  pullRequestId
  revisionId
  overrideStatus =
    OverridePullRequestApprovalRules'
      { pullRequestId,
        revisionId,
        overrideStatus
      }

-- | The system-generated ID of the pull request for which you want to override all approval rule requirements. To get this information, use 'GetPullRequest' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oprarPullRequestId :: Lens.Lens' OverridePullRequestApprovalRules Types.PullRequestId
oprarPullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED oprarPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The system-generated ID of the most recent revision of the pull request. You cannot override approval rules for anything but the most recent revision of a pull request. To get the revision ID, use GetPullRequest.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oprarRevisionId :: Lens.Lens' OverridePullRequestApprovalRules Types.RevisionId
oprarRevisionId = Lens.field @"revisionId"
{-# DEPRECATED oprarRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | Whether you want to set aside approval rule requirements for the pull request (OVERRIDE) or revoke a previous override and apply approval rule requirements (REVOKE). REVOKE status is not stored.
--
-- /Note:/ Consider using 'overrideStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oprarOverrideStatus :: Lens.Lens' OverridePullRequestApprovalRules Types.OverrideStatus
oprarOverrideStatus = Lens.field @"overrideStatus"
{-# DEPRECATED oprarOverrideStatus "Use generic-lens or generic-optics with 'overrideStatus' instead." #-}

instance Core.FromJSON OverridePullRequestApprovalRules where
  toJSON OverridePullRequestApprovalRules {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("revisionId" Core..= revisionId),
            Core.Just ("overrideStatus" Core..= overrideStatus)
          ]
      )

instance Core.AWSRequest OverridePullRequestApprovalRules where
  type
    Rs OverridePullRequestApprovalRules =
      OverridePullRequestApprovalRulesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.OverridePullRequestApprovalRules"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull OverridePullRequestApprovalRulesResponse'

-- | /See:/ 'mkOverridePullRequestApprovalRulesResponse' smart constructor.
data OverridePullRequestApprovalRulesResponse = OverridePullRequestApprovalRulesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OverridePullRequestApprovalRulesResponse' value with any optional fields omitted.
mkOverridePullRequestApprovalRulesResponse ::
  OverridePullRequestApprovalRulesResponse
mkOverridePullRequestApprovalRulesResponse =
  OverridePullRequestApprovalRulesResponse'
