{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeletePullRequestApprovalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an approval rule from a specified pull request. Approval rules can be deleted from a pull request only if the pull request is open, and if the approval rule was created specifically for a pull request and not generated from an approval rule template associated with the repository where the pull request was created. You cannot delete an approval rule from a merged or closed pull request.
module Network.AWS.CodeCommit.DeletePullRequestApprovalRule
  ( -- * Creating a request
    DeletePullRequestApprovalRule (..),
    mkDeletePullRequestApprovalRule,

    -- ** Request lenses
    dprarPullRequestId,
    dprarApprovalRuleName,

    -- * Destructuring the response
    DeletePullRequestApprovalRuleResponse (..),
    mkDeletePullRequestApprovalRuleResponse,

    -- ** Response lenses
    dprarrrsApprovalRuleId,
    dprarrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeletePullRequestApprovalRule' smart constructor.
data DeletePullRequestApprovalRule = DeletePullRequestApprovalRule'
  { -- | The system-generated ID of the pull request that contains the approval rule you want to delete.
    pullRequestId :: Types.PullRequestId,
    -- | The name of the approval rule you want to delete.
    approvalRuleName :: Types.ApprovalRuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePullRequestApprovalRule' value with any optional fields omitted.
mkDeletePullRequestApprovalRule ::
  -- | 'pullRequestId'
  Types.PullRequestId ->
  -- | 'approvalRuleName'
  Types.ApprovalRuleName ->
  DeletePullRequestApprovalRule
mkDeletePullRequestApprovalRule pullRequestId approvalRuleName =
  DeletePullRequestApprovalRule' {pullRequestId, approvalRuleName}

-- | The system-generated ID of the pull request that contains the approval rule you want to delete.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprarPullRequestId :: Lens.Lens' DeletePullRequestApprovalRule Types.PullRequestId
dprarPullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED dprarPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The name of the approval rule you want to delete.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprarApprovalRuleName :: Lens.Lens' DeletePullRequestApprovalRule Types.ApprovalRuleName
dprarApprovalRuleName = Lens.field @"approvalRuleName"
{-# DEPRECATED dprarApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

instance Core.FromJSON DeletePullRequestApprovalRule where
  toJSON DeletePullRequestApprovalRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("approvalRuleName" Core..= approvalRuleName)
          ]
      )

instance Core.AWSRequest DeletePullRequestApprovalRule where
  type
    Rs DeletePullRequestApprovalRule =
      DeletePullRequestApprovalRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.DeletePullRequestApprovalRule"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePullRequestApprovalRuleResponse'
            Core.<$> (x Core..: "approvalRuleId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeletePullRequestApprovalRuleResponse' smart constructor.
data DeletePullRequestApprovalRuleResponse = DeletePullRequestApprovalRuleResponse'
  { -- | The ID of the deleted approval rule.
    approvalRuleId :: Types.ApprovalRuleId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePullRequestApprovalRuleResponse' value with any optional fields omitted.
mkDeletePullRequestApprovalRuleResponse ::
  -- | 'approvalRuleId'
  Types.ApprovalRuleId ->
  -- | 'responseStatus'
  Core.Int ->
  DeletePullRequestApprovalRuleResponse
mkDeletePullRequestApprovalRuleResponse
  approvalRuleId
  responseStatus =
    DeletePullRequestApprovalRuleResponse'
      { approvalRuleId,
        responseStatus
      }

-- | The ID of the deleted approval rule.
--
-- /Note:/ Consider using 'approvalRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprarrrsApprovalRuleId :: Lens.Lens' DeletePullRequestApprovalRuleResponse Types.ApprovalRuleId
dprarrrsApprovalRuleId = Lens.field @"approvalRuleId"
{-# DEPRECATED dprarrrsApprovalRuleId "Use generic-lens or generic-optics with 'approvalRuleId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprarrrsResponseStatus :: Lens.Lens' DeletePullRequestApprovalRuleResponse Core.Int
dprarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
