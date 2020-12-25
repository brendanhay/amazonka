{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the structure of an approval rule created specifically for a pull request. For example, you can change the number of required approvers and the approval pool for approvers.
module Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent
  ( -- * Creating a request
    UpdatePullRequestApprovalRuleContent (..),
    mkUpdatePullRequestApprovalRuleContent,

    -- ** Request lenses
    uprarcPullRequestId,
    uprarcApprovalRuleName,
    uprarcNewRuleContent,
    uprarcExistingRuleContentSha256,

    -- * Destructuring the response
    UpdatePullRequestApprovalRuleContentResponse (..),
    mkUpdatePullRequestApprovalRuleContentResponse,

    -- ** Response lenses
    uprarcrrsApprovalRule,
    uprarcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePullRequestApprovalRuleContent' smart constructor.
data UpdatePullRequestApprovalRuleContent = UpdatePullRequestApprovalRuleContent'
  { -- | The system-generated ID of the pull request.
    pullRequestId :: Types.PullRequestId,
    -- | The name of the approval rule you want to update.
    approvalRuleName :: Types.ApprovalRuleName,
    -- | The updated content for the approval rule.
    newRuleContent :: Types.ApprovalRuleContent,
    -- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
    existingRuleContentSha256 :: Core.Maybe Types.RuleContentSha256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePullRequestApprovalRuleContent' value with any optional fields omitted.
mkUpdatePullRequestApprovalRuleContent ::
  -- | 'pullRequestId'
  Types.PullRequestId ->
  -- | 'approvalRuleName'
  Types.ApprovalRuleName ->
  -- | 'newRuleContent'
  Types.ApprovalRuleContent ->
  UpdatePullRequestApprovalRuleContent
mkUpdatePullRequestApprovalRuleContent
  pullRequestId
  approvalRuleName
  newRuleContent =
    UpdatePullRequestApprovalRuleContent'
      { pullRequestId,
        approvalRuleName,
        newRuleContent,
        existingRuleContentSha256 = Core.Nothing
      }

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcPullRequestId :: Lens.Lens' UpdatePullRequestApprovalRuleContent Types.PullRequestId
uprarcPullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED uprarcPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The name of the approval rule you want to update.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcApprovalRuleName :: Lens.Lens' UpdatePullRequestApprovalRuleContent Types.ApprovalRuleName
uprarcApprovalRuleName = Lens.field @"approvalRuleName"
{-# DEPRECATED uprarcApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

-- | The updated content for the approval rule.
--
-- /Note:/ Consider using 'newRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcNewRuleContent :: Lens.Lens' UpdatePullRequestApprovalRuleContent Types.ApprovalRuleContent
uprarcNewRuleContent = Lens.field @"newRuleContent"
{-# DEPRECATED uprarcNewRuleContent "Use generic-lens or generic-optics with 'newRuleContent' instead." #-}

-- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
--
-- /Note:/ Consider using 'existingRuleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcExistingRuleContentSha256 :: Lens.Lens' UpdatePullRequestApprovalRuleContent (Core.Maybe Types.RuleContentSha256)
uprarcExistingRuleContentSha256 = Lens.field @"existingRuleContentSha256"
{-# DEPRECATED uprarcExistingRuleContentSha256 "Use generic-lens or generic-optics with 'existingRuleContentSha256' instead." #-}

instance Core.FromJSON UpdatePullRequestApprovalRuleContent where
  toJSON UpdatePullRequestApprovalRuleContent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("approvalRuleName" Core..= approvalRuleName),
            Core.Just ("newRuleContent" Core..= newRuleContent),
            ("existingRuleContentSha256" Core..=)
              Core.<$> existingRuleContentSha256
          ]
      )

instance Core.AWSRequest UpdatePullRequestApprovalRuleContent where
  type
    Rs UpdatePullRequestApprovalRuleContent =
      UpdatePullRequestApprovalRuleContentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.UpdatePullRequestApprovalRuleContent"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestApprovalRuleContentResponse'
            Core.<$> (x Core..: "approvalRule") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdatePullRequestApprovalRuleContentResponse' smart constructor.
data UpdatePullRequestApprovalRuleContentResponse = UpdatePullRequestApprovalRuleContentResponse'
  { -- | Information about the updated approval rule.
    approvalRule :: Types.ApprovalRule,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdatePullRequestApprovalRuleContentResponse' value with any optional fields omitted.
mkUpdatePullRequestApprovalRuleContentResponse ::
  -- | 'approvalRule'
  Types.ApprovalRule ->
  -- | 'responseStatus'
  Core.Int ->
  UpdatePullRequestApprovalRuleContentResponse
mkUpdatePullRequestApprovalRuleContentResponse
  approvalRule
  responseStatus =
    UpdatePullRequestApprovalRuleContentResponse'
      { approvalRule,
        responseStatus
      }

-- | Information about the updated approval rule.
--
-- /Note:/ Consider using 'approvalRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcrrsApprovalRule :: Lens.Lens' UpdatePullRequestApprovalRuleContentResponse Types.ApprovalRule
uprarcrrsApprovalRule = Lens.field @"approvalRule"
{-# DEPRECATED uprarcrrsApprovalRule "Use generic-lens or generic-optics with 'approvalRule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcrrsResponseStatus :: Lens.Lens' UpdatePullRequestApprovalRuleContentResponse Core.Int
uprarcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprarcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
