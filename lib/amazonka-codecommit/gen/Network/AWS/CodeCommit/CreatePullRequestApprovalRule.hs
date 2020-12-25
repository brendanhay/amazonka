{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreatePullRequestApprovalRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an approval rule for a pull request.
module Network.AWS.CodeCommit.CreatePullRequestApprovalRule
  ( -- * Creating a request
    CreatePullRequestApprovalRule (..),
    mkCreatePullRequestApprovalRule,

    -- ** Request lenses
    cprarPullRequestId,
    cprarApprovalRuleName,
    cprarApprovalRuleContent,

    -- * Destructuring the response
    CreatePullRequestApprovalRuleResponse (..),
    mkCreatePullRequestApprovalRuleResponse,

    -- ** Response lenses
    cprarrrsApprovalRule,
    cprarrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePullRequestApprovalRule' smart constructor.
data CreatePullRequestApprovalRule = CreatePullRequestApprovalRule'
  { -- | The system-generated ID of the pull request for which you want to create the approval rule.
    pullRequestId :: Types.PullRequestId,
    -- | The name for the approval rule.
    approvalRuleName :: Types.ApprovalRuleName,
    -- | The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
    approvalRuleContent :: Types.ApprovalRuleContent
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePullRequestApprovalRule' value with any optional fields omitted.
mkCreatePullRequestApprovalRule ::
  -- | 'pullRequestId'
  Types.PullRequestId ->
  -- | 'approvalRuleName'
  Types.ApprovalRuleName ->
  -- | 'approvalRuleContent'
  Types.ApprovalRuleContent ->
  CreatePullRequestApprovalRule
mkCreatePullRequestApprovalRule
  pullRequestId
  approvalRuleName
  approvalRuleContent =
    CreatePullRequestApprovalRule'
      { pullRequestId,
        approvalRuleName,
        approvalRuleContent
      }

-- | The system-generated ID of the pull request for which you want to create the approval rule.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarPullRequestId :: Lens.Lens' CreatePullRequestApprovalRule Types.PullRequestId
cprarPullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED cprarPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The name for the approval rule.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarApprovalRuleName :: Lens.Lens' CreatePullRequestApprovalRule Types.ApprovalRuleName
cprarApprovalRuleName = Lens.field @"approvalRuleName"
{-# DEPRECATED cprarApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

-- | The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
--
-- /Note:/ Consider using 'approvalRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarApprovalRuleContent :: Lens.Lens' CreatePullRequestApprovalRule Types.ApprovalRuleContent
cprarApprovalRuleContent = Lens.field @"approvalRuleContent"
{-# DEPRECATED cprarApprovalRuleContent "Use generic-lens or generic-optics with 'approvalRuleContent' instead." #-}

instance Core.FromJSON CreatePullRequestApprovalRule where
  toJSON CreatePullRequestApprovalRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("approvalRuleName" Core..= approvalRuleName),
            Core.Just ("approvalRuleContent" Core..= approvalRuleContent)
          ]
      )

instance Core.AWSRequest CreatePullRequestApprovalRule where
  type
    Rs CreatePullRequestApprovalRule =
      CreatePullRequestApprovalRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.CreatePullRequestApprovalRule"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePullRequestApprovalRuleResponse'
            Core.<$> (x Core..: "approvalRule") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePullRequestApprovalRuleResponse' smart constructor.
data CreatePullRequestApprovalRuleResponse = CreatePullRequestApprovalRuleResponse'
  { -- | Information about the created approval rule.
    approvalRule :: Types.ApprovalRule,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreatePullRequestApprovalRuleResponse' value with any optional fields omitted.
mkCreatePullRequestApprovalRuleResponse ::
  -- | 'approvalRule'
  Types.ApprovalRule ->
  -- | 'responseStatus'
  Core.Int ->
  CreatePullRequestApprovalRuleResponse
mkCreatePullRequestApprovalRuleResponse approvalRule responseStatus =
  CreatePullRequestApprovalRuleResponse'
    { approvalRule,
      responseStatus
    }

-- | Information about the created approval rule.
--
-- /Note:/ Consider using 'approvalRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarrrsApprovalRule :: Lens.Lens' CreatePullRequestApprovalRuleResponse Types.ApprovalRule
cprarrrsApprovalRule = Lens.field @"approvalRule"
{-# DEPRECATED cprarrrsApprovalRule "Use generic-lens or generic-optics with 'approvalRule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarrrsResponseStatus :: Lens.Lens' CreatePullRequestApprovalRuleResponse Core.Int
cprarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
