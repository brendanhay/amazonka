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
    cprarApprovalRuleName,
    cprarPullRequestId,
    cprarApprovalRuleContent,

    -- * Destructuring the response
    CreatePullRequestApprovalRuleResponse (..),
    mkCreatePullRequestApprovalRuleResponse,

    -- ** Response lenses
    cprarrsApprovalRule,
    cprarrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePullRequestApprovalRule' smart constructor.
data CreatePullRequestApprovalRule = CreatePullRequestApprovalRule'
  { -- | The name for the approval rule.
    approvalRuleName :: Lude.Text,
    -- | The system-generated ID of the pull request for which you want to create the approval rule.
    pullRequestId :: Lude.Text,
    -- | The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
    approvalRuleContent :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePullRequestApprovalRule' with the minimum fields required to make a request.
--
-- * 'approvalRuleName' - The name for the approval rule.
-- * 'pullRequestId' - The system-generated ID of the pull request for which you want to create the approval rule.
-- * 'approvalRuleContent' - The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
mkCreatePullRequestApprovalRule ::
  -- | 'approvalRuleName'
  Lude.Text ->
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'approvalRuleContent'
  Lude.Text ->
  CreatePullRequestApprovalRule
mkCreatePullRequestApprovalRule
  pApprovalRuleName_
  pPullRequestId_
  pApprovalRuleContent_ =
    CreatePullRequestApprovalRule'
      { approvalRuleName =
          pApprovalRuleName_,
        pullRequestId = pPullRequestId_,
        approvalRuleContent = pApprovalRuleContent_
      }

-- | The name for the approval rule.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarApprovalRuleName :: Lens.Lens' CreatePullRequestApprovalRule Lude.Text
cprarApprovalRuleName = Lens.lens (approvalRuleName :: CreatePullRequestApprovalRule -> Lude.Text) (\s a -> s {approvalRuleName = a} :: CreatePullRequestApprovalRule)
{-# DEPRECATED cprarApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

-- | The system-generated ID of the pull request for which you want to create the approval rule.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarPullRequestId :: Lens.Lens' CreatePullRequestApprovalRule Lude.Text
cprarPullRequestId = Lens.lens (pullRequestId :: CreatePullRequestApprovalRule -> Lude.Text) (\s a -> s {pullRequestId = a} :: CreatePullRequestApprovalRule)
{-# DEPRECATED cprarPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The content of the approval rule, including the number of approvals needed and the structure of an approval pool defined for approvals, if any. For more information about approval pools, see the AWS CodeCommit User Guide.
--
-- /Note:/ Consider using 'approvalRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarApprovalRuleContent :: Lens.Lens' CreatePullRequestApprovalRule Lude.Text
cprarApprovalRuleContent = Lens.lens (approvalRuleContent :: CreatePullRequestApprovalRule -> Lude.Text) (\s a -> s {approvalRuleContent = a} :: CreatePullRequestApprovalRule)
{-# DEPRECATED cprarApprovalRuleContent "Use generic-lens or generic-optics with 'approvalRuleContent' instead." #-}

instance Lude.AWSRequest CreatePullRequestApprovalRule where
  type
    Rs CreatePullRequestApprovalRule =
      CreatePullRequestApprovalRuleResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePullRequestApprovalRuleResponse'
            Lude.<$> (x Lude..:> "approvalRule") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePullRequestApprovalRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.CreatePullRequestApprovalRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePullRequestApprovalRule where
  toJSON CreatePullRequestApprovalRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("approvalRuleName" Lude..= approvalRuleName),
            Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("approvalRuleContent" Lude..= approvalRuleContent)
          ]
      )

instance Lude.ToPath CreatePullRequestApprovalRule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePullRequestApprovalRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePullRequestApprovalRuleResponse' smart constructor.
data CreatePullRequestApprovalRuleResponse = CreatePullRequestApprovalRuleResponse'
  { -- | Information about the created approval rule.
    approvalRule :: ApprovalRule,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePullRequestApprovalRuleResponse' with the minimum fields required to make a request.
--
-- * 'approvalRule' - Information about the created approval rule.
-- * 'responseStatus' - The response status code.
mkCreatePullRequestApprovalRuleResponse ::
  -- | 'approvalRule'
  ApprovalRule ->
  -- | 'responseStatus'
  Lude.Int ->
  CreatePullRequestApprovalRuleResponse
mkCreatePullRequestApprovalRuleResponse
  pApprovalRule_
  pResponseStatus_ =
    CreatePullRequestApprovalRuleResponse'
      { approvalRule =
          pApprovalRule_,
        responseStatus = pResponseStatus_
      }

-- | Information about the created approval rule.
--
-- /Note:/ Consider using 'approvalRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarrsApprovalRule :: Lens.Lens' CreatePullRequestApprovalRuleResponse ApprovalRule
cprarrsApprovalRule = Lens.lens (approvalRule :: CreatePullRequestApprovalRuleResponse -> ApprovalRule) (\s a -> s {approvalRule = a} :: CreatePullRequestApprovalRuleResponse)
{-# DEPRECATED cprarrsApprovalRule "Use generic-lens or generic-optics with 'approvalRule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprarrsResponseStatus :: Lens.Lens' CreatePullRequestApprovalRuleResponse Lude.Int
cprarrsResponseStatus = Lens.lens (responseStatus :: CreatePullRequestApprovalRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePullRequestApprovalRuleResponse)
{-# DEPRECATED cprarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
