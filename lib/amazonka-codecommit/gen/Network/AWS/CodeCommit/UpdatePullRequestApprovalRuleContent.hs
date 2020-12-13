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
    uprarcExistingRuleContentSha256,
    uprarcNewRuleContent,
    uprarcApprovalRuleName,
    uprarcPullRequestId,

    -- * Destructuring the response
    UpdatePullRequestApprovalRuleContentResponse (..),
    mkUpdatePullRequestApprovalRuleContentResponse,

    -- ** Response lenses
    uprarcrsApprovalRule,
    uprarcrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePullRequestApprovalRuleContent' smart constructor.
data UpdatePullRequestApprovalRuleContent = UpdatePullRequestApprovalRuleContent'
  { -- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
    existingRuleContentSha256 :: Lude.Maybe Lude.Text,
    -- | The updated content for the approval rule.
    newRuleContent :: Lude.Text,
    -- | The name of the approval rule you want to update.
    approvalRuleName :: Lude.Text,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestApprovalRuleContent' with the minimum fields required to make a request.
--
-- * 'existingRuleContentSha256' - The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
-- * 'newRuleContent' - The updated content for the approval rule.
-- * 'approvalRuleName' - The name of the approval rule you want to update.
-- * 'pullRequestId' - The system-generated ID of the pull request.
mkUpdatePullRequestApprovalRuleContent ::
  -- | 'newRuleContent'
  Lude.Text ->
  -- | 'approvalRuleName'
  Lude.Text ->
  -- | 'pullRequestId'
  Lude.Text ->
  UpdatePullRequestApprovalRuleContent
mkUpdatePullRequestApprovalRuleContent
  pNewRuleContent_
  pApprovalRuleName_
  pPullRequestId_ =
    UpdatePullRequestApprovalRuleContent'
      { existingRuleContentSha256 =
          Lude.Nothing,
        newRuleContent = pNewRuleContent_,
        approvalRuleName = pApprovalRuleName_,
        pullRequestId = pPullRequestId_
      }

-- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
--
-- /Note:/ Consider using 'existingRuleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcExistingRuleContentSha256 :: Lens.Lens' UpdatePullRequestApprovalRuleContent (Lude.Maybe Lude.Text)
uprarcExistingRuleContentSha256 = Lens.lens (existingRuleContentSha256 :: UpdatePullRequestApprovalRuleContent -> Lude.Maybe Lude.Text) (\s a -> s {existingRuleContentSha256 = a} :: UpdatePullRequestApprovalRuleContent)
{-# DEPRECATED uprarcExistingRuleContentSha256 "Use generic-lens or generic-optics with 'existingRuleContentSha256' instead." #-}

-- | The updated content for the approval rule.
--
-- /Note:/ Consider using 'newRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcNewRuleContent :: Lens.Lens' UpdatePullRequestApprovalRuleContent Lude.Text
uprarcNewRuleContent = Lens.lens (newRuleContent :: UpdatePullRequestApprovalRuleContent -> Lude.Text) (\s a -> s {newRuleContent = a} :: UpdatePullRequestApprovalRuleContent)
{-# DEPRECATED uprarcNewRuleContent "Use generic-lens or generic-optics with 'newRuleContent' instead." #-}

-- | The name of the approval rule you want to update.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcApprovalRuleName :: Lens.Lens' UpdatePullRequestApprovalRuleContent Lude.Text
uprarcApprovalRuleName = Lens.lens (approvalRuleName :: UpdatePullRequestApprovalRuleContent -> Lude.Text) (\s a -> s {approvalRuleName = a} :: UpdatePullRequestApprovalRuleContent)
{-# DEPRECATED uprarcApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcPullRequestId :: Lens.Lens' UpdatePullRequestApprovalRuleContent Lude.Text
uprarcPullRequestId = Lens.lens (pullRequestId :: UpdatePullRequestApprovalRuleContent -> Lude.Text) (\s a -> s {pullRequestId = a} :: UpdatePullRequestApprovalRuleContent)
{-# DEPRECATED uprarcPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

instance Lude.AWSRequest UpdatePullRequestApprovalRuleContent where
  type
    Rs UpdatePullRequestApprovalRuleContent =
      UpdatePullRequestApprovalRuleContentResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePullRequestApprovalRuleContentResponse'
            Lude.<$> (x Lude..:> "approvalRule") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePullRequestApprovalRuleContent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.UpdatePullRequestApprovalRuleContent" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePullRequestApprovalRuleContent where
  toJSON UpdatePullRequestApprovalRuleContent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("existingRuleContentSha256" Lude..=)
              Lude.<$> existingRuleContentSha256,
            Lude.Just ("newRuleContent" Lude..= newRuleContent),
            Lude.Just ("approvalRuleName" Lude..= approvalRuleName),
            Lude.Just ("pullRequestId" Lude..= pullRequestId)
          ]
      )

instance Lude.ToPath UpdatePullRequestApprovalRuleContent where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePullRequestApprovalRuleContent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePullRequestApprovalRuleContentResponse' smart constructor.
data UpdatePullRequestApprovalRuleContentResponse = UpdatePullRequestApprovalRuleContentResponse'
  { -- | Information about the updated approval rule.
    approvalRule :: ApprovalRule,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestApprovalRuleContentResponse' with the minimum fields required to make a request.
--
-- * 'approvalRule' - Information about the updated approval rule.
-- * 'responseStatus' - The response status code.
mkUpdatePullRequestApprovalRuleContentResponse ::
  -- | 'approvalRule'
  ApprovalRule ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePullRequestApprovalRuleContentResponse
mkUpdatePullRequestApprovalRuleContentResponse
  pApprovalRule_
  pResponseStatus_ =
    UpdatePullRequestApprovalRuleContentResponse'
      { approvalRule =
          pApprovalRule_,
        responseStatus = pResponseStatus_
      }

-- | Information about the updated approval rule.
--
-- /Note:/ Consider using 'approvalRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcrsApprovalRule :: Lens.Lens' UpdatePullRequestApprovalRuleContentResponse ApprovalRule
uprarcrsApprovalRule = Lens.lens (approvalRule :: UpdatePullRequestApprovalRuleContentResponse -> ApprovalRule) (\s a -> s {approvalRule = a} :: UpdatePullRequestApprovalRuleContentResponse)
{-# DEPRECATED uprarcrsApprovalRule "Use generic-lens or generic-optics with 'approvalRule' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcrsResponseStatus :: Lens.Lens' UpdatePullRequestApprovalRuleContentResponse Lude.Int
uprarcrsResponseStatus = Lens.lens (responseStatus :: UpdatePullRequestApprovalRuleContentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePullRequestApprovalRuleContentResponse)
{-# DEPRECATED uprarcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
