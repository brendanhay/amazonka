{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    uprarcPullRequestId,
    uprarcApprovalRuleName,
    uprarcNewRuleContent,

    -- * Destructuring the response
    UpdatePullRequestApprovalRuleContentResponse (..),
    mkUpdatePullRequestApprovalRuleContentResponse,

    -- ** Response lenses
    uprarcrsResponseStatus,
    uprarcrsApprovalRule,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePullRequestApprovalRuleContent' smart constructor.
data UpdatePullRequestApprovalRuleContent = UpdatePullRequestApprovalRuleContent'
  { existingRuleContentSha256 ::
      Lude.Maybe
        Lude.Text,
    pullRequestId ::
      Lude.Text,
    approvalRuleName ::
      Lude.Text,
    newRuleContent ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestApprovalRuleContent' with the minimum fields required to make a request.
--
-- * 'approvalRuleName' - The name of the approval rule you want to update.
-- * 'existingRuleContentSha256' - The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
-- * 'newRuleContent' - The updated content for the approval rule.
-- * 'pullRequestId' - The system-generated ID of the pull request.
mkUpdatePullRequestApprovalRuleContent ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'approvalRuleName'
  Lude.Text ->
  -- | 'newRuleContent'
  Lude.Text ->
  UpdatePullRequestApprovalRuleContent
mkUpdatePullRequestApprovalRuleContent
  pPullRequestId_
  pApprovalRuleName_
  pNewRuleContent_ =
    UpdatePullRequestApprovalRuleContent'
      { existingRuleContentSha256 =
          Lude.Nothing,
        pullRequestId = pPullRequestId_,
        approvalRuleName = pApprovalRuleName_,
        newRuleContent = pNewRuleContent_
      }

-- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
--
-- /Note:/ Consider using 'existingRuleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcExistingRuleContentSha256 :: Lens.Lens' UpdatePullRequestApprovalRuleContent (Lude.Maybe Lude.Text)
uprarcExistingRuleContentSha256 = Lens.lens (existingRuleContentSha256 :: UpdatePullRequestApprovalRuleContent -> Lude.Maybe Lude.Text) (\s a -> s {existingRuleContentSha256 = a} :: UpdatePullRequestApprovalRuleContent)
{-# DEPRECATED uprarcExistingRuleContentSha256 "Use generic-lens or generic-optics with 'existingRuleContentSha256' instead." #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcPullRequestId :: Lens.Lens' UpdatePullRequestApprovalRuleContent Lude.Text
uprarcPullRequestId = Lens.lens (pullRequestId :: UpdatePullRequestApprovalRuleContent -> Lude.Text) (\s a -> s {pullRequestId = a} :: UpdatePullRequestApprovalRuleContent)
{-# DEPRECATED uprarcPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The name of the approval rule you want to update.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcApprovalRuleName :: Lens.Lens' UpdatePullRequestApprovalRuleContent Lude.Text
uprarcApprovalRuleName = Lens.lens (approvalRuleName :: UpdatePullRequestApprovalRuleContent -> Lude.Text) (\s a -> s {approvalRuleName = a} :: UpdatePullRequestApprovalRuleContent)
{-# DEPRECATED uprarcApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

-- | The updated content for the approval rule.
--
-- /Note:/ Consider using 'newRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcNewRuleContent :: Lens.Lens' UpdatePullRequestApprovalRuleContent Lude.Text
uprarcNewRuleContent = Lens.lens (newRuleContent :: UpdatePullRequestApprovalRuleContent -> Lude.Text) (\s a -> s {newRuleContent = a} :: UpdatePullRequestApprovalRuleContent)
{-# DEPRECATED uprarcNewRuleContent "Use generic-lens or generic-optics with 'newRuleContent' instead." #-}

instance Lude.AWSRequest UpdatePullRequestApprovalRuleContent where
  type
    Rs UpdatePullRequestApprovalRuleContent =
      UpdatePullRequestApprovalRuleContentResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePullRequestApprovalRuleContentResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "approvalRule")
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
            Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("approvalRuleName" Lude..= approvalRuleName),
            Lude.Just ("newRuleContent" Lude..= newRuleContent)
          ]
      )

instance Lude.ToPath UpdatePullRequestApprovalRuleContent where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePullRequestApprovalRuleContent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePullRequestApprovalRuleContentResponse' smart constructor.
data UpdatePullRequestApprovalRuleContentResponse = UpdatePullRequestApprovalRuleContentResponse'
  { responseStatus ::
      Lude.Int,
    approvalRule ::
      ApprovalRule
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestApprovalRuleContentResponse' with the minimum fields required to make a request.
--
-- * 'approvalRule' - Information about the updated approval rule.
-- * 'responseStatus' - The response status code.
mkUpdatePullRequestApprovalRuleContentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'approvalRule'
  ApprovalRule ->
  UpdatePullRequestApprovalRuleContentResponse
mkUpdatePullRequestApprovalRuleContentResponse
  pResponseStatus_
  pApprovalRule_ =
    UpdatePullRequestApprovalRuleContentResponse'
      { responseStatus =
          pResponseStatus_,
        approvalRule = pApprovalRule_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcrsResponseStatus :: Lens.Lens' UpdatePullRequestApprovalRuleContentResponse Lude.Int
uprarcrsResponseStatus = Lens.lens (responseStatus :: UpdatePullRequestApprovalRuleContentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePullRequestApprovalRuleContentResponse)
{-# DEPRECATED uprarcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about the updated approval rule.
--
-- /Note:/ Consider using 'approvalRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprarcrsApprovalRule :: Lens.Lens' UpdatePullRequestApprovalRuleContentResponse ApprovalRule
uprarcrsApprovalRule = Lens.lens (approvalRule :: UpdatePullRequestApprovalRuleContentResponse -> ApprovalRule) (\s a -> s {approvalRule = a} :: UpdatePullRequestApprovalRuleContentResponse)
{-# DEPRECATED uprarcrsApprovalRule "Use generic-lens or generic-optics with 'approvalRule' instead." #-}
