{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dprarrsResponseStatus,
    dprarrsApprovalRuleId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePullRequestApprovalRule' smart constructor.
data DeletePullRequestApprovalRule = DeletePullRequestApprovalRule'
  { pullRequestId ::
      Lude.Text,
    approvalRuleName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePullRequestApprovalRule' with the minimum fields required to make a request.
--
-- * 'approvalRuleName' - The name of the approval rule you want to delete.
-- * 'pullRequestId' - The system-generated ID of the pull request that contains the approval rule you want to delete.
mkDeletePullRequestApprovalRule ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'approvalRuleName'
  Lude.Text ->
  DeletePullRequestApprovalRule
mkDeletePullRequestApprovalRule pPullRequestId_ pApprovalRuleName_ =
  DeletePullRequestApprovalRule'
    { pullRequestId = pPullRequestId_,
      approvalRuleName = pApprovalRuleName_
    }

-- | The system-generated ID of the pull request that contains the approval rule you want to delete.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprarPullRequestId :: Lens.Lens' DeletePullRequestApprovalRule Lude.Text
dprarPullRequestId = Lens.lens (pullRequestId :: DeletePullRequestApprovalRule -> Lude.Text) (\s a -> s {pullRequestId = a} :: DeletePullRequestApprovalRule)
{-# DEPRECATED dprarPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The name of the approval rule you want to delete.
--
-- /Note:/ Consider using 'approvalRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprarApprovalRuleName :: Lens.Lens' DeletePullRequestApprovalRule Lude.Text
dprarApprovalRuleName = Lens.lens (approvalRuleName :: DeletePullRequestApprovalRule -> Lude.Text) (\s a -> s {approvalRuleName = a} :: DeletePullRequestApprovalRule)
{-# DEPRECATED dprarApprovalRuleName "Use generic-lens or generic-optics with 'approvalRuleName' instead." #-}

instance Lude.AWSRequest DeletePullRequestApprovalRule where
  type
    Rs DeletePullRequestApprovalRule =
      DeletePullRequestApprovalRuleResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeletePullRequestApprovalRuleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "approvalRuleId")
      )

instance Lude.ToHeaders DeletePullRequestApprovalRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.DeletePullRequestApprovalRule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePullRequestApprovalRule where
  toJSON DeletePullRequestApprovalRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("approvalRuleName" Lude..= approvalRuleName)
          ]
      )

instance Lude.ToPath DeletePullRequestApprovalRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePullRequestApprovalRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePullRequestApprovalRuleResponse' smart constructor.
data DeletePullRequestApprovalRuleResponse = DeletePullRequestApprovalRuleResponse'
  { responseStatus ::
      Lude.Int,
    approvalRuleId ::
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

-- | Creates a value of 'DeletePullRequestApprovalRuleResponse' with the minimum fields required to make a request.
--
-- * 'approvalRuleId' - The ID of the deleted approval rule.
-- * 'responseStatus' - The response status code.
mkDeletePullRequestApprovalRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'approvalRuleId'
  Lude.Text ->
  DeletePullRequestApprovalRuleResponse
mkDeletePullRequestApprovalRuleResponse
  pResponseStatus_
  pApprovalRuleId_ =
    DeletePullRequestApprovalRuleResponse'
      { responseStatus =
          pResponseStatus_,
        approvalRuleId = pApprovalRuleId_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprarrsResponseStatus :: Lens.Lens' DeletePullRequestApprovalRuleResponse Lude.Int
dprarrsResponseStatus = Lens.lens (responseStatus :: DeletePullRequestApprovalRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePullRequestApprovalRuleResponse)
{-# DEPRECATED dprarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The ID of the deleted approval rule.
--
-- /Note:/ Consider using 'approvalRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprarrsApprovalRuleId :: Lens.Lens' DeletePullRequestApprovalRuleResponse Lude.Text
dprarrsApprovalRuleId = Lens.lens (approvalRuleId :: DeletePullRequestApprovalRuleResponse -> Lude.Text) (\s a -> s {approvalRuleId = a} :: DeletePullRequestApprovalRuleResponse)
{-# DEPRECATED dprarrsApprovalRuleId "Use generic-lens or generic-optics with 'approvalRuleId' instead." #-}
