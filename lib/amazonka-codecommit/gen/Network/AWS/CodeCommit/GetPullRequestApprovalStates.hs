{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetPullRequestApprovalStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the approval states for a specified pull request. Approval states only apply to pull requests that have one or more approval rules applied to them.
module Network.AWS.CodeCommit.GetPullRequestApprovalStates
  ( -- * Creating a request
    GetPullRequestApprovalStates (..),
    mkGetPullRequestApprovalStates,

    -- ** Request lenses
    gprasPullRequestId,
    gprasRevisionId,

    -- * Destructuring the response
    GetPullRequestApprovalStatesResponse (..),
    mkGetPullRequestApprovalStatesResponse,

    -- ** Response lenses
    gprasrsApprovals,
    gprasrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPullRequestApprovalStates' smart constructor.
data GetPullRequestApprovalStates = GetPullRequestApprovalStates'
  { pullRequestId ::
      Lude.Text,
    revisionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPullRequestApprovalStates' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The system-generated ID for the pull request.
-- * 'revisionId' - The system-generated ID for the pull request revision.
mkGetPullRequestApprovalStates ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'revisionId'
  Lude.Text ->
  GetPullRequestApprovalStates
mkGetPullRequestApprovalStates pPullRequestId_ pRevisionId_ =
  GetPullRequestApprovalStates'
    { pullRequestId = pPullRequestId_,
      revisionId = pRevisionId_
    }

-- | The system-generated ID for the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprasPullRequestId :: Lens.Lens' GetPullRequestApprovalStates Lude.Text
gprasPullRequestId = Lens.lens (pullRequestId :: GetPullRequestApprovalStates -> Lude.Text) (\s a -> s {pullRequestId = a} :: GetPullRequestApprovalStates)
{-# DEPRECATED gprasPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The system-generated ID for the pull request revision.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprasRevisionId :: Lens.Lens' GetPullRequestApprovalStates Lude.Text
gprasRevisionId = Lens.lens (revisionId :: GetPullRequestApprovalStates -> Lude.Text) (\s a -> s {revisionId = a} :: GetPullRequestApprovalStates)
{-# DEPRECATED gprasRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest GetPullRequestApprovalStates where
  type
    Rs GetPullRequestApprovalStates =
      GetPullRequestApprovalStatesResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPullRequestApprovalStatesResponse'
            Lude.<$> (x Lude..?> "approvals" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPullRequestApprovalStates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.GetPullRequestApprovalStates" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPullRequestApprovalStates where
  toJSON GetPullRequestApprovalStates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("revisionId" Lude..= revisionId)
          ]
      )

instance Lude.ToPath GetPullRequestApprovalStates where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPullRequestApprovalStates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPullRequestApprovalStatesResponse' smart constructor.
data GetPullRequestApprovalStatesResponse = GetPullRequestApprovalStatesResponse'
  { approvals ::
      Lude.Maybe
        [Approval],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPullRequestApprovalStatesResponse' with the minimum fields required to make a request.
--
-- * 'approvals' - Information about users who have approved the pull request.
-- * 'responseStatus' - The response status code.
mkGetPullRequestApprovalStatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPullRequestApprovalStatesResponse
mkGetPullRequestApprovalStatesResponse pResponseStatus_ =
  GetPullRequestApprovalStatesResponse'
    { approvals = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about users who have approved the pull request.
--
-- /Note:/ Consider using 'approvals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprasrsApprovals :: Lens.Lens' GetPullRequestApprovalStatesResponse (Lude.Maybe [Approval])
gprasrsApprovals = Lens.lens (approvals :: GetPullRequestApprovalStatesResponse -> Lude.Maybe [Approval]) (\s a -> s {approvals = a} :: GetPullRequestApprovalStatesResponse)
{-# DEPRECATED gprasrsApprovals "Use generic-lens or generic-optics with 'approvals' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprasrsResponseStatus :: Lens.Lens' GetPullRequestApprovalStatesResponse Lude.Int
gprasrsResponseStatus = Lens.lens (responseStatus :: GetPullRequestApprovalStatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPullRequestApprovalStatesResponse)
{-# DEPRECATED gprasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
