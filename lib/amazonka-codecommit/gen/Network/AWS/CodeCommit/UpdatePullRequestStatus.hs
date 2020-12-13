{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a pull request.
module Network.AWS.CodeCommit.UpdatePullRequestStatus
  ( -- * Creating a request
    UpdatePullRequestStatus (..),
    mkUpdatePullRequestStatus,

    -- ** Request lenses
    uprsPullRequestId,
    uprsPullRequestStatus,

    -- * Destructuring the response
    UpdatePullRequestStatusResponse (..),
    mkUpdatePullRequestStatusResponse,

    -- ** Response lenses
    uprsrsPullRequest,
    uprsrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePullRequestStatus' smart constructor.
data UpdatePullRequestStatus = UpdatePullRequestStatus'
  { -- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
    pullRequestId :: Lude.Text,
    -- | The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from @CLOSED@ to @CLOSED@ .
    pullRequestStatus :: PullRequestStatusEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestStatus' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
-- * 'pullRequestStatus' - The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from @CLOSED@ to @CLOSED@ .
mkUpdatePullRequestStatus ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'pullRequestStatus'
  PullRequestStatusEnum ->
  UpdatePullRequestStatus
mkUpdatePullRequestStatus pPullRequestId_ pPullRequestStatus_ =
  UpdatePullRequestStatus'
    { pullRequestId = pPullRequestId_,
      pullRequestStatus = pPullRequestStatus_
    }

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPullRequestId :: Lens.Lens' UpdatePullRequestStatus Lude.Text
uprsPullRequestId = Lens.lens (pullRequestId :: UpdatePullRequestStatus -> Lude.Text) (\s a -> s {pullRequestId = a} :: UpdatePullRequestStatus)
{-# DEPRECATED uprsPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from @CLOSED@ to @CLOSED@ .
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPullRequestStatus :: Lens.Lens' UpdatePullRequestStatus PullRequestStatusEnum
uprsPullRequestStatus = Lens.lens (pullRequestStatus :: UpdatePullRequestStatus -> PullRequestStatusEnum) (\s a -> s {pullRequestStatus = a} :: UpdatePullRequestStatus)
{-# DEPRECATED uprsPullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead." #-}

instance Lude.AWSRequest UpdatePullRequestStatus where
  type Rs UpdatePullRequestStatus = UpdatePullRequestStatusResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePullRequestStatusResponse'
            Lude.<$> (x Lude..:> "pullRequest") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePullRequestStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.UpdatePullRequestStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePullRequestStatus where
  toJSON UpdatePullRequestStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("pullRequestStatus" Lude..= pullRequestStatus)
          ]
      )

instance Lude.ToPath UpdatePullRequestStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePullRequestStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePullRequestStatusResponse' smart constructor.
data UpdatePullRequestStatusResponse = UpdatePullRequestStatusResponse'
  { -- | Information about the pull request.
    pullRequest :: PullRequest,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestStatusResponse' with the minimum fields required to make a request.
--
-- * 'pullRequest' - Information about the pull request.
-- * 'responseStatus' - The response status code.
mkUpdatePullRequestStatusResponse ::
  -- | 'pullRequest'
  PullRequest ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePullRequestStatusResponse
mkUpdatePullRequestStatusResponse pPullRequest_ pResponseStatus_ =
  UpdatePullRequestStatusResponse'
    { pullRequest = pPullRequest_,
      responseStatus = pResponseStatus_
    }

-- | Information about the pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsrsPullRequest :: Lens.Lens' UpdatePullRequestStatusResponse PullRequest
uprsrsPullRequest = Lens.lens (pullRequest :: UpdatePullRequestStatusResponse -> PullRequest) (\s a -> s {pullRequest = a} :: UpdatePullRequestStatusResponse)
{-# DEPRECATED uprsrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsrsResponseStatus :: Lens.Lens' UpdatePullRequestStatusResponse Lude.Int
uprsrsResponseStatus = Lens.lens (responseStatus :: UpdatePullRequestStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePullRequestStatusResponse)
{-# DEPRECATED uprsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
