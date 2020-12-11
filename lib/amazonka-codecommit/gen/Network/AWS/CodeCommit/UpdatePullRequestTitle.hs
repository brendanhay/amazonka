{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestTitle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the title of a pull request.
module Network.AWS.CodeCommit.UpdatePullRequestTitle
  ( -- * Creating a request
    UpdatePullRequestTitle (..),
    mkUpdatePullRequestTitle,

    -- ** Request lenses
    uprtPullRequestId,
    uprtTitle,

    -- * Destructuring the response
    UpdatePullRequestTitleResponse (..),
    mkUpdatePullRequestTitleResponse,

    -- ** Response lenses
    uprtrsResponseStatus,
    uprtrsPullRequest,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePullRequestTitle' smart constructor.
data UpdatePullRequestTitle = UpdatePullRequestTitle'
  { pullRequestId ::
      Lude.Text,
    title :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestTitle' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
-- * 'title' - The updated title of the pull request. This replaces the existing title.
mkUpdatePullRequestTitle ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'title'
  Lude.Text ->
  UpdatePullRequestTitle
mkUpdatePullRequestTitle pPullRequestId_ pTitle_ =
  UpdatePullRequestTitle'
    { pullRequestId = pPullRequestId_,
      title = pTitle_
    }

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprtPullRequestId :: Lens.Lens' UpdatePullRequestTitle Lude.Text
uprtPullRequestId = Lens.lens (pullRequestId :: UpdatePullRequestTitle -> Lude.Text) (\s a -> s {pullRequestId = a} :: UpdatePullRequestTitle)
{-# DEPRECATED uprtPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The updated title of the pull request. This replaces the existing title.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprtTitle :: Lens.Lens' UpdatePullRequestTitle Lude.Text
uprtTitle = Lens.lens (title :: UpdatePullRequestTitle -> Lude.Text) (\s a -> s {title = a} :: UpdatePullRequestTitle)
{-# DEPRECATED uprtTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.AWSRequest UpdatePullRequestTitle where
  type Rs UpdatePullRequestTitle = UpdatePullRequestTitleResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePullRequestTitleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "pullRequest")
      )

instance Lude.ToHeaders UpdatePullRequestTitle where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.UpdatePullRequestTitle" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePullRequestTitle where
  toJSON UpdatePullRequestTitle' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("title" Lude..= title)
          ]
      )

instance Lude.ToPath UpdatePullRequestTitle where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePullRequestTitle where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePullRequestTitleResponse' smart constructor.
data UpdatePullRequestTitleResponse = UpdatePullRequestTitleResponse'
  { responseStatus ::
      Lude.Int,
    pullRequest :: PullRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePullRequestTitleResponse' with the minimum fields required to make a request.
--
-- * 'pullRequest' - Information about the updated pull request.
-- * 'responseStatus' - The response status code.
mkUpdatePullRequestTitleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'pullRequest'
  PullRequest ->
  UpdatePullRequestTitleResponse
mkUpdatePullRequestTitleResponse pResponseStatus_ pPullRequest_ =
  UpdatePullRequestTitleResponse'
    { responseStatus =
        pResponseStatus_,
      pullRequest = pPullRequest_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprtrsResponseStatus :: Lens.Lens' UpdatePullRequestTitleResponse Lude.Int
uprtrsResponseStatus = Lens.lens (responseStatus :: UpdatePullRequestTitleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePullRequestTitleResponse)
{-# DEPRECATED uprtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about the updated pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprtrsPullRequest :: Lens.Lens' UpdatePullRequestTitleResponse PullRequest
uprtrsPullRequest = Lens.lens (pullRequest :: UpdatePullRequestTitleResponse -> PullRequest) (\s a -> s {pullRequest = a} :: UpdatePullRequestTitleResponse)
{-# DEPRECATED uprtrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}
