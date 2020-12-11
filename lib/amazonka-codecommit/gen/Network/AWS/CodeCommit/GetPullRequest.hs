{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetPullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a pull request in a specified repository.
module Network.AWS.CodeCommit.GetPullRequest
  ( -- * Creating a request
    GetPullRequest (..),
    mkGetPullRequest,

    -- ** Request lenses
    gprPullRequestId,

    -- * Destructuring the response
    GetPullRequestResponse (..),
    mkGetPullRequestResponse,

    -- ** Response lenses
    gprrsResponseStatus,
    gprrsPullRequest,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPullRequest' smart constructor.
newtype GetPullRequest = GetPullRequest'
  { pullRequestId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPullRequest' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
mkGetPullRequest ::
  -- | 'pullRequestId'
  Lude.Text ->
  GetPullRequest
mkGetPullRequest pPullRequestId_ =
  GetPullRequest' {pullRequestId = pPullRequestId_}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprPullRequestId :: Lens.Lens' GetPullRequest Lude.Text
gprPullRequestId = Lens.lens (pullRequestId :: GetPullRequest -> Lude.Text) (\s a -> s {pullRequestId = a} :: GetPullRequest)
{-# DEPRECATED gprPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

instance Lude.AWSRequest GetPullRequest where
  type Rs GetPullRequest = GetPullRequestResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPullRequestResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "pullRequest")
      )

instance Lude.ToHeaders GetPullRequest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetPullRequest" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPullRequest where
  toJSON GetPullRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("pullRequestId" Lude..= pullRequestId)]
      )

instance Lude.ToPath GetPullRequest where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPullRequest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPullRequestResponse' smart constructor.
data GetPullRequestResponse = GetPullRequestResponse'
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

-- | Creates a value of 'GetPullRequestResponse' with the minimum fields required to make a request.
--
-- * 'pullRequest' - Information about the specified pull request.
-- * 'responseStatus' - The response status code.
mkGetPullRequestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'pullRequest'
  PullRequest ->
  GetPullRequestResponse
mkGetPullRequestResponse pResponseStatus_ pPullRequest_ =
  GetPullRequestResponse'
    { responseStatus = pResponseStatus_,
      pullRequest = pPullRequest_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetPullRequestResponse Lude.Int
gprrsResponseStatus = Lens.lens (responseStatus :: GetPullRequestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPullRequestResponse)
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about the specified pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsPullRequest :: Lens.Lens' GetPullRequestResponse PullRequest
gprrsPullRequest = Lens.lens (pullRequest :: GetPullRequestResponse -> PullRequest) (\s a -> s {pullRequest = a} :: GetPullRequestResponse)
{-# DEPRECATED gprrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}
