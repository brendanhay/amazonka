{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.MergePullRequestByFastForward
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to merge the source commit of a pull request into the specified destination branch for that pull request at the specified commit using the fast-forward merge strategy. If the merge is successful, it closes the pull request.
module Network.AWS.CodeCommit.MergePullRequestByFastForward
  ( -- * Creating a request
    MergePullRequestByFastForward (..),
    mkMergePullRequestByFastForward,

    -- ** Request lenses
    mprbffSourceCommitId,
    mprbffPullRequestId,
    mprbffRepositoryName,

    -- * Destructuring the response
    MergePullRequestByFastForwardResponse (..),
    mkMergePullRequestByFastForwardResponse,

    -- ** Response lenses
    mprbffrsPullRequest,
    mprbffrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMergePullRequestByFastForward' smart constructor.
data MergePullRequestByFastForward = MergePullRequestByFastForward'
  { sourceCommitId ::
      Lude.Maybe Lude.Text,
    pullRequestId :: Lude.Text,
    repositoryName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergePullRequestByFastForward' with the minimum fields required to make a request.
--
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
-- * 'repositoryName' - The name of the repository where the pull request was created.
-- * 'sourceCommitId' - The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
mkMergePullRequestByFastForward ::
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  MergePullRequestByFastForward
mkMergePullRequestByFastForward pPullRequestId_ pRepositoryName_ =
  MergePullRequestByFastForward'
    { sourceCommitId = Lude.Nothing,
      pullRequestId = pPullRequestId_,
      repositoryName = pRepositoryName_
    }

-- | The full commit ID of the original or updated commit in the pull request source branch. Pass this value if you want an exception thrown if the current commit ID of the tip of the source branch does not match this commit ID.
--
-- /Note:/ Consider using 'sourceCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffSourceCommitId :: Lens.Lens' MergePullRequestByFastForward (Lude.Maybe Lude.Text)
mprbffSourceCommitId = Lens.lens (sourceCommitId :: MergePullRequestByFastForward -> Lude.Maybe Lude.Text) (\s a -> s {sourceCommitId = a} :: MergePullRequestByFastForward)
{-# DEPRECATED mprbffSourceCommitId "Use generic-lens or generic-optics with 'sourceCommitId' instead." #-}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffPullRequestId :: Lens.Lens' MergePullRequestByFastForward Lude.Text
mprbffPullRequestId = Lens.lens (pullRequestId :: MergePullRequestByFastForward -> Lude.Text) (\s a -> s {pullRequestId = a} :: MergePullRequestByFastForward)
{-# DEPRECATED mprbffPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The name of the repository where the pull request was created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffRepositoryName :: Lens.Lens' MergePullRequestByFastForward Lude.Text
mprbffRepositoryName = Lens.lens (repositoryName :: MergePullRequestByFastForward -> Lude.Text) (\s a -> s {repositoryName = a} :: MergePullRequestByFastForward)
{-# DEPRECATED mprbffRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest MergePullRequestByFastForward where
  type
    Rs MergePullRequestByFastForward =
      MergePullRequestByFastForwardResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          MergePullRequestByFastForwardResponse'
            Lude.<$> (x Lude..?> "pullRequest") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MergePullRequestByFastForward where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.MergePullRequestByFastForward" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MergePullRequestByFastForward where
  toJSON MergePullRequestByFastForward' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sourceCommitId" Lude..=) Lude.<$> sourceCommitId,
            Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath MergePullRequestByFastForward where
  toPath = Lude.const "/"

instance Lude.ToQuery MergePullRequestByFastForward where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMergePullRequestByFastForwardResponse' smart constructor.
data MergePullRequestByFastForwardResponse = MergePullRequestByFastForwardResponse'
  { pullRequest ::
      Lude.Maybe
        PullRequest,
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

-- | Creates a value of 'MergePullRequestByFastForwardResponse' with the minimum fields required to make a request.
--
-- * 'pullRequest' - Information about the specified pull request, including the merge.
-- * 'responseStatus' - The response status code.
mkMergePullRequestByFastForwardResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MergePullRequestByFastForwardResponse
mkMergePullRequestByFastForwardResponse pResponseStatus_ =
  MergePullRequestByFastForwardResponse'
    { pullRequest =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the specified pull request, including the merge.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffrsPullRequest :: Lens.Lens' MergePullRequestByFastForwardResponse (Lude.Maybe PullRequest)
mprbffrsPullRequest = Lens.lens (pullRequest :: MergePullRequestByFastForwardResponse -> Lude.Maybe PullRequest) (\s a -> s {pullRequest = a} :: MergePullRequestByFastForwardResponse)
{-# DEPRECATED mprbffrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprbffrsResponseStatus :: Lens.Lens' MergePullRequestByFastForwardResponse Lude.Int
mprbffrsResponseStatus = Lens.lens (responseStatus :: MergePullRequestByFastForwardResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MergePullRequestByFastForwardResponse)
{-# DEPRECATED mprbffrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
