{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetCommentsForPullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns comments made on a pull request.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetCommentsForPullRequest
  ( -- * Creating a request
    GetCommentsForPullRequest (..),
    mkGetCommentsForPullRequest,

    -- ** Request lenses
    gcfprAfterCommitId,
    gcfprNextToken,
    gcfprBeforeCommitId,
    gcfprRepositoryName,
    gcfprMaxResults,
    gcfprPullRequestId,

    -- * Destructuring the response
    GetCommentsForPullRequestResponse (..),
    mkGetCommentsForPullRequestResponse,

    -- ** Response lenses
    gcfprrsCommentsForPullRequestData,
    gcfprrsNextToken,
    gcfprrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCommentsForPullRequest' smart constructor.
data GetCommentsForPullRequest = GetCommentsForPullRequest'
  { afterCommitId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    beforeCommitId :: Lude.Maybe Lude.Text,
    repositoryName :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    pullRequestId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommentsForPullRequest' with the minimum fields required to make a request.
--
-- * 'afterCommitId' - The full commit ID of the commit in the source branch that was the tip of the branch at the time the comment was made.
-- * 'beforeCommitId' - The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results. The default is 100 comments. You can return up to 500 comments with a single request.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
-- * 'repositoryName' - The name of the repository that contains the pull request.
mkGetCommentsForPullRequest ::
  -- | 'pullRequestId'
  Lude.Text ->
  GetCommentsForPullRequest
mkGetCommentsForPullRequest pPullRequestId_ =
  GetCommentsForPullRequest'
    { afterCommitId = Lude.Nothing,
      nextToken = Lude.Nothing,
      beforeCommitId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      maxResults = Lude.Nothing,
      pullRequestId = pPullRequestId_
    }

-- | The full commit ID of the commit in the source branch that was the tip of the branch at the time the comment was made.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprAfterCommitId :: Lens.Lens' GetCommentsForPullRequest (Lude.Maybe Lude.Text)
gcfprAfterCommitId = Lens.lens (afterCommitId :: GetCommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {afterCommitId = a} :: GetCommentsForPullRequest)
{-# DEPRECATED gcfprAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprNextToken :: Lens.Lens' GetCommentsForPullRequest (Lude.Maybe Lude.Text)
gcfprNextToken = Lens.lens (nextToken :: GetCommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCommentsForPullRequest)
{-# DEPRECATED gcfprNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprBeforeCommitId :: Lens.Lens' GetCommentsForPullRequest (Lude.Maybe Lude.Text)
gcfprBeforeCommitId = Lens.lens (beforeCommitId :: GetCommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitId = a} :: GetCommentsForPullRequest)
{-# DEPRECATED gcfprBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The name of the repository that contains the pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprRepositoryName :: Lens.Lens' GetCommentsForPullRequest (Lude.Maybe Lude.Text)
gcfprRepositoryName = Lens.lens (repositoryName :: GetCommentsForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: GetCommentsForPullRequest)
{-# DEPRECATED gcfprRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is 100 comments. You can return up to 500 comments with a single request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprMaxResults :: Lens.Lens' GetCommentsForPullRequest (Lude.Maybe Lude.Int)
gcfprMaxResults = Lens.lens (maxResults :: GetCommentsForPullRequest -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetCommentsForPullRequest)
{-# DEPRECATED gcfprMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprPullRequestId :: Lens.Lens' GetCommentsForPullRequest Lude.Text
gcfprPullRequestId = Lens.lens (pullRequestId :: GetCommentsForPullRequest -> Lude.Text) (\s a -> s {pullRequestId = a} :: GetCommentsForPullRequest)
{-# DEPRECATED gcfprPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

instance Page.AWSPager GetCommentsForPullRequest where
  page rq rs
    | Page.stop (rs Lens.^. gcfprrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcfprrsCommentsForPullRequestData) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcfprNextToken Lens..~ rs Lens.^. gcfprrsNextToken

instance Lude.AWSRequest GetCommentsForPullRequest where
  type
    Rs GetCommentsForPullRequest =
      GetCommentsForPullRequestResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCommentsForPullRequestResponse'
            Lude.<$> (x Lude..?> "commentsForPullRequestData" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCommentsForPullRequest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.GetCommentsForPullRequest" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCommentsForPullRequest where
  toJSON GetCommentsForPullRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("afterCommitId" Lude..=) Lude.<$> afterCommitId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("beforeCommitId" Lude..=) Lude.<$> beforeCommitId,
            ("repositoryName" Lude..=) Lude.<$> repositoryName,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("pullRequestId" Lude..= pullRequestId)
          ]
      )

instance Lude.ToPath GetCommentsForPullRequest where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCommentsForPullRequest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCommentsForPullRequestResponse' smart constructor.
data GetCommentsForPullRequestResponse = GetCommentsForPullRequestResponse'
  { commentsForPullRequestData ::
      Lude.Maybe
        [CommentsForPullRequest],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetCommentsForPullRequestResponse' with the minimum fields required to make a request.
--
-- * 'commentsForPullRequestData' - An array of comment objects on the pull request.
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'responseStatus' - The response status code.
mkGetCommentsForPullRequestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCommentsForPullRequestResponse
mkGetCommentsForPullRequestResponse pResponseStatus_ =
  GetCommentsForPullRequestResponse'
    { commentsForPullRequestData =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of comment objects on the pull request.
--
-- /Note:/ Consider using 'commentsForPullRequestData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprrsCommentsForPullRequestData :: Lens.Lens' GetCommentsForPullRequestResponse (Lude.Maybe [CommentsForPullRequest])
gcfprrsCommentsForPullRequestData = Lens.lens (commentsForPullRequestData :: GetCommentsForPullRequestResponse -> Lude.Maybe [CommentsForPullRequest]) (\s a -> s {commentsForPullRequestData = a} :: GetCommentsForPullRequestResponse)
{-# DEPRECATED gcfprrsCommentsForPullRequestData "Use generic-lens or generic-optics with 'commentsForPullRequestData' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprrsNextToken :: Lens.Lens' GetCommentsForPullRequestResponse (Lude.Maybe Lude.Text)
gcfprrsNextToken = Lens.lens (nextToken :: GetCommentsForPullRequestResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCommentsForPullRequestResponse)
{-# DEPRECATED gcfprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprrsResponseStatus :: Lens.Lens' GetCommentsForPullRequestResponse Lude.Int
gcfprrsResponseStatus = Lens.lens (responseStatus :: GetCommentsForPullRequestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCommentsForPullRequestResponse)
{-# DEPRECATED gcfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
