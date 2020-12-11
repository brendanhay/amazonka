{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListPullRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of pull requests for a specified repository. The return list can be refined by pull request status or pull request author ARN.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListPullRequests
  ( -- * Creating a request
    ListPullRequests (..),
    mkListPullRequests,

    -- ** Request lenses
    lprAuthorARN,
    lprNextToken,
    lprPullRequestStatus,
    lprMaxResults,
    lprRepositoryName,

    -- * Destructuring the response
    ListPullRequestsResponse (..),
    mkListPullRequestsResponse,

    -- ** Response lenses
    lprrsNextToken,
    lprrsResponseStatus,
    lprrsPullRequestIds,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPullRequests' smart constructor.
data ListPullRequests = ListPullRequests'
  { authorARN ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    pullRequestStatus :: Lude.Maybe PullRequestStatusEnum,
    maxResults :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'ListPullRequests' with the minimum fields required to make a request.
--
-- * 'authorARN' - Optional. The Amazon Resource Name (ARN) of the user who created the pull request. If used, this filters the results to pull requests created by that user.
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results.
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'pullRequestStatus' - Optional. The status of the pull request. If used, this refines the results to the pull requests that match the specified status.
-- * 'repositoryName' - The name of the repository for which you want to list pull requests.
mkListPullRequests ::
  -- | 'repositoryName'
  Lude.Text ->
  ListPullRequests
mkListPullRequests pRepositoryName_ =
  ListPullRequests'
    { authorARN = Lude.Nothing,
      nextToken = Lude.Nothing,
      pullRequestStatus = Lude.Nothing,
      maxResults = Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | Optional. The Amazon Resource Name (ARN) of the user who created the pull request. If used, this filters the results to pull requests created by that user.
--
-- /Note:/ Consider using 'authorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprAuthorARN :: Lens.Lens' ListPullRequests (Lude.Maybe Lude.Text)
lprAuthorARN = Lens.lens (authorARN :: ListPullRequests -> Lude.Maybe Lude.Text) (\s a -> s {authorARN = a} :: ListPullRequests)
{-# DEPRECATED lprAuthorARN "Use generic-lens or generic-optics with 'authorARN' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprNextToken :: Lens.Lens' ListPullRequests (Lude.Maybe Lude.Text)
lprNextToken = Lens.lens (nextToken :: ListPullRequests -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPullRequests)
{-# DEPRECATED lprNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. The status of the pull request. If used, this refines the results to the pull requests that match the specified status.
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprPullRequestStatus :: Lens.Lens' ListPullRequests (Lude.Maybe PullRequestStatusEnum)
lprPullRequestStatus = Lens.lens (pullRequestStatus :: ListPullRequests -> Lude.Maybe PullRequestStatusEnum) (\s a -> s {pullRequestStatus = a} :: ListPullRequests)
{-# DEPRECATED lprPullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprMaxResults :: Lens.Lens' ListPullRequests (Lude.Maybe Lude.Int)
lprMaxResults = Lens.lens (maxResults :: ListPullRequests -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListPullRequests)
{-# DEPRECATED lprMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the repository for which you want to list pull requests.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprRepositoryName :: Lens.Lens' ListPullRequests Lude.Text
lprRepositoryName = Lens.lens (repositoryName :: ListPullRequests -> Lude.Text) (\s a -> s {repositoryName = a} :: ListPullRequests)
{-# DEPRECATED lprRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Page.AWSPager ListPullRequests where
  page rq rs
    | Page.stop (rs Lens.^. lprrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprrsPullRequestIds) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lprNextToken Lens..~ rs Lens.^. lprrsNextToken

instance Lude.AWSRequest ListPullRequests where
  type Rs ListPullRequests = ListPullRequestsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPullRequestsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "pullRequestIds" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListPullRequests where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.ListPullRequests" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPullRequests where
  toJSON ListPullRequests' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("authorArn" Lude..=) Lude.<$> authorARN,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("pullRequestStatus" Lude..=) Lude.<$> pullRequestStatus,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath ListPullRequests where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPullRequests where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPullRequestsResponse' smart constructor.
data ListPullRequestsResponse = ListPullRequestsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    pullRequestIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPullRequestsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that allows the operation to batch the next results of the operation.
-- * 'pullRequestIds' - The system-generated IDs of the pull requests.
-- * 'responseStatus' - The response status code.
mkListPullRequestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPullRequestsResponse
mkListPullRequestsResponse pResponseStatus_ =
  ListPullRequestsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      pullRequestIds = Lude.mempty
    }

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListPullRequestsResponse (Lude.Maybe Lude.Text)
lprrsNextToken = Lens.lens (nextToken :: ListPullRequestsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPullRequestsResponse)
{-# DEPRECATED lprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPullRequestsResponse Lude.Int
lprrsResponseStatus = Lens.lens (responseStatus :: ListPullRequestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPullRequestsResponse)
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The system-generated IDs of the pull requests.
--
-- /Note:/ Consider using 'pullRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPullRequestIds :: Lens.Lens' ListPullRequestsResponse [Lude.Text]
lprrsPullRequestIds = Lens.lens (pullRequestIds :: ListPullRequestsResponse -> [Lude.Text]) (\s a -> s {pullRequestIds = a} :: ListPullRequestsResponse)
{-# DEPRECATED lprrsPullRequestIds "Use generic-lens or generic-optics with 'pullRequestIds' instead." #-}
