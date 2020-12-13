{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PostCommentForPullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on a pull request.
module Network.AWS.CodeCommit.PostCommentForPullRequest
  ( -- * Creating a request
    PostCommentForPullRequest (..),
    mkPostCommentForPullRequest,

    -- ** Request lenses
    pcfprLocation,
    pcfprAfterCommitId,
    pcfprPullRequestId,
    pcfprContent,
    pcfprBeforeCommitId,
    pcfprRepositoryName,
    pcfprClientRequestToken,

    -- * Destructuring the response
    PostCommentForPullRequestResponse (..),
    mkPostCommentForPullRequestResponse,

    -- ** Response lenses
    pcfprrsBeforeBlobId,
    pcfprrsLocation,
    pcfprrsAfterCommitId,
    pcfprrsPullRequestId,
    pcfprrsAfterBlobId,
    pcfprrsBeforeCommitId,
    pcfprrsRepositoryName,
    pcfprrsComment,
    pcfprrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPostCommentForPullRequest' smart constructor.
data PostCommentForPullRequest = PostCommentForPullRequest'
  { -- | The location of the change where you want to post your comment. If no location is provided, the comment is posted as a general comment on the pull request difference between the before commit ID and the after commit ID.
    location :: Lude.Maybe Location,
    -- | The full commit ID of the commit in the source branch that is the current tip of the branch for the pull request when you post the comment.
    afterCommitId :: Lude.Text,
    -- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
    pullRequestId :: Lude.Text,
    -- | The content of your comment on the change.
    content :: Lude.Text,
    -- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
    beforeCommitId :: Lude.Text,
    -- | The name of the repository where you want to post a comment on a pull request.
    repositoryName :: Lude.Text,
    -- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostCommentForPullRequest' with the minimum fields required to make a request.
--
-- * 'location' - The location of the change where you want to post your comment. If no location is provided, the comment is posted as a general comment on the pull request difference between the before commit ID and the after commit ID.
-- * 'afterCommitId' - The full commit ID of the commit in the source branch that is the current tip of the branch for the pull request when you post the comment.
-- * 'pullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
-- * 'content' - The content of your comment on the change.
-- * 'beforeCommitId' - The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
-- * 'repositoryName' - The name of the repository where you want to post a comment on a pull request.
-- * 'clientRequestToken' - A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
mkPostCommentForPullRequest ::
  -- | 'afterCommitId'
  Lude.Text ->
  -- | 'pullRequestId'
  Lude.Text ->
  -- | 'content'
  Lude.Text ->
  -- | 'beforeCommitId'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  PostCommentForPullRequest
mkPostCommentForPullRequest
  pAfterCommitId_
  pPullRequestId_
  pContent_
  pBeforeCommitId_
  pRepositoryName_ =
    PostCommentForPullRequest'
      { location = Lude.Nothing,
        afterCommitId = pAfterCommitId_,
        pullRequestId = pPullRequestId_,
        content = pContent_,
        beforeCommitId = pBeforeCommitId_,
        repositoryName = pRepositoryName_,
        clientRequestToken = Lude.Nothing
      }

-- | The location of the change where you want to post your comment. If no location is provided, the comment is posted as a general comment on the pull request difference between the before commit ID and the after commit ID.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprLocation :: Lens.Lens' PostCommentForPullRequest (Lude.Maybe Location)
pcfprLocation = Lens.lens (location :: PostCommentForPullRequest -> Lude.Maybe Location) (\s a -> s {location = a} :: PostCommentForPullRequest)
{-# DEPRECATED pcfprLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The full commit ID of the commit in the source branch that is the current tip of the branch for the pull request when you post the comment.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprAfterCommitId :: Lens.Lens' PostCommentForPullRequest Lude.Text
pcfprAfterCommitId = Lens.lens (afterCommitId :: PostCommentForPullRequest -> Lude.Text) (\s a -> s {afterCommitId = a} :: PostCommentForPullRequest)
{-# DEPRECATED pcfprAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprPullRequestId :: Lens.Lens' PostCommentForPullRequest Lude.Text
pcfprPullRequestId = Lens.lens (pullRequestId :: PostCommentForPullRequest -> Lude.Text) (\s a -> s {pullRequestId = a} :: PostCommentForPullRequest)
{-# DEPRECATED pcfprPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The content of your comment on the change.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprContent :: Lens.Lens' PostCommentForPullRequest Lude.Text
pcfprContent = Lens.lens (content :: PostCommentForPullRequest -> Lude.Text) (\s a -> s {content = a} :: PostCommentForPullRequest)
{-# DEPRECATED pcfprContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprBeforeCommitId :: Lens.Lens' PostCommentForPullRequest Lude.Text
pcfprBeforeCommitId = Lens.lens (beforeCommitId :: PostCommentForPullRequest -> Lude.Text) (\s a -> s {beforeCommitId = a} :: PostCommentForPullRequest)
{-# DEPRECATED pcfprBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The name of the repository where you want to post a comment on a pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprRepositoryName :: Lens.Lens' PostCommentForPullRequest Lude.Text
pcfprRepositoryName = Lens.lens (repositoryName :: PostCommentForPullRequest -> Lude.Text) (\s a -> s {repositoryName = a} :: PostCommentForPullRequest)
{-# DEPRECATED pcfprRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprClientRequestToken :: Lens.Lens' PostCommentForPullRequest (Lude.Maybe Lude.Text)
pcfprClientRequestToken = Lens.lens (clientRequestToken :: PostCommentForPullRequest -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: PostCommentForPullRequest)
{-# DEPRECATED pcfprClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest PostCommentForPullRequest where
  type
    Rs PostCommentForPullRequest =
      PostCommentForPullRequestResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          PostCommentForPullRequestResponse'
            Lude.<$> (x Lude..?> "beforeBlobId")
            Lude.<*> (x Lude..?> "location")
            Lude.<*> (x Lude..?> "afterCommitId")
            Lude.<*> (x Lude..?> "pullRequestId")
            Lude.<*> (x Lude..?> "afterBlobId")
            Lude.<*> (x Lude..?> "beforeCommitId")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "comment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PostCommentForPullRequest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.PostCommentForPullRequest" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PostCommentForPullRequest where
  toJSON PostCommentForPullRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("location" Lude..=) Lude.<$> location,
            Lude.Just ("afterCommitId" Lude..= afterCommitId),
            Lude.Just ("pullRequestId" Lude..= pullRequestId),
            Lude.Just ("content" Lude..= content),
            Lude.Just ("beforeCommitId" Lude..= beforeCommitId),
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath PostCommentForPullRequest where
  toPath = Lude.const "/"

instance Lude.ToQuery PostCommentForPullRequest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPostCommentForPullRequestResponse' smart constructor.
data PostCommentForPullRequestResponse = PostCommentForPullRequestResponse'
  { -- | In the directionality of the pull request, the blob ID of the before blob.
    beforeBlobId :: Lude.Maybe Lude.Text,
    -- | The location of the change where you posted your comment.
    location :: Lude.Maybe Location,
    -- | The full commit ID of the commit in the destination branch where the pull request is merged.
    afterCommitId :: Lude.Maybe Lude.Text,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Lude.Maybe Lude.Text,
    -- | In the directionality of the pull request, the blob ID of the after blob.
    afterBlobId :: Lude.Maybe Lude.Text,
    -- | The full commit ID of the commit in the source branch used to create the pull request, or in the case of an updated pull request, the full commit ID of the commit used to update the pull request.
    beforeCommitId :: Lude.Maybe Lude.Text,
    -- | The name of the repository where you posted a comment on a pull request.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The content of the comment you posted.
    comment :: Lude.Maybe Comment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostCommentForPullRequestResponse' with the minimum fields required to make a request.
--
-- * 'beforeBlobId' - In the directionality of the pull request, the blob ID of the before blob.
-- * 'location' - The location of the change where you posted your comment.
-- * 'afterCommitId' - The full commit ID of the commit in the destination branch where the pull request is merged.
-- * 'pullRequestId' - The system-generated ID of the pull request.
-- * 'afterBlobId' - In the directionality of the pull request, the blob ID of the after blob.
-- * 'beforeCommitId' - The full commit ID of the commit in the source branch used to create the pull request, or in the case of an updated pull request, the full commit ID of the commit used to update the pull request.
-- * 'repositoryName' - The name of the repository where you posted a comment on a pull request.
-- * 'comment' - The content of the comment you posted.
-- * 'responseStatus' - The response status code.
mkPostCommentForPullRequestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PostCommentForPullRequestResponse
mkPostCommentForPullRequestResponse pResponseStatus_ =
  PostCommentForPullRequestResponse'
    { beforeBlobId = Lude.Nothing,
      location = Lude.Nothing,
      afterCommitId = Lude.Nothing,
      pullRequestId = Lude.Nothing,
      afterBlobId = Lude.Nothing,
      beforeCommitId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      comment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | In the directionality of the pull request, the blob ID of the before blob.
--
-- /Note:/ Consider using 'beforeBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsBeforeBlobId :: Lens.Lens' PostCommentForPullRequestResponse (Lude.Maybe Lude.Text)
pcfprrsBeforeBlobId = Lens.lens (beforeBlobId :: PostCommentForPullRequestResponse -> Lude.Maybe Lude.Text) (\s a -> s {beforeBlobId = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsBeforeBlobId "Use generic-lens or generic-optics with 'beforeBlobId' instead." #-}

-- | The location of the change where you posted your comment.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsLocation :: Lens.Lens' PostCommentForPullRequestResponse (Lude.Maybe Location)
pcfprrsLocation = Lens.lens (location :: PostCommentForPullRequestResponse -> Lude.Maybe Location) (\s a -> s {location = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The full commit ID of the commit in the destination branch where the pull request is merged.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsAfterCommitId :: Lens.Lens' PostCommentForPullRequestResponse (Lude.Maybe Lude.Text)
pcfprrsAfterCommitId = Lens.lens (afterCommitId :: PostCommentForPullRequestResponse -> Lude.Maybe Lude.Text) (\s a -> s {afterCommitId = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | The system-generated ID of the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsPullRequestId :: Lens.Lens' PostCommentForPullRequestResponse (Lude.Maybe Lude.Text)
pcfprrsPullRequestId = Lens.lens (pullRequestId :: PostCommentForPullRequestResponse -> Lude.Maybe Lude.Text) (\s a -> s {pullRequestId = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | In the directionality of the pull request, the blob ID of the after blob.
--
-- /Note:/ Consider using 'afterBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsAfterBlobId :: Lens.Lens' PostCommentForPullRequestResponse (Lude.Maybe Lude.Text)
pcfprrsAfterBlobId = Lens.lens (afterBlobId :: PostCommentForPullRequestResponse -> Lude.Maybe Lude.Text) (\s a -> s {afterBlobId = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsAfterBlobId "Use generic-lens or generic-optics with 'afterBlobId' instead." #-}

-- | The full commit ID of the commit in the source branch used to create the pull request, or in the case of an updated pull request, the full commit ID of the commit used to update the pull request.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsBeforeCommitId :: Lens.Lens' PostCommentForPullRequestResponse (Lude.Maybe Lude.Text)
pcfprrsBeforeCommitId = Lens.lens (beforeCommitId :: PostCommentForPullRequestResponse -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitId = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The name of the repository where you posted a comment on a pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsRepositoryName :: Lens.Lens' PostCommentForPullRequestResponse (Lude.Maybe Lude.Text)
pcfprrsRepositoryName = Lens.lens (repositoryName :: PostCommentForPullRequestResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The content of the comment you posted.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsComment :: Lens.Lens' PostCommentForPullRequestResponse (Lude.Maybe Comment)
pcfprrsComment = Lens.lens (comment :: PostCommentForPullRequestResponse -> Lude.Maybe Comment) (\s a -> s {comment = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfprrsResponseStatus :: Lens.Lens' PostCommentForPullRequestResponse Lude.Int
pcfprrsResponseStatus = Lens.lens (responseStatus :: PostCommentForPullRequestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PostCommentForPullRequestResponse)
{-# DEPRECATED pcfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
