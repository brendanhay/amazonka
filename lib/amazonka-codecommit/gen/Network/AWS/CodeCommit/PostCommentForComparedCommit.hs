{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PostCommentForComparedCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on the comparison between two commits.
module Network.AWS.CodeCommit.PostCommentForComparedCommit
  ( -- * Creating a request
    PostCommentForComparedCommit (..),
    mkPostCommentForComparedCommit,

    -- ** Request lenses
    pcfccLocation,
    pcfccAfterCommitId,
    pcfccContent,
    pcfccBeforeCommitId,
    pcfccRepositoryName,
    pcfccClientRequestToken,

    -- * Destructuring the response
    PostCommentForComparedCommitResponse (..),
    mkPostCommentForComparedCommitResponse,

    -- ** Response lenses
    pcfccrsBeforeBlobId,
    pcfccrsLocation,
    pcfccrsAfterCommitId,
    pcfccrsAfterBlobId,
    pcfccrsBeforeCommitId,
    pcfccrsRepositoryName,
    pcfccrsComment,
    pcfccrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPostCommentForComparedCommit' smart constructor.
data PostCommentForComparedCommit = PostCommentForComparedCommit'
  { -- | The location of the comparison where you want to comment.
    location :: Lude.Maybe Location,
    -- | To establish the directionality of the comparison, the full commit ID of the after commit.
    afterCommitId :: Lude.Text,
    -- | The content of the comment you want to make.
    content :: Lude.Text,
    -- | To establish the directionality of the comparison, the full commit ID of the before commit. Required for commenting on any commit unless that commit is the initial commit.
    beforeCommitId :: Lude.Maybe Lude.Text,
    -- | The name of the repository where you want to post a comment on the comparison between commits.
    repositoryName :: Lude.Text,
    -- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
    clientRequestToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostCommentForComparedCommit' with the minimum fields required to make a request.
--
-- * 'location' - The location of the comparison where you want to comment.
-- * 'afterCommitId' - To establish the directionality of the comparison, the full commit ID of the after commit.
-- * 'content' - The content of the comment you want to make.
-- * 'beforeCommitId' - To establish the directionality of the comparison, the full commit ID of the before commit. Required for commenting on any commit unless that commit is the initial commit.
-- * 'repositoryName' - The name of the repository where you want to post a comment on the comparison between commits.
-- * 'clientRequestToken' - A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
mkPostCommentForComparedCommit ::
  -- | 'afterCommitId'
  Lude.Text ->
  -- | 'content'
  Lude.Text ->
  -- | 'repositoryName'
  Lude.Text ->
  PostCommentForComparedCommit
mkPostCommentForComparedCommit
  pAfterCommitId_
  pContent_
  pRepositoryName_ =
    PostCommentForComparedCommit'
      { location = Lude.Nothing,
        afterCommitId = pAfterCommitId_,
        content = pContent_,
        beforeCommitId = Lude.Nothing,
        repositoryName = pRepositoryName_,
        clientRequestToken = Lude.Nothing
      }

-- | The location of the comparison where you want to comment.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccLocation :: Lens.Lens' PostCommentForComparedCommit (Lude.Maybe Location)
pcfccLocation = Lens.lens (location :: PostCommentForComparedCommit -> Lude.Maybe Location) (\s a -> s {location = a} :: PostCommentForComparedCommit)
{-# DEPRECATED pcfccLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | To establish the directionality of the comparison, the full commit ID of the after commit.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccAfterCommitId :: Lens.Lens' PostCommentForComparedCommit Lude.Text
pcfccAfterCommitId = Lens.lens (afterCommitId :: PostCommentForComparedCommit -> Lude.Text) (\s a -> s {afterCommitId = a} :: PostCommentForComparedCommit)
{-# DEPRECATED pcfccAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | The content of the comment you want to make.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccContent :: Lens.Lens' PostCommentForComparedCommit Lude.Text
pcfccContent = Lens.lens (content :: PostCommentForComparedCommit -> Lude.Text) (\s a -> s {content = a} :: PostCommentForComparedCommit)
{-# DEPRECATED pcfccContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | To establish the directionality of the comparison, the full commit ID of the before commit. Required for commenting on any commit unless that commit is the initial commit.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccBeforeCommitId :: Lens.Lens' PostCommentForComparedCommit (Lude.Maybe Lude.Text)
pcfccBeforeCommitId = Lens.lens (beforeCommitId :: PostCommentForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitId = a} :: PostCommentForComparedCommit)
{-# DEPRECATED pcfccBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The name of the repository where you want to post a comment on the comparison between commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccRepositoryName :: Lens.Lens' PostCommentForComparedCommit Lude.Text
pcfccRepositoryName = Lens.lens (repositoryName :: PostCommentForComparedCommit -> Lude.Text) (\s a -> s {repositoryName = a} :: PostCommentForComparedCommit)
{-# DEPRECATED pcfccRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccClientRequestToken :: Lens.Lens' PostCommentForComparedCommit (Lude.Maybe Lude.Text)
pcfccClientRequestToken = Lens.lens (clientRequestToken :: PostCommentForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: PostCommentForComparedCommit)
{-# DEPRECATED pcfccClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest PostCommentForComparedCommit where
  type
    Rs PostCommentForComparedCommit =
      PostCommentForComparedCommitResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          PostCommentForComparedCommitResponse'
            Lude.<$> (x Lude..?> "beforeBlobId")
            Lude.<*> (x Lude..?> "location")
            Lude.<*> (x Lude..?> "afterCommitId")
            Lude.<*> (x Lude..?> "afterBlobId")
            Lude.<*> (x Lude..?> "beforeCommitId")
            Lude.<*> (x Lude..?> "repositoryName")
            Lude.<*> (x Lude..?> "comment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PostCommentForComparedCommit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.PostCommentForComparedCommit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PostCommentForComparedCommit where
  toJSON PostCommentForComparedCommit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("location" Lude..=) Lude.<$> location,
            Lude.Just ("afterCommitId" Lude..= afterCommitId),
            Lude.Just ("content" Lude..= content),
            ("beforeCommitId" Lude..=) Lude.<$> beforeCommitId,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken
          ]
      )

instance Lude.ToPath PostCommentForComparedCommit where
  toPath = Lude.const "/"

instance Lude.ToQuery PostCommentForComparedCommit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPostCommentForComparedCommitResponse' smart constructor.
data PostCommentForComparedCommitResponse = PostCommentForComparedCommitResponse'
  { -- | In the directionality you established, the blob ID of the before blob.
    beforeBlobId :: Lude.Maybe Lude.Text,
    -- | The location of the comment in the comparison between the two commits.
    location :: Lude.Maybe Location,
    -- | In the directionality you established, the full commit ID of the after commit.
    afterCommitId :: Lude.Maybe Lude.Text,
    -- | In the directionality you established, the blob ID of the after blob.
    afterBlobId :: Lude.Maybe Lude.Text,
    -- | In the directionality you established, the full commit ID of the before commit.
    beforeCommitId :: Lude.Maybe Lude.Text,
    -- | The name of the repository where you posted a comment on the comparison between commits.
    repositoryName :: Lude.Maybe Lude.Text,
    -- | The content of the comment you posted.
    comment :: Lude.Maybe Comment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostCommentForComparedCommitResponse' with the minimum fields required to make a request.
--
-- * 'beforeBlobId' - In the directionality you established, the blob ID of the before blob.
-- * 'location' - The location of the comment in the comparison between the two commits.
-- * 'afterCommitId' - In the directionality you established, the full commit ID of the after commit.
-- * 'afterBlobId' - In the directionality you established, the blob ID of the after blob.
-- * 'beforeCommitId' - In the directionality you established, the full commit ID of the before commit.
-- * 'repositoryName' - The name of the repository where you posted a comment on the comparison between commits.
-- * 'comment' - The content of the comment you posted.
-- * 'responseStatus' - The response status code.
mkPostCommentForComparedCommitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PostCommentForComparedCommitResponse
mkPostCommentForComparedCommitResponse pResponseStatus_ =
  PostCommentForComparedCommitResponse'
    { beforeBlobId =
        Lude.Nothing,
      location = Lude.Nothing,
      afterCommitId = Lude.Nothing,
      afterBlobId = Lude.Nothing,
      beforeCommitId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      comment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | In the directionality you established, the blob ID of the before blob.
--
-- /Note:/ Consider using 'beforeBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrsBeforeBlobId :: Lens.Lens' PostCommentForComparedCommitResponse (Lude.Maybe Lude.Text)
pcfccrsBeforeBlobId = Lens.lens (beforeBlobId :: PostCommentForComparedCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {beforeBlobId = a} :: PostCommentForComparedCommitResponse)
{-# DEPRECATED pcfccrsBeforeBlobId "Use generic-lens or generic-optics with 'beforeBlobId' instead." #-}

-- | The location of the comment in the comparison between the two commits.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrsLocation :: Lens.Lens' PostCommentForComparedCommitResponse (Lude.Maybe Location)
pcfccrsLocation = Lens.lens (location :: PostCommentForComparedCommitResponse -> Lude.Maybe Location) (\s a -> s {location = a} :: PostCommentForComparedCommitResponse)
{-# DEPRECATED pcfccrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | In the directionality you established, the full commit ID of the after commit.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrsAfterCommitId :: Lens.Lens' PostCommentForComparedCommitResponse (Lude.Maybe Lude.Text)
pcfccrsAfterCommitId = Lens.lens (afterCommitId :: PostCommentForComparedCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {afterCommitId = a} :: PostCommentForComparedCommitResponse)
{-# DEPRECATED pcfccrsAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

-- | In the directionality you established, the blob ID of the after blob.
--
-- /Note:/ Consider using 'afterBlobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrsAfterBlobId :: Lens.Lens' PostCommentForComparedCommitResponse (Lude.Maybe Lude.Text)
pcfccrsAfterBlobId = Lens.lens (afterBlobId :: PostCommentForComparedCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {afterBlobId = a} :: PostCommentForComparedCommitResponse)
{-# DEPRECATED pcfccrsAfterBlobId "Use generic-lens or generic-optics with 'afterBlobId' instead." #-}

-- | In the directionality you established, the full commit ID of the before commit.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrsBeforeCommitId :: Lens.Lens' PostCommentForComparedCommitResponse (Lude.Maybe Lude.Text)
pcfccrsBeforeCommitId = Lens.lens (beforeCommitId :: PostCommentForComparedCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitId = a} :: PostCommentForComparedCommitResponse)
{-# DEPRECATED pcfccrsBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | The name of the repository where you posted a comment on the comparison between commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrsRepositoryName :: Lens.Lens' PostCommentForComparedCommitResponse (Lude.Maybe Lude.Text)
pcfccrsRepositoryName = Lens.lens (repositoryName :: PostCommentForComparedCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: PostCommentForComparedCommitResponse)
{-# DEPRECATED pcfccrsRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The content of the comment you posted.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrsComment :: Lens.Lens' PostCommentForComparedCommitResponse (Lude.Maybe Comment)
pcfccrsComment = Lens.lens (comment :: PostCommentForComparedCommitResponse -> Lude.Maybe Comment) (\s a -> s {comment = a} :: PostCommentForComparedCommitResponse)
{-# DEPRECATED pcfccrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcfccrsResponseStatus :: Lens.Lens' PostCommentForComparedCommitResponse Lude.Int
pcfccrsResponseStatus = Lens.lens (responseStatus :: PostCommentForComparedCommitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PostCommentForComparedCommitResponse)
{-# DEPRECATED pcfccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
