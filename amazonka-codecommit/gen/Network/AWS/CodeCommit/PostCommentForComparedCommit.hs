{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PostCommentForComparedCommit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on the comparison between two commits.
module Network.AWS.CodeCommit.PostCommentForComparedCommit
  ( -- * Creating a Request
    PostCommentForComparedCommit (..),
    newPostCommentForComparedCommit,

    -- * Request Lenses
    postCommentForComparedCommit_beforeCommitId,
    postCommentForComparedCommit_clientRequestToken,
    postCommentForComparedCommit_location,
    postCommentForComparedCommit_repositoryName,
    postCommentForComparedCommit_afterCommitId,
    postCommentForComparedCommit_content,

    -- * Destructuring the Response
    PostCommentForComparedCommitResponse (..),
    newPostCommentForComparedCommitResponse,

    -- * Response Lenses
    postCommentForComparedCommitResponse_beforeBlobId,
    postCommentForComparedCommitResponse_comment,
    postCommentForComparedCommitResponse_repositoryName,
    postCommentForComparedCommitResponse_beforeCommitId,
    postCommentForComparedCommitResponse_afterBlobId,
    postCommentForComparedCommitResponse_afterCommitId,
    postCommentForComparedCommitResponse_location,
    postCommentForComparedCommitResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPostCommentForComparedCommit' smart constructor.
data PostCommentForComparedCommit = PostCommentForComparedCommit'
  { -- | To establish the directionality of the comparison, the full commit ID of
    -- the before commit. Required for commenting on any commit unless that
    -- commit is the initial commit.
    beforeCommitId :: Core.Maybe Core.Text,
    -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The location of the comparison where you want to comment.
    location :: Core.Maybe Location,
    -- | The name of the repository where you want to post a comment on the
    -- comparison between commits.
    repositoryName :: Core.Text,
    -- | To establish the directionality of the comparison, the full commit ID of
    -- the after commit.
    afterCommitId :: Core.Text,
    -- | The content of the comment you want to make.
    content :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PostCommentForComparedCommit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beforeCommitId', 'postCommentForComparedCommit_beforeCommitId' - To establish the directionality of the comparison, the full commit ID of
-- the before commit. Required for commenting on any commit unless that
-- commit is the initial commit.
--
-- 'clientRequestToken', 'postCommentForComparedCommit_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- 'location', 'postCommentForComparedCommit_location' - The location of the comparison where you want to comment.
--
-- 'repositoryName', 'postCommentForComparedCommit_repositoryName' - The name of the repository where you want to post a comment on the
-- comparison between commits.
--
-- 'afterCommitId', 'postCommentForComparedCommit_afterCommitId' - To establish the directionality of the comparison, the full commit ID of
-- the after commit.
--
-- 'content', 'postCommentForComparedCommit_content' - The content of the comment you want to make.
newPostCommentForComparedCommit ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'afterCommitId'
  Core.Text ->
  -- | 'content'
  Core.Text ->
  PostCommentForComparedCommit
newPostCommentForComparedCommit
  pRepositoryName_
  pAfterCommitId_
  pContent_ =
    PostCommentForComparedCommit'
      { beforeCommitId =
          Core.Nothing,
        clientRequestToken = Core.Nothing,
        location = Core.Nothing,
        repositoryName = pRepositoryName_,
        afterCommitId = pAfterCommitId_,
        content = pContent_
      }

-- | To establish the directionality of the comparison, the full commit ID of
-- the before commit. Required for commenting on any commit unless that
-- commit is the initial commit.
postCommentForComparedCommit_beforeCommitId :: Lens.Lens' PostCommentForComparedCommit (Core.Maybe Core.Text)
postCommentForComparedCommit_beforeCommitId = Lens.lens (\PostCommentForComparedCommit' {beforeCommitId} -> beforeCommitId) (\s@PostCommentForComparedCommit' {} a -> s {beforeCommitId = a} :: PostCommentForComparedCommit)

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
postCommentForComparedCommit_clientRequestToken :: Lens.Lens' PostCommentForComparedCommit (Core.Maybe Core.Text)
postCommentForComparedCommit_clientRequestToken = Lens.lens (\PostCommentForComparedCommit' {clientRequestToken} -> clientRequestToken) (\s@PostCommentForComparedCommit' {} a -> s {clientRequestToken = a} :: PostCommentForComparedCommit)

-- | The location of the comparison where you want to comment.
postCommentForComparedCommit_location :: Lens.Lens' PostCommentForComparedCommit (Core.Maybe Location)
postCommentForComparedCommit_location = Lens.lens (\PostCommentForComparedCommit' {location} -> location) (\s@PostCommentForComparedCommit' {} a -> s {location = a} :: PostCommentForComparedCommit)

-- | The name of the repository where you want to post a comment on the
-- comparison between commits.
postCommentForComparedCommit_repositoryName :: Lens.Lens' PostCommentForComparedCommit Core.Text
postCommentForComparedCommit_repositoryName = Lens.lens (\PostCommentForComparedCommit' {repositoryName} -> repositoryName) (\s@PostCommentForComparedCommit' {} a -> s {repositoryName = a} :: PostCommentForComparedCommit)

-- | To establish the directionality of the comparison, the full commit ID of
-- the after commit.
postCommentForComparedCommit_afterCommitId :: Lens.Lens' PostCommentForComparedCommit Core.Text
postCommentForComparedCommit_afterCommitId = Lens.lens (\PostCommentForComparedCommit' {afterCommitId} -> afterCommitId) (\s@PostCommentForComparedCommit' {} a -> s {afterCommitId = a} :: PostCommentForComparedCommit)

-- | The content of the comment you want to make.
postCommentForComparedCommit_content :: Lens.Lens' PostCommentForComparedCommit Core.Text
postCommentForComparedCommit_content = Lens.lens (\PostCommentForComparedCommit' {content} -> content) (\s@PostCommentForComparedCommit' {} a -> s {content = a} :: PostCommentForComparedCommit)

instance Core.AWSRequest PostCommentForComparedCommit where
  type
    AWSResponse PostCommentForComparedCommit =
      PostCommentForComparedCommitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PostCommentForComparedCommitResponse'
            Core.<$> (x Core..?> "beforeBlobId")
            Core.<*> (x Core..?> "comment")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "beforeCommitId")
            Core.<*> (x Core..?> "afterBlobId")
            Core.<*> (x Core..?> "afterCommitId")
            Core.<*> (x Core..?> "location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PostCommentForComparedCommit

instance Core.NFData PostCommentForComparedCommit

instance Core.ToHeaders PostCommentForComparedCommit where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.PostCommentForComparedCommit" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PostCommentForComparedCommit where
  toJSON PostCommentForComparedCommit' {..} =
    Core.object
      ( Core.catMaybes
          [ ("beforeCommitId" Core..=) Core.<$> beforeCommitId,
            ("clientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("location" Core..=) Core.<$> location,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("afterCommitId" Core..= afterCommitId),
            Core.Just ("content" Core..= content)
          ]
      )

instance Core.ToPath PostCommentForComparedCommit where
  toPath = Core.const "/"

instance Core.ToQuery PostCommentForComparedCommit where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPostCommentForComparedCommitResponse' smart constructor.
data PostCommentForComparedCommitResponse = PostCommentForComparedCommitResponse'
  { -- | In the directionality you established, the blob ID of the before blob.
    beforeBlobId :: Core.Maybe Core.Text,
    -- | The content of the comment you posted.
    comment :: Core.Maybe Comment,
    -- | The name of the repository where you posted a comment on the comparison
    -- between commits.
    repositoryName :: Core.Maybe Core.Text,
    -- | In the directionality you established, the full commit ID of the before
    -- commit.
    beforeCommitId :: Core.Maybe Core.Text,
    -- | In the directionality you established, the blob ID of the after blob.
    afterBlobId :: Core.Maybe Core.Text,
    -- | In the directionality you established, the full commit ID of the after
    -- commit.
    afterCommitId :: Core.Maybe Core.Text,
    -- | The location of the comment in the comparison between the two commits.
    location :: Core.Maybe Location,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PostCommentForComparedCommitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beforeBlobId', 'postCommentForComparedCommitResponse_beforeBlobId' - In the directionality you established, the blob ID of the before blob.
--
-- 'comment', 'postCommentForComparedCommitResponse_comment' - The content of the comment you posted.
--
-- 'repositoryName', 'postCommentForComparedCommitResponse_repositoryName' - The name of the repository where you posted a comment on the comparison
-- between commits.
--
-- 'beforeCommitId', 'postCommentForComparedCommitResponse_beforeCommitId' - In the directionality you established, the full commit ID of the before
-- commit.
--
-- 'afterBlobId', 'postCommentForComparedCommitResponse_afterBlobId' - In the directionality you established, the blob ID of the after blob.
--
-- 'afterCommitId', 'postCommentForComparedCommitResponse_afterCommitId' - In the directionality you established, the full commit ID of the after
-- commit.
--
-- 'location', 'postCommentForComparedCommitResponse_location' - The location of the comment in the comparison between the two commits.
--
-- 'httpStatus', 'postCommentForComparedCommitResponse_httpStatus' - The response's http status code.
newPostCommentForComparedCommitResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PostCommentForComparedCommitResponse
newPostCommentForComparedCommitResponse pHttpStatus_ =
  PostCommentForComparedCommitResponse'
    { beforeBlobId =
        Core.Nothing,
      comment = Core.Nothing,
      repositoryName = Core.Nothing,
      beforeCommitId = Core.Nothing,
      afterBlobId = Core.Nothing,
      afterCommitId = Core.Nothing,
      location = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | In the directionality you established, the blob ID of the before blob.
postCommentForComparedCommitResponse_beforeBlobId :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Core.Text)
postCommentForComparedCommitResponse_beforeBlobId = Lens.lens (\PostCommentForComparedCommitResponse' {beforeBlobId} -> beforeBlobId) (\s@PostCommentForComparedCommitResponse' {} a -> s {beforeBlobId = a} :: PostCommentForComparedCommitResponse)

-- | The content of the comment you posted.
postCommentForComparedCommitResponse_comment :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Comment)
postCommentForComparedCommitResponse_comment = Lens.lens (\PostCommentForComparedCommitResponse' {comment} -> comment) (\s@PostCommentForComparedCommitResponse' {} a -> s {comment = a} :: PostCommentForComparedCommitResponse)

-- | The name of the repository where you posted a comment on the comparison
-- between commits.
postCommentForComparedCommitResponse_repositoryName :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Core.Text)
postCommentForComparedCommitResponse_repositoryName = Lens.lens (\PostCommentForComparedCommitResponse' {repositoryName} -> repositoryName) (\s@PostCommentForComparedCommitResponse' {} a -> s {repositoryName = a} :: PostCommentForComparedCommitResponse)

-- | In the directionality you established, the full commit ID of the before
-- commit.
postCommentForComparedCommitResponse_beforeCommitId :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Core.Text)
postCommentForComparedCommitResponse_beforeCommitId = Lens.lens (\PostCommentForComparedCommitResponse' {beforeCommitId} -> beforeCommitId) (\s@PostCommentForComparedCommitResponse' {} a -> s {beforeCommitId = a} :: PostCommentForComparedCommitResponse)

-- | In the directionality you established, the blob ID of the after blob.
postCommentForComparedCommitResponse_afterBlobId :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Core.Text)
postCommentForComparedCommitResponse_afterBlobId = Lens.lens (\PostCommentForComparedCommitResponse' {afterBlobId} -> afterBlobId) (\s@PostCommentForComparedCommitResponse' {} a -> s {afterBlobId = a} :: PostCommentForComparedCommitResponse)

-- | In the directionality you established, the full commit ID of the after
-- commit.
postCommentForComparedCommitResponse_afterCommitId :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Core.Text)
postCommentForComparedCommitResponse_afterCommitId = Lens.lens (\PostCommentForComparedCommitResponse' {afterCommitId} -> afterCommitId) (\s@PostCommentForComparedCommitResponse' {} a -> s {afterCommitId = a} :: PostCommentForComparedCommitResponse)

-- | The location of the comment in the comparison between the two commits.
postCommentForComparedCommitResponse_location :: Lens.Lens' PostCommentForComparedCommitResponse (Core.Maybe Location)
postCommentForComparedCommitResponse_location = Lens.lens (\PostCommentForComparedCommitResponse' {location} -> location) (\s@PostCommentForComparedCommitResponse' {} a -> s {location = a} :: PostCommentForComparedCommitResponse)

-- | The response's http status code.
postCommentForComparedCommitResponse_httpStatus :: Lens.Lens' PostCommentForComparedCommitResponse Core.Int
postCommentForComparedCommitResponse_httpStatus = Lens.lens (\PostCommentForComparedCommitResponse' {httpStatus} -> httpStatus) (\s@PostCommentForComparedCommitResponse' {} a -> s {httpStatus = a} :: PostCommentForComparedCommitResponse)

instance
  Core.NFData
    PostCommentForComparedCommitResponse
