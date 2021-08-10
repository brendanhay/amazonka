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
-- Module      : Network.AWS.CodeCommit.PostCommentForPullRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on a pull request.
module Network.AWS.CodeCommit.PostCommentForPullRequest
  ( -- * Creating a Request
    PostCommentForPullRequest (..),
    newPostCommentForPullRequest,

    -- * Request Lenses
    postCommentForPullRequest_clientRequestToken,
    postCommentForPullRequest_location,
    postCommentForPullRequest_pullRequestId,
    postCommentForPullRequest_repositoryName,
    postCommentForPullRequest_beforeCommitId,
    postCommentForPullRequest_afterCommitId,
    postCommentForPullRequest_content,

    -- * Destructuring the Response
    PostCommentForPullRequestResponse (..),
    newPostCommentForPullRequestResponse,

    -- * Response Lenses
    postCommentForPullRequestResponse_beforeBlobId,
    postCommentForPullRequestResponse_comment,
    postCommentForPullRequestResponse_repositoryName,
    postCommentForPullRequestResponse_beforeCommitId,
    postCommentForPullRequestResponse_afterBlobId,
    postCommentForPullRequestResponse_pullRequestId,
    postCommentForPullRequestResponse_afterCommitId,
    postCommentForPullRequestResponse_location,
    postCommentForPullRequestResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPostCommentForPullRequest' smart constructor.
data PostCommentForPullRequest = PostCommentForPullRequest'
  { -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The location of the change where you want to post your comment. If no
    -- location is provided, the comment is posted as a general comment on the
    -- pull request difference between the before commit ID and the after
    -- commit ID.
    location :: Prelude.Maybe Location,
    -- | The system-generated ID of the pull request. To get this ID, use
    -- ListPullRequests.
    pullRequestId :: Prelude.Text,
    -- | The name of the repository where you want to post a comment on a pull
    -- request.
    repositoryName :: Prelude.Text,
    -- | The full commit ID of the commit in the destination branch that was the
    -- tip of the branch at the time the pull request was created.
    beforeCommitId :: Prelude.Text,
    -- | The full commit ID of the commit in the source branch that is the
    -- current tip of the branch for the pull request when you post the
    -- comment.
    afterCommitId :: Prelude.Text,
    -- | The content of your comment on the change.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostCommentForPullRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'postCommentForPullRequest_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- 'location', 'postCommentForPullRequest_location' - The location of the change where you want to post your comment. If no
-- location is provided, the comment is posted as a general comment on the
-- pull request difference between the before commit ID and the after
-- commit ID.
--
-- 'pullRequestId', 'postCommentForPullRequest_pullRequestId' - The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
--
-- 'repositoryName', 'postCommentForPullRequest_repositoryName' - The name of the repository where you want to post a comment on a pull
-- request.
--
-- 'beforeCommitId', 'postCommentForPullRequest_beforeCommitId' - The full commit ID of the commit in the destination branch that was the
-- tip of the branch at the time the pull request was created.
--
-- 'afterCommitId', 'postCommentForPullRequest_afterCommitId' - The full commit ID of the commit in the source branch that is the
-- current tip of the branch for the pull request when you post the
-- comment.
--
-- 'content', 'postCommentForPullRequest_content' - The content of your comment on the change.
newPostCommentForPullRequest ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'beforeCommitId'
  Prelude.Text ->
  -- | 'afterCommitId'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  PostCommentForPullRequest
newPostCommentForPullRequest
  pPullRequestId_
  pRepositoryName_
  pBeforeCommitId_
  pAfterCommitId_
  pContent_ =
    PostCommentForPullRequest'
      { clientRequestToken =
          Prelude.Nothing,
        location = Prelude.Nothing,
        pullRequestId = pPullRequestId_,
        repositoryName = pRepositoryName_,
        beforeCommitId = pBeforeCommitId_,
        afterCommitId = pAfterCommitId_,
        content = pContent_
      }

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
postCommentForPullRequest_clientRequestToken :: Lens.Lens' PostCommentForPullRequest (Prelude.Maybe Prelude.Text)
postCommentForPullRequest_clientRequestToken = Lens.lens (\PostCommentForPullRequest' {clientRequestToken} -> clientRequestToken) (\s@PostCommentForPullRequest' {} a -> s {clientRequestToken = a} :: PostCommentForPullRequest)

-- | The location of the change where you want to post your comment. If no
-- location is provided, the comment is posted as a general comment on the
-- pull request difference between the before commit ID and the after
-- commit ID.
postCommentForPullRequest_location :: Lens.Lens' PostCommentForPullRequest (Prelude.Maybe Location)
postCommentForPullRequest_location = Lens.lens (\PostCommentForPullRequest' {location} -> location) (\s@PostCommentForPullRequest' {} a -> s {location = a} :: PostCommentForPullRequest)

-- | The system-generated ID of the pull request. To get this ID, use
-- ListPullRequests.
postCommentForPullRequest_pullRequestId :: Lens.Lens' PostCommentForPullRequest Prelude.Text
postCommentForPullRequest_pullRequestId = Lens.lens (\PostCommentForPullRequest' {pullRequestId} -> pullRequestId) (\s@PostCommentForPullRequest' {} a -> s {pullRequestId = a} :: PostCommentForPullRequest)

-- | The name of the repository where you want to post a comment on a pull
-- request.
postCommentForPullRequest_repositoryName :: Lens.Lens' PostCommentForPullRequest Prelude.Text
postCommentForPullRequest_repositoryName = Lens.lens (\PostCommentForPullRequest' {repositoryName} -> repositoryName) (\s@PostCommentForPullRequest' {} a -> s {repositoryName = a} :: PostCommentForPullRequest)

-- | The full commit ID of the commit in the destination branch that was the
-- tip of the branch at the time the pull request was created.
postCommentForPullRequest_beforeCommitId :: Lens.Lens' PostCommentForPullRequest Prelude.Text
postCommentForPullRequest_beforeCommitId = Lens.lens (\PostCommentForPullRequest' {beforeCommitId} -> beforeCommitId) (\s@PostCommentForPullRequest' {} a -> s {beforeCommitId = a} :: PostCommentForPullRequest)

-- | The full commit ID of the commit in the source branch that is the
-- current tip of the branch for the pull request when you post the
-- comment.
postCommentForPullRequest_afterCommitId :: Lens.Lens' PostCommentForPullRequest Prelude.Text
postCommentForPullRequest_afterCommitId = Lens.lens (\PostCommentForPullRequest' {afterCommitId} -> afterCommitId) (\s@PostCommentForPullRequest' {} a -> s {afterCommitId = a} :: PostCommentForPullRequest)

-- | The content of your comment on the change.
postCommentForPullRequest_content :: Lens.Lens' PostCommentForPullRequest Prelude.Text
postCommentForPullRequest_content = Lens.lens (\PostCommentForPullRequest' {content} -> content) (\s@PostCommentForPullRequest' {} a -> s {content = a} :: PostCommentForPullRequest)

instance Core.AWSRequest PostCommentForPullRequest where
  type
    AWSResponse PostCommentForPullRequest =
      PostCommentForPullRequestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PostCommentForPullRequestResponse'
            Prelude.<$> (x Core..?> "beforeBlobId")
            Prelude.<*> (x Core..?> "comment")
            Prelude.<*> (x Core..?> "repositoryName")
            Prelude.<*> (x Core..?> "beforeCommitId")
            Prelude.<*> (x Core..?> "afterBlobId")
            Prelude.<*> (x Core..?> "pullRequestId")
            Prelude.<*> (x Core..?> "afterCommitId")
            Prelude.<*> (x Core..?> "location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PostCommentForPullRequest

instance Prelude.NFData PostCommentForPullRequest

instance Core.ToHeaders PostCommentForPullRequest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.PostCommentForPullRequest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PostCommentForPullRequest where
  toJSON PostCommentForPullRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("location" Core..=) Prelude.<$> location,
            Prelude.Just ("pullRequestId" Core..= pullRequestId),
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just
              ("beforeCommitId" Core..= beforeCommitId),
            Prelude.Just ("afterCommitId" Core..= afterCommitId),
            Prelude.Just ("content" Core..= content)
          ]
      )

instance Core.ToPath PostCommentForPullRequest where
  toPath = Prelude.const "/"

instance Core.ToQuery PostCommentForPullRequest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPostCommentForPullRequestResponse' smart constructor.
data PostCommentForPullRequestResponse = PostCommentForPullRequestResponse'
  { -- | In the directionality of the pull request, the blob ID of the before
    -- blob.
    beforeBlobId :: Prelude.Maybe Prelude.Text,
    -- | The content of the comment you posted.
    comment :: Prelude.Maybe Comment,
    -- | The name of the repository where you posted a comment on a pull request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit in the source branch used to create the
    -- pull request, or in the case of an updated pull request, the full commit
    -- ID of the commit used to update the pull request.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | In the directionality of the pull request, the blob ID of the after
    -- blob.
    afterBlobId :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit in the destination branch where the
    -- pull request is merged.
    afterCommitId :: Prelude.Maybe Prelude.Text,
    -- | The location of the change where you posted your comment.
    location :: Prelude.Maybe Location,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostCommentForPullRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beforeBlobId', 'postCommentForPullRequestResponse_beforeBlobId' - In the directionality of the pull request, the blob ID of the before
-- blob.
--
-- 'comment', 'postCommentForPullRequestResponse_comment' - The content of the comment you posted.
--
-- 'repositoryName', 'postCommentForPullRequestResponse_repositoryName' - The name of the repository where you posted a comment on a pull request.
--
-- 'beforeCommitId', 'postCommentForPullRequestResponse_beforeCommitId' - The full commit ID of the commit in the source branch used to create the
-- pull request, or in the case of an updated pull request, the full commit
-- ID of the commit used to update the pull request.
--
-- 'afterBlobId', 'postCommentForPullRequestResponse_afterBlobId' - In the directionality of the pull request, the blob ID of the after
-- blob.
--
-- 'pullRequestId', 'postCommentForPullRequestResponse_pullRequestId' - The system-generated ID of the pull request.
--
-- 'afterCommitId', 'postCommentForPullRequestResponse_afterCommitId' - The full commit ID of the commit in the destination branch where the
-- pull request is merged.
--
-- 'location', 'postCommentForPullRequestResponse_location' - The location of the change where you posted your comment.
--
-- 'httpStatus', 'postCommentForPullRequestResponse_httpStatus' - The response's http status code.
newPostCommentForPullRequestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PostCommentForPullRequestResponse
newPostCommentForPullRequestResponse pHttpStatus_ =
  PostCommentForPullRequestResponse'
    { beforeBlobId =
        Prelude.Nothing,
      comment = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      beforeCommitId = Prelude.Nothing,
      afterBlobId = Prelude.Nothing,
      pullRequestId = Prelude.Nothing,
      afterCommitId = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | In the directionality of the pull request, the blob ID of the before
-- blob.
postCommentForPullRequestResponse_beforeBlobId :: Lens.Lens' PostCommentForPullRequestResponse (Prelude.Maybe Prelude.Text)
postCommentForPullRequestResponse_beforeBlobId = Lens.lens (\PostCommentForPullRequestResponse' {beforeBlobId} -> beforeBlobId) (\s@PostCommentForPullRequestResponse' {} a -> s {beforeBlobId = a} :: PostCommentForPullRequestResponse)

-- | The content of the comment you posted.
postCommentForPullRequestResponse_comment :: Lens.Lens' PostCommentForPullRequestResponse (Prelude.Maybe Comment)
postCommentForPullRequestResponse_comment = Lens.lens (\PostCommentForPullRequestResponse' {comment} -> comment) (\s@PostCommentForPullRequestResponse' {} a -> s {comment = a} :: PostCommentForPullRequestResponse)

-- | The name of the repository where you posted a comment on a pull request.
postCommentForPullRequestResponse_repositoryName :: Lens.Lens' PostCommentForPullRequestResponse (Prelude.Maybe Prelude.Text)
postCommentForPullRequestResponse_repositoryName = Lens.lens (\PostCommentForPullRequestResponse' {repositoryName} -> repositoryName) (\s@PostCommentForPullRequestResponse' {} a -> s {repositoryName = a} :: PostCommentForPullRequestResponse)

-- | The full commit ID of the commit in the source branch used to create the
-- pull request, or in the case of an updated pull request, the full commit
-- ID of the commit used to update the pull request.
postCommentForPullRequestResponse_beforeCommitId :: Lens.Lens' PostCommentForPullRequestResponse (Prelude.Maybe Prelude.Text)
postCommentForPullRequestResponse_beforeCommitId = Lens.lens (\PostCommentForPullRequestResponse' {beforeCommitId} -> beforeCommitId) (\s@PostCommentForPullRequestResponse' {} a -> s {beforeCommitId = a} :: PostCommentForPullRequestResponse)

-- | In the directionality of the pull request, the blob ID of the after
-- blob.
postCommentForPullRequestResponse_afterBlobId :: Lens.Lens' PostCommentForPullRequestResponse (Prelude.Maybe Prelude.Text)
postCommentForPullRequestResponse_afterBlobId = Lens.lens (\PostCommentForPullRequestResponse' {afterBlobId} -> afterBlobId) (\s@PostCommentForPullRequestResponse' {} a -> s {afterBlobId = a} :: PostCommentForPullRequestResponse)

-- | The system-generated ID of the pull request.
postCommentForPullRequestResponse_pullRequestId :: Lens.Lens' PostCommentForPullRequestResponse (Prelude.Maybe Prelude.Text)
postCommentForPullRequestResponse_pullRequestId = Lens.lens (\PostCommentForPullRequestResponse' {pullRequestId} -> pullRequestId) (\s@PostCommentForPullRequestResponse' {} a -> s {pullRequestId = a} :: PostCommentForPullRequestResponse)

-- | The full commit ID of the commit in the destination branch where the
-- pull request is merged.
postCommentForPullRequestResponse_afterCommitId :: Lens.Lens' PostCommentForPullRequestResponse (Prelude.Maybe Prelude.Text)
postCommentForPullRequestResponse_afterCommitId = Lens.lens (\PostCommentForPullRequestResponse' {afterCommitId} -> afterCommitId) (\s@PostCommentForPullRequestResponse' {} a -> s {afterCommitId = a} :: PostCommentForPullRequestResponse)

-- | The location of the change where you posted your comment.
postCommentForPullRequestResponse_location :: Lens.Lens' PostCommentForPullRequestResponse (Prelude.Maybe Location)
postCommentForPullRequestResponse_location = Lens.lens (\PostCommentForPullRequestResponse' {location} -> location) (\s@PostCommentForPullRequestResponse' {} a -> s {location = a} :: PostCommentForPullRequestResponse)

-- | The response's http status code.
postCommentForPullRequestResponse_httpStatus :: Lens.Lens' PostCommentForPullRequestResponse Prelude.Int
postCommentForPullRequestResponse_httpStatus = Lens.lens (\PostCommentForPullRequestResponse' {httpStatus} -> httpStatus) (\s@PostCommentForPullRequestResponse' {} a -> s {httpStatus = a} :: PostCommentForPullRequestResponse)

instance
  Prelude.NFData
    PostCommentForPullRequestResponse
