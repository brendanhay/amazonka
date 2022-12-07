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
-- Module      : Amazonka.CodeCommit.PostCommentForComparedCommit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on the comparison between two commits.
module Amazonka.CodeCommit.PostCommentForComparedCommit
  ( -- * Creating a Request
    PostCommentForComparedCommit (..),
    newPostCommentForComparedCommit,

    -- * Request Lenses
    postCommentForComparedCommit_clientRequestToken,
    postCommentForComparedCommit_beforeCommitId,
    postCommentForComparedCommit_location,
    postCommentForComparedCommit_repositoryName,
    postCommentForComparedCommit_afterCommitId,
    postCommentForComparedCommit_content,

    -- * Destructuring the Response
    PostCommentForComparedCommitResponse (..),
    newPostCommentForComparedCommitResponse,

    -- * Response Lenses
    postCommentForComparedCommitResponse_beforeBlobId,
    postCommentForComparedCommitResponse_afterCommitId,
    postCommentForComparedCommitResponse_repositoryName,
    postCommentForComparedCommitResponse_beforeCommitId,
    postCommentForComparedCommitResponse_location,
    postCommentForComparedCommitResponse_comment,
    postCommentForComparedCommitResponse_afterBlobId,
    postCommentForComparedCommitResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPostCommentForComparedCommit' smart constructor.
data PostCommentForComparedCommit = PostCommentForComparedCommit'
  { -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | To establish the directionality of the comparison, the full commit ID of
    -- the before commit. Required for commenting on any commit unless that
    -- commit is the initial commit.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | The location of the comparison where you want to comment.
    location :: Prelude.Maybe Location,
    -- | The name of the repository where you want to post a comment on the
    -- comparison between commits.
    repositoryName :: Prelude.Text,
    -- | To establish the directionality of the comparison, the full commit ID of
    -- the after commit.
    afterCommitId :: Prelude.Text,
    -- | The content of the comment you want to make.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostCommentForComparedCommit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'postCommentForComparedCommit_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- 'beforeCommitId', 'postCommentForComparedCommit_beforeCommitId' - To establish the directionality of the comparison, the full commit ID of
-- the before commit. Required for commenting on any commit unless that
-- commit is the initial commit.
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
  Prelude.Text ->
  -- | 'afterCommitId'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  PostCommentForComparedCommit
newPostCommentForComparedCommit
  pRepositoryName_
  pAfterCommitId_
  pContent_ =
    PostCommentForComparedCommit'
      { clientRequestToken =
          Prelude.Nothing,
        beforeCommitId = Prelude.Nothing,
        location = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        afterCommitId = pAfterCommitId_,
        content = pContent_
      }

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
postCommentForComparedCommit_clientRequestToken :: Lens.Lens' PostCommentForComparedCommit (Prelude.Maybe Prelude.Text)
postCommentForComparedCommit_clientRequestToken = Lens.lens (\PostCommentForComparedCommit' {clientRequestToken} -> clientRequestToken) (\s@PostCommentForComparedCommit' {} a -> s {clientRequestToken = a} :: PostCommentForComparedCommit)

-- | To establish the directionality of the comparison, the full commit ID of
-- the before commit. Required for commenting on any commit unless that
-- commit is the initial commit.
postCommentForComparedCommit_beforeCommitId :: Lens.Lens' PostCommentForComparedCommit (Prelude.Maybe Prelude.Text)
postCommentForComparedCommit_beforeCommitId = Lens.lens (\PostCommentForComparedCommit' {beforeCommitId} -> beforeCommitId) (\s@PostCommentForComparedCommit' {} a -> s {beforeCommitId = a} :: PostCommentForComparedCommit)

-- | The location of the comparison where you want to comment.
postCommentForComparedCommit_location :: Lens.Lens' PostCommentForComparedCommit (Prelude.Maybe Location)
postCommentForComparedCommit_location = Lens.lens (\PostCommentForComparedCommit' {location} -> location) (\s@PostCommentForComparedCommit' {} a -> s {location = a} :: PostCommentForComparedCommit)

-- | The name of the repository where you want to post a comment on the
-- comparison between commits.
postCommentForComparedCommit_repositoryName :: Lens.Lens' PostCommentForComparedCommit Prelude.Text
postCommentForComparedCommit_repositoryName = Lens.lens (\PostCommentForComparedCommit' {repositoryName} -> repositoryName) (\s@PostCommentForComparedCommit' {} a -> s {repositoryName = a} :: PostCommentForComparedCommit)

-- | To establish the directionality of the comparison, the full commit ID of
-- the after commit.
postCommentForComparedCommit_afterCommitId :: Lens.Lens' PostCommentForComparedCommit Prelude.Text
postCommentForComparedCommit_afterCommitId = Lens.lens (\PostCommentForComparedCommit' {afterCommitId} -> afterCommitId) (\s@PostCommentForComparedCommit' {} a -> s {afterCommitId = a} :: PostCommentForComparedCommit)

-- | The content of the comment you want to make.
postCommentForComparedCommit_content :: Lens.Lens' PostCommentForComparedCommit Prelude.Text
postCommentForComparedCommit_content = Lens.lens (\PostCommentForComparedCommit' {content} -> content) (\s@PostCommentForComparedCommit' {} a -> s {content = a} :: PostCommentForComparedCommit)

instance Core.AWSRequest PostCommentForComparedCommit where
  type
    AWSResponse PostCommentForComparedCommit =
      PostCommentForComparedCommitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PostCommentForComparedCommitResponse'
            Prelude.<$> (x Data..?> "beforeBlobId")
            Prelude.<*> (x Data..?> "afterCommitId")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (x Data..?> "beforeCommitId")
            Prelude.<*> (x Data..?> "location")
            Prelude.<*> (x Data..?> "comment")
            Prelude.<*> (x Data..?> "afterBlobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PostCommentForComparedCommit
  where
  hashWithSalt _salt PostCommentForComparedCommit' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` beforeCommitId
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` afterCommitId
      `Prelude.hashWithSalt` content

instance Prelude.NFData PostCommentForComparedCommit where
  rnf PostCommentForComparedCommit' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf beforeCommitId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf afterCommitId
      `Prelude.seq` Prelude.rnf content

instance Data.ToHeaders PostCommentForComparedCommit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.PostCommentForComparedCommit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PostCommentForComparedCommit where
  toJSON PostCommentForComparedCommit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("beforeCommitId" Data..=)
              Prelude.<$> beforeCommitId,
            ("location" Data..=) Prelude.<$> location,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("afterCommitId" Data..= afterCommitId),
            Prelude.Just ("content" Data..= content)
          ]
      )

instance Data.ToPath PostCommentForComparedCommit where
  toPath = Prelude.const "/"

instance Data.ToQuery PostCommentForComparedCommit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPostCommentForComparedCommitResponse' smart constructor.
data PostCommentForComparedCommitResponse = PostCommentForComparedCommitResponse'
  { -- | In the directionality you established, the blob ID of the before blob.
    beforeBlobId :: Prelude.Maybe Prelude.Text,
    -- | In the directionality you established, the full commit ID of the after
    -- commit.
    afterCommitId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where you posted a comment on the comparison
    -- between commits.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | In the directionality you established, the full commit ID of the before
    -- commit.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | The location of the comment in the comparison between the two commits.
    location :: Prelude.Maybe Location,
    -- | The content of the comment you posted.
    comment :: Prelude.Maybe Comment,
    -- | In the directionality you established, the blob ID of the after blob.
    afterBlobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'afterCommitId', 'postCommentForComparedCommitResponse_afterCommitId' - In the directionality you established, the full commit ID of the after
-- commit.
--
-- 'repositoryName', 'postCommentForComparedCommitResponse_repositoryName' - The name of the repository where you posted a comment on the comparison
-- between commits.
--
-- 'beforeCommitId', 'postCommentForComparedCommitResponse_beforeCommitId' - In the directionality you established, the full commit ID of the before
-- commit.
--
-- 'location', 'postCommentForComparedCommitResponse_location' - The location of the comment in the comparison between the two commits.
--
-- 'comment', 'postCommentForComparedCommitResponse_comment' - The content of the comment you posted.
--
-- 'afterBlobId', 'postCommentForComparedCommitResponse_afterBlobId' - In the directionality you established, the blob ID of the after blob.
--
-- 'httpStatus', 'postCommentForComparedCommitResponse_httpStatus' - The response's http status code.
newPostCommentForComparedCommitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PostCommentForComparedCommitResponse
newPostCommentForComparedCommitResponse pHttpStatus_ =
  PostCommentForComparedCommitResponse'
    { beforeBlobId =
        Prelude.Nothing,
      afterCommitId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      beforeCommitId = Prelude.Nothing,
      location = Prelude.Nothing,
      comment = Prelude.Nothing,
      afterBlobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | In the directionality you established, the blob ID of the before blob.
postCommentForComparedCommitResponse_beforeBlobId :: Lens.Lens' PostCommentForComparedCommitResponse (Prelude.Maybe Prelude.Text)
postCommentForComparedCommitResponse_beforeBlobId = Lens.lens (\PostCommentForComparedCommitResponse' {beforeBlobId} -> beforeBlobId) (\s@PostCommentForComparedCommitResponse' {} a -> s {beforeBlobId = a} :: PostCommentForComparedCommitResponse)

-- | In the directionality you established, the full commit ID of the after
-- commit.
postCommentForComparedCommitResponse_afterCommitId :: Lens.Lens' PostCommentForComparedCommitResponse (Prelude.Maybe Prelude.Text)
postCommentForComparedCommitResponse_afterCommitId = Lens.lens (\PostCommentForComparedCommitResponse' {afterCommitId} -> afterCommitId) (\s@PostCommentForComparedCommitResponse' {} a -> s {afterCommitId = a} :: PostCommentForComparedCommitResponse)

-- | The name of the repository where you posted a comment on the comparison
-- between commits.
postCommentForComparedCommitResponse_repositoryName :: Lens.Lens' PostCommentForComparedCommitResponse (Prelude.Maybe Prelude.Text)
postCommentForComparedCommitResponse_repositoryName = Lens.lens (\PostCommentForComparedCommitResponse' {repositoryName} -> repositoryName) (\s@PostCommentForComparedCommitResponse' {} a -> s {repositoryName = a} :: PostCommentForComparedCommitResponse)

-- | In the directionality you established, the full commit ID of the before
-- commit.
postCommentForComparedCommitResponse_beforeCommitId :: Lens.Lens' PostCommentForComparedCommitResponse (Prelude.Maybe Prelude.Text)
postCommentForComparedCommitResponse_beforeCommitId = Lens.lens (\PostCommentForComparedCommitResponse' {beforeCommitId} -> beforeCommitId) (\s@PostCommentForComparedCommitResponse' {} a -> s {beforeCommitId = a} :: PostCommentForComparedCommitResponse)

-- | The location of the comment in the comparison between the two commits.
postCommentForComparedCommitResponse_location :: Lens.Lens' PostCommentForComparedCommitResponse (Prelude.Maybe Location)
postCommentForComparedCommitResponse_location = Lens.lens (\PostCommentForComparedCommitResponse' {location} -> location) (\s@PostCommentForComparedCommitResponse' {} a -> s {location = a} :: PostCommentForComparedCommitResponse)

-- | The content of the comment you posted.
postCommentForComparedCommitResponse_comment :: Lens.Lens' PostCommentForComparedCommitResponse (Prelude.Maybe Comment)
postCommentForComparedCommitResponse_comment = Lens.lens (\PostCommentForComparedCommitResponse' {comment} -> comment) (\s@PostCommentForComparedCommitResponse' {} a -> s {comment = a} :: PostCommentForComparedCommitResponse)

-- | In the directionality you established, the blob ID of the after blob.
postCommentForComparedCommitResponse_afterBlobId :: Lens.Lens' PostCommentForComparedCommitResponse (Prelude.Maybe Prelude.Text)
postCommentForComparedCommitResponse_afterBlobId = Lens.lens (\PostCommentForComparedCommitResponse' {afterBlobId} -> afterBlobId) (\s@PostCommentForComparedCommitResponse' {} a -> s {afterBlobId = a} :: PostCommentForComparedCommitResponse)

-- | The response's http status code.
postCommentForComparedCommitResponse_httpStatus :: Lens.Lens' PostCommentForComparedCommitResponse Prelude.Int
postCommentForComparedCommitResponse_httpStatus = Lens.lens (\PostCommentForComparedCommitResponse' {httpStatus} -> httpStatus) (\s@PostCommentForComparedCommitResponse' {} a -> s {httpStatus = a} :: PostCommentForComparedCommitResponse)

instance
  Prelude.NFData
    PostCommentForComparedCommitResponse
  where
  rnf PostCommentForComparedCommitResponse' {..} =
    Prelude.rnf beforeBlobId
      `Prelude.seq` Prelude.rnf afterCommitId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf beforeCommitId
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf afterBlobId
      `Prelude.seq` Prelude.rnf httpStatus
