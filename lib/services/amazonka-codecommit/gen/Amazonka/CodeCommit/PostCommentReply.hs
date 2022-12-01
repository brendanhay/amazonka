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
-- Module      : Amazonka.CodeCommit.PostCommentReply
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment in reply to an existing comment on a comparison between
-- commits or a pull request.
module Amazonka.CodeCommit.PostCommentReply
  ( -- * Creating a Request
    PostCommentReply (..),
    newPostCommentReply,

    -- * Request Lenses
    postCommentReply_clientRequestToken,
    postCommentReply_inReplyTo,
    postCommentReply_content,

    -- * Destructuring the Response
    PostCommentReplyResponse (..),
    newPostCommentReplyResponse,

    -- * Response Lenses
    postCommentReplyResponse_comment,
    postCommentReplyResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPostCommentReply' smart constructor.
data PostCommentReply = PostCommentReply'
  { -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the comment to which you want to reply. To
    -- get this ID, use GetCommentsForComparedCommit or
    -- GetCommentsForPullRequest.
    inReplyTo :: Prelude.Text,
    -- | The contents of your reply to a comment.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostCommentReply' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'postCommentReply_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- 'inReplyTo', 'postCommentReply_inReplyTo' - The system-generated ID of the comment to which you want to reply. To
-- get this ID, use GetCommentsForComparedCommit or
-- GetCommentsForPullRequest.
--
-- 'content', 'postCommentReply_content' - The contents of your reply to a comment.
newPostCommentReply ::
  -- | 'inReplyTo'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  PostCommentReply
newPostCommentReply pInReplyTo_ pContent_ =
  PostCommentReply'
    { clientRequestToken =
        Prelude.Nothing,
      inReplyTo = pInReplyTo_,
      content = pContent_
    }

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
postCommentReply_clientRequestToken :: Lens.Lens' PostCommentReply (Prelude.Maybe Prelude.Text)
postCommentReply_clientRequestToken = Lens.lens (\PostCommentReply' {clientRequestToken} -> clientRequestToken) (\s@PostCommentReply' {} a -> s {clientRequestToken = a} :: PostCommentReply)

-- | The system-generated ID of the comment to which you want to reply. To
-- get this ID, use GetCommentsForComparedCommit or
-- GetCommentsForPullRequest.
postCommentReply_inReplyTo :: Lens.Lens' PostCommentReply Prelude.Text
postCommentReply_inReplyTo = Lens.lens (\PostCommentReply' {inReplyTo} -> inReplyTo) (\s@PostCommentReply' {} a -> s {inReplyTo = a} :: PostCommentReply)

-- | The contents of your reply to a comment.
postCommentReply_content :: Lens.Lens' PostCommentReply Prelude.Text
postCommentReply_content = Lens.lens (\PostCommentReply' {content} -> content) (\s@PostCommentReply' {} a -> s {content = a} :: PostCommentReply)

instance Core.AWSRequest PostCommentReply where
  type
    AWSResponse PostCommentReply =
      PostCommentReplyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PostCommentReplyResponse'
            Prelude.<$> (x Core..?> "comment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PostCommentReply where
  hashWithSalt _salt PostCommentReply' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` inReplyTo
      `Prelude.hashWithSalt` content

instance Prelude.NFData PostCommentReply where
  rnf PostCommentReply' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf inReplyTo
      `Prelude.seq` Prelude.rnf content

instance Core.ToHeaders PostCommentReply where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.PostCommentReply" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PostCommentReply where
  toJSON PostCommentReply' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("inReplyTo" Core..= inReplyTo),
            Prelude.Just ("content" Core..= content)
          ]
      )

instance Core.ToPath PostCommentReply where
  toPath = Prelude.const "/"

instance Core.ToQuery PostCommentReply where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPostCommentReplyResponse' smart constructor.
data PostCommentReplyResponse = PostCommentReplyResponse'
  { -- | Information about the reply to a comment.
    comment :: Prelude.Maybe Comment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostCommentReplyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'postCommentReplyResponse_comment' - Information about the reply to a comment.
--
-- 'httpStatus', 'postCommentReplyResponse_httpStatus' - The response's http status code.
newPostCommentReplyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PostCommentReplyResponse
newPostCommentReplyResponse pHttpStatus_ =
  PostCommentReplyResponse'
    { comment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the reply to a comment.
postCommentReplyResponse_comment :: Lens.Lens' PostCommentReplyResponse (Prelude.Maybe Comment)
postCommentReplyResponse_comment = Lens.lens (\PostCommentReplyResponse' {comment} -> comment) (\s@PostCommentReplyResponse' {} a -> s {comment = a} :: PostCommentReplyResponse)

-- | The response's http status code.
postCommentReplyResponse_httpStatus :: Lens.Lens' PostCommentReplyResponse Prelude.Int
postCommentReplyResponse_httpStatus = Lens.lens (\PostCommentReplyResponse' {httpStatus} -> httpStatus) (\s@PostCommentReplyResponse' {} a -> s {httpStatus = a} :: PostCommentReplyResponse)

instance Prelude.NFData PostCommentReplyResponse where
  rnf PostCommentReplyResponse' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf httpStatus
