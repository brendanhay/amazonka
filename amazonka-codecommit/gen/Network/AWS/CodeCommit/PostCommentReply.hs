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
-- Module      : Network.AWS.CodeCommit.PostCommentReply
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment in reply to an existing comment on a comparison between
-- commits or a pull request.
module Network.AWS.CodeCommit.PostCommentReply
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

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPostCommentReply' smart constructor.
data PostCommentReply = PostCommentReply'
  { -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The system-generated ID of the comment to which you want to reply. To
    -- get this ID, use GetCommentsForComparedCommit or
    -- GetCommentsForPullRequest.
    inReplyTo :: Core.Text,
    -- | The contents of your reply to a comment.
    content :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'content'
  Core.Text ->
  PostCommentReply
newPostCommentReply pInReplyTo_ pContent_ =
  PostCommentReply'
    { clientRequestToken =
        Core.Nothing,
      inReplyTo = pInReplyTo_,
      content = pContent_
    }

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
postCommentReply_clientRequestToken :: Lens.Lens' PostCommentReply (Core.Maybe Core.Text)
postCommentReply_clientRequestToken = Lens.lens (\PostCommentReply' {clientRequestToken} -> clientRequestToken) (\s@PostCommentReply' {} a -> s {clientRequestToken = a} :: PostCommentReply)

-- | The system-generated ID of the comment to which you want to reply. To
-- get this ID, use GetCommentsForComparedCommit or
-- GetCommentsForPullRequest.
postCommentReply_inReplyTo :: Lens.Lens' PostCommentReply Core.Text
postCommentReply_inReplyTo = Lens.lens (\PostCommentReply' {inReplyTo} -> inReplyTo) (\s@PostCommentReply' {} a -> s {inReplyTo = a} :: PostCommentReply)

-- | The contents of your reply to a comment.
postCommentReply_content :: Lens.Lens' PostCommentReply Core.Text
postCommentReply_content = Lens.lens (\PostCommentReply' {content} -> content) (\s@PostCommentReply' {} a -> s {content = a} :: PostCommentReply)

instance Core.AWSRequest PostCommentReply where
  type
    AWSResponse PostCommentReply =
      PostCommentReplyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PostCommentReplyResponse'
            Core.<$> (x Core..?> "comment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PostCommentReply

instance Core.NFData PostCommentReply

instance Core.ToHeaders PostCommentReply where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.PostCommentReply" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PostCommentReply where
  toJSON PostCommentReply' {..} =
    Core.object
      ( Core.catMaybes
          [ ("clientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("inReplyTo" Core..= inReplyTo),
            Core.Just ("content" Core..= content)
          ]
      )

instance Core.ToPath PostCommentReply where
  toPath = Core.const "/"

instance Core.ToQuery PostCommentReply where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPostCommentReplyResponse' smart constructor.
data PostCommentReplyResponse = PostCommentReplyResponse'
  { -- | Information about the reply to a comment.
    comment :: Core.Maybe Comment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PostCommentReplyResponse
newPostCommentReplyResponse pHttpStatus_ =
  PostCommentReplyResponse'
    { comment = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the reply to a comment.
postCommentReplyResponse_comment :: Lens.Lens' PostCommentReplyResponse (Core.Maybe Comment)
postCommentReplyResponse_comment = Lens.lens (\PostCommentReplyResponse' {comment} -> comment) (\s@PostCommentReplyResponse' {} a -> s {comment = a} :: PostCommentReplyResponse)

-- | The response's http status code.
postCommentReplyResponse_httpStatus :: Lens.Lens' PostCommentReplyResponse Core.Int
postCommentReplyResponse_httpStatus = Lens.lens (\PostCommentReplyResponse' {httpStatus} -> httpStatus) (\s@PostCommentReplyResponse' {} a -> s {httpStatus = a} :: PostCommentReplyResponse)

instance Core.NFData PostCommentReplyResponse
