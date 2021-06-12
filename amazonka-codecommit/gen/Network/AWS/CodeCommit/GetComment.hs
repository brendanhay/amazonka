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
-- Module      : Network.AWS.CodeCommit.GetComment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the content of a comment made on a change, file, or commit in a
-- repository.
--
-- Reaction counts might include numbers from user identities who were
-- deleted after the reaction was made. For a count of reactions from
-- active identities, use GetCommentReactions.
module Network.AWS.CodeCommit.GetComment
  ( -- * Creating a Request
    GetComment (..),
    newGetComment,

    -- * Request Lenses
    getComment_commentId,

    -- * Destructuring the Response
    GetCommentResponse (..),
    newGetCommentResponse,

    -- * Response Lenses
    getCommentResponse_comment,
    getCommentResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetComment' smart constructor.
data GetComment = GetComment'
  { -- | The unique, system-generated ID of the comment. To get this ID, use
    -- GetCommentsForComparedCommit or GetCommentsForPullRequest.
    commentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentId', 'getComment_commentId' - The unique, system-generated ID of the comment. To get this ID, use
-- GetCommentsForComparedCommit or GetCommentsForPullRequest.
newGetComment ::
  -- | 'commentId'
  Core.Text ->
  GetComment
newGetComment pCommentId_ =
  GetComment' {commentId = pCommentId_}

-- | The unique, system-generated ID of the comment. To get this ID, use
-- GetCommentsForComparedCommit or GetCommentsForPullRequest.
getComment_commentId :: Lens.Lens' GetComment Core.Text
getComment_commentId = Lens.lens (\GetComment' {commentId} -> commentId) (\s@GetComment' {} a -> s {commentId = a} :: GetComment)

instance Core.AWSRequest GetComment where
  type AWSResponse GetComment = GetCommentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentResponse'
            Core.<$> (x Core..?> "comment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetComment

instance Core.NFData GetComment

instance Core.ToHeaders GetComment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetComment" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetComment where
  toJSON GetComment' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("commentId" Core..= commentId)]
      )

instance Core.ToPath GetComment where
  toPath = Core.const "/"

instance Core.ToQuery GetComment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCommentResponse' smart constructor.
data GetCommentResponse = GetCommentResponse'
  { -- | The contents of the comment.
    comment :: Core.Maybe Comment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCommentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'getCommentResponse_comment' - The contents of the comment.
--
-- 'httpStatus', 'getCommentResponse_httpStatus' - The response's http status code.
newGetCommentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCommentResponse
newGetCommentResponse pHttpStatus_ =
  GetCommentResponse'
    { comment = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The contents of the comment.
getCommentResponse_comment :: Lens.Lens' GetCommentResponse (Core.Maybe Comment)
getCommentResponse_comment = Lens.lens (\GetCommentResponse' {comment} -> comment) (\s@GetCommentResponse' {} a -> s {comment = a} :: GetCommentResponse)

-- | The response's http status code.
getCommentResponse_httpStatus :: Lens.Lens' GetCommentResponse Core.Int
getCommentResponse_httpStatus = Lens.lens (\GetCommentResponse' {httpStatus} -> httpStatus) (\s@GetCommentResponse' {} a -> s {httpStatus = a} :: GetCommentResponse)

instance Core.NFData GetCommentResponse
