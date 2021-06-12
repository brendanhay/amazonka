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
-- Module      : Network.AWS.CodeCommit.UpdateComment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of a comment.
module Network.AWS.CodeCommit.UpdateComment
  ( -- * Creating a Request
    UpdateComment (..),
    newUpdateComment,

    -- * Request Lenses
    updateComment_commentId,
    updateComment_content,

    -- * Destructuring the Response
    UpdateCommentResponse (..),
    newUpdateCommentResponse,

    -- * Response Lenses
    updateCommentResponse_comment,
    updateCommentResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateComment' smart constructor.
data UpdateComment = UpdateComment'
  { -- | The system-generated ID of the comment you want to update. To get this
    -- ID, use GetCommentsForComparedCommit or GetCommentsForPullRequest.
    commentId :: Core.Text,
    -- | The updated content to replace the existing content of the comment.
    content :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateComment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentId', 'updateComment_commentId' - The system-generated ID of the comment you want to update. To get this
-- ID, use GetCommentsForComparedCommit or GetCommentsForPullRequest.
--
-- 'content', 'updateComment_content' - The updated content to replace the existing content of the comment.
newUpdateComment ::
  -- | 'commentId'
  Core.Text ->
  -- | 'content'
  Core.Text ->
  UpdateComment
newUpdateComment pCommentId_ pContent_ =
  UpdateComment'
    { commentId = pCommentId_,
      content = pContent_
    }

-- | The system-generated ID of the comment you want to update. To get this
-- ID, use GetCommentsForComparedCommit or GetCommentsForPullRequest.
updateComment_commentId :: Lens.Lens' UpdateComment Core.Text
updateComment_commentId = Lens.lens (\UpdateComment' {commentId} -> commentId) (\s@UpdateComment' {} a -> s {commentId = a} :: UpdateComment)

-- | The updated content to replace the existing content of the comment.
updateComment_content :: Lens.Lens' UpdateComment Core.Text
updateComment_content = Lens.lens (\UpdateComment' {content} -> content) (\s@UpdateComment' {} a -> s {content = a} :: UpdateComment)

instance Core.AWSRequest UpdateComment where
  type
    AWSResponse UpdateComment =
      UpdateCommentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCommentResponse'
            Core.<$> (x Core..?> "comment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateComment

instance Core.NFData UpdateComment

instance Core.ToHeaders UpdateComment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.UpdateComment" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateComment where
  toJSON UpdateComment' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("commentId" Core..= commentId),
            Core.Just ("content" Core..= content)
          ]
      )

instance Core.ToPath UpdateComment where
  toPath = Core.const "/"

instance Core.ToQuery UpdateComment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCommentResponse' smart constructor.
data UpdateCommentResponse = UpdateCommentResponse'
  { -- | Information about the updated comment.
    comment :: Core.Maybe Comment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCommentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'updateCommentResponse_comment' - Information about the updated comment.
--
-- 'httpStatus', 'updateCommentResponse_httpStatus' - The response's http status code.
newUpdateCommentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateCommentResponse
newUpdateCommentResponse pHttpStatus_ =
  UpdateCommentResponse'
    { comment = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the updated comment.
updateCommentResponse_comment :: Lens.Lens' UpdateCommentResponse (Core.Maybe Comment)
updateCommentResponse_comment = Lens.lens (\UpdateCommentResponse' {comment} -> comment) (\s@UpdateCommentResponse' {} a -> s {comment = a} :: UpdateCommentResponse)

-- | The response's http status code.
updateCommentResponse_httpStatus :: Lens.Lens' UpdateCommentResponse Core.Int
updateCommentResponse_httpStatus = Lens.lens (\UpdateCommentResponse' {httpStatus} -> httpStatus) (\s@UpdateCommentResponse' {} a -> s {httpStatus = a} :: UpdateCommentResponse)

instance Core.NFData UpdateCommentResponse
