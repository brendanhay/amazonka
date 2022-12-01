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
-- Module      : Amazonka.CodeCommit.GetComment
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodeCommit.GetComment
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComment' smart constructor.
data GetComment = GetComment'
  { -- | The unique, system-generated ID of the comment. To get this ID, use
    -- GetCommentsForComparedCommit or GetCommentsForPullRequest.
    commentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetComment
newGetComment pCommentId_ =
  GetComment' {commentId = pCommentId_}

-- | The unique, system-generated ID of the comment. To get this ID, use
-- GetCommentsForComparedCommit or GetCommentsForPullRequest.
getComment_commentId :: Lens.Lens' GetComment Prelude.Text
getComment_commentId = Lens.lens (\GetComment' {commentId} -> commentId) (\s@GetComment' {} a -> s {commentId = a} :: GetComment)

instance Core.AWSRequest GetComment where
  type AWSResponse GetComment = GetCommentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentResponse'
            Prelude.<$> (x Core..?> "comment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComment where
  hashWithSalt _salt GetComment' {..} =
    _salt `Prelude.hashWithSalt` commentId

instance Prelude.NFData GetComment where
  rnf GetComment' {..} = Prelude.rnf commentId

instance Core.ToHeaders GetComment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetComment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetComment where
  toJSON GetComment' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("commentId" Core..= commentId)]
      )

instance Core.ToPath GetComment where
  toPath = Prelude.const "/"

instance Core.ToQuery GetComment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCommentResponse' smart constructor.
data GetCommentResponse = GetCommentResponse'
  { -- | The contents of the comment.
    comment :: Prelude.Maybe Comment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetCommentResponse
newGetCommentResponse pHttpStatus_ =
  GetCommentResponse'
    { comment = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The contents of the comment.
getCommentResponse_comment :: Lens.Lens' GetCommentResponse (Prelude.Maybe Comment)
getCommentResponse_comment = Lens.lens (\GetCommentResponse' {comment} -> comment) (\s@GetCommentResponse' {} a -> s {comment = a} :: GetCommentResponse)

-- | The response's http status code.
getCommentResponse_httpStatus :: Lens.Lens' GetCommentResponse Prelude.Int
getCommentResponse_httpStatus = Lens.lens (\GetCommentResponse' {httpStatus} -> httpStatus) (\s@GetCommentResponse' {} a -> s {httpStatus = a} :: GetCommentResponse)

instance Prelude.NFData GetCommentResponse where
  rnf GetCommentResponse' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf httpStatus
