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
-- Module      : Amazonka.CodeCommit.DeleteCommentContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content of a comment made on a change, file, or commit in a
-- repository.
module Amazonka.CodeCommit.DeleteCommentContent
  ( -- * Creating a Request
    DeleteCommentContent (..),
    newDeleteCommentContent,

    -- * Request Lenses
    deleteCommentContent_commentId,

    -- * Destructuring the Response
    DeleteCommentContentResponse (..),
    newDeleteCommentContentResponse,

    -- * Response Lenses
    deleteCommentContentResponse_comment,
    deleteCommentContentResponse_httpStatus,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCommentContent' smart constructor.
data DeleteCommentContent = DeleteCommentContent'
  { -- | The unique, system-generated ID of the comment. To get this ID, use
    -- GetCommentsForComparedCommit or GetCommentsForPullRequest.
    commentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCommentContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentId', 'deleteCommentContent_commentId' - The unique, system-generated ID of the comment. To get this ID, use
-- GetCommentsForComparedCommit or GetCommentsForPullRequest.
newDeleteCommentContent ::
  -- | 'commentId'
  Prelude.Text ->
  DeleteCommentContent
newDeleteCommentContent pCommentId_ =
  DeleteCommentContent' {commentId = pCommentId_}

-- | The unique, system-generated ID of the comment. To get this ID, use
-- GetCommentsForComparedCommit or GetCommentsForPullRequest.
deleteCommentContent_commentId :: Lens.Lens' DeleteCommentContent Prelude.Text
deleteCommentContent_commentId = Lens.lens (\DeleteCommentContent' {commentId} -> commentId) (\s@DeleteCommentContent' {} a -> s {commentId = a} :: DeleteCommentContent)

instance Core.AWSRequest DeleteCommentContent where
  type
    AWSResponse DeleteCommentContent =
      DeleteCommentContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCommentContentResponse'
            Prelude.<$> (x Data..?> "comment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCommentContent where
  hashWithSalt _salt DeleteCommentContent' {..} =
    _salt `Prelude.hashWithSalt` commentId

instance Prelude.NFData DeleteCommentContent where
  rnf DeleteCommentContent' {..} = Prelude.rnf commentId

instance Data.ToHeaders DeleteCommentContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.DeleteCommentContent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCommentContent where
  toJSON DeleteCommentContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("commentId" Data..= commentId)]
      )

instance Data.ToPath DeleteCommentContent where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCommentContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCommentContentResponse' smart constructor.
data DeleteCommentContentResponse = DeleteCommentContentResponse'
  { -- | Information about the comment you just deleted.
    comment :: Prelude.Maybe Comment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCommentContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'deleteCommentContentResponse_comment' - Information about the comment you just deleted.
--
-- 'httpStatus', 'deleteCommentContentResponse_httpStatus' - The response's http status code.
newDeleteCommentContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCommentContentResponse
newDeleteCommentContentResponse pHttpStatus_ =
  DeleteCommentContentResponse'
    { comment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the comment you just deleted.
deleteCommentContentResponse_comment :: Lens.Lens' DeleteCommentContentResponse (Prelude.Maybe Comment)
deleteCommentContentResponse_comment = Lens.lens (\DeleteCommentContentResponse' {comment} -> comment) (\s@DeleteCommentContentResponse' {} a -> s {comment = a} :: DeleteCommentContentResponse)

-- | The response's http status code.
deleteCommentContentResponse_httpStatus :: Lens.Lens' DeleteCommentContentResponse Prelude.Int
deleteCommentContentResponse_httpStatus = Lens.lens (\DeleteCommentContentResponse' {httpStatus} -> httpStatus) (\s@DeleteCommentContentResponse' {} a -> s {httpStatus = a} :: DeleteCommentContentResponse)

instance Prelude.NFData DeleteCommentContentResponse where
  rnf DeleteCommentContentResponse' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf httpStatus
