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
-- Module      : Amazonka.WorkDocs.CreateComment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new comment to the specified document version.
module Amazonka.WorkDocs.CreateComment
  ( -- * Creating a Request
    CreateComment (..),
    newCreateComment,

    -- * Request Lenses
    createComment_threadId,
    createComment_notifyCollaborators,
    createComment_visibility,
    createComment_parentId,
    createComment_authenticationToken,
    createComment_documentId,
    createComment_versionId,
    createComment_text,

    -- * Destructuring the Response
    CreateCommentResponse (..),
    newCreateCommentResponse,

    -- * Response Lenses
    createCommentResponse_comment,
    createCommentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newCreateComment' smart constructor.
data CreateComment = CreateComment'
  { -- | The ID of the root comment in the thread.
    threadId :: Prelude.Maybe Prelude.Text,
    -- | Set this parameter to TRUE to send an email out to the document
    -- collaborators after the comment is created.
    notifyCollaborators :: Prelude.Maybe Prelude.Bool,
    -- | The visibility of the comment. Options are either PRIVATE, where the
    -- comment is visible only to the comment author and document owner and
    -- co-owners, or PUBLIC, where the comment is visible to document owners,
    -- co-owners, and contributors.
    visibility :: Prelude.Maybe CommentVisibilityType,
    -- | The ID of the parent comment.
    parentId :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the document.
    documentId :: Prelude.Text,
    -- | The ID of the document version.
    versionId :: Prelude.Text,
    -- | The text of the comment.
    text :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threadId', 'createComment_threadId' - The ID of the root comment in the thread.
--
-- 'notifyCollaborators', 'createComment_notifyCollaborators' - Set this parameter to TRUE to send an email out to the document
-- collaborators after the comment is created.
--
-- 'visibility', 'createComment_visibility' - The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
--
-- 'parentId', 'createComment_parentId' - The ID of the parent comment.
--
-- 'authenticationToken', 'createComment_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'documentId', 'createComment_documentId' - The ID of the document.
--
-- 'versionId', 'createComment_versionId' - The ID of the document version.
--
-- 'text', 'createComment_text' - The text of the comment.
newCreateComment ::
  -- | 'documentId'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  CreateComment
newCreateComment pDocumentId_ pVersionId_ pText_ =
  CreateComment'
    { threadId = Prelude.Nothing,
      notifyCollaborators = Prelude.Nothing,
      visibility = Prelude.Nothing,
      parentId = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_,
      text = Data._Sensitive Lens.# pText_
    }

-- | The ID of the root comment in the thread.
createComment_threadId :: Lens.Lens' CreateComment (Prelude.Maybe Prelude.Text)
createComment_threadId = Lens.lens (\CreateComment' {threadId} -> threadId) (\s@CreateComment' {} a -> s {threadId = a} :: CreateComment)

-- | Set this parameter to TRUE to send an email out to the document
-- collaborators after the comment is created.
createComment_notifyCollaborators :: Lens.Lens' CreateComment (Prelude.Maybe Prelude.Bool)
createComment_notifyCollaborators = Lens.lens (\CreateComment' {notifyCollaborators} -> notifyCollaborators) (\s@CreateComment' {} a -> s {notifyCollaborators = a} :: CreateComment)

-- | The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
createComment_visibility :: Lens.Lens' CreateComment (Prelude.Maybe CommentVisibilityType)
createComment_visibility = Lens.lens (\CreateComment' {visibility} -> visibility) (\s@CreateComment' {} a -> s {visibility = a} :: CreateComment)

-- | The ID of the parent comment.
createComment_parentId :: Lens.Lens' CreateComment (Prelude.Maybe Prelude.Text)
createComment_parentId = Lens.lens (\CreateComment' {parentId} -> parentId) (\s@CreateComment' {} a -> s {parentId = a} :: CreateComment)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
createComment_authenticationToken :: Lens.Lens' CreateComment (Prelude.Maybe Prelude.Text)
createComment_authenticationToken = Lens.lens (\CreateComment' {authenticationToken} -> authenticationToken) (\s@CreateComment' {} a -> s {authenticationToken = a} :: CreateComment) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the document.
createComment_documentId :: Lens.Lens' CreateComment Prelude.Text
createComment_documentId = Lens.lens (\CreateComment' {documentId} -> documentId) (\s@CreateComment' {} a -> s {documentId = a} :: CreateComment)

-- | The ID of the document version.
createComment_versionId :: Lens.Lens' CreateComment Prelude.Text
createComment_versionId = Lens.lens (\CreateComment' {versionId} -> versionId) (\s@CreateComment' {} a -> s {versionId = a} :: CreateComment)

-- | The text of the comment.
createComment_text :: Lens.Lens' CreateComment Prelude.Text
createComment_text = Lens.lens (\CreateComment' {text} -> text) (\s@CreateComment' {} a -> s {text = a} :: CreateComment) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateComment where
  type
    AWSResponse CreateComment =
      CreateCommentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCommentResponse'
            Prelude.<$> (x Data..?> "Comment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateComment where
  hashWithSalt _salt CreateComment' {..} =
    _salt `Prelude.hashWithSalt` threadId
      `Prelude.hashWithSalt` notifyCollaborators
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` text

instance Prelude.NFData CreateComment where
  rnf CreateComment' {..} =
    Prelude.rnf threadId
      `Prelude.seq` Prelude.rnf notifyCollaborators
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf parentId
      `Prelude.seq` Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf text

instance Data.ToHeaders CreateComment where
  toHeaders CreateComment' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateComment where
  toJSON CreateComment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ThreadId" Data..=) Prelude.<$> threadId,
            ("NotifyCollaborators" Data..=)
              Prelude.<$> notifyCollaborators,
            ("Visibility" Data..=) Prelude.<$> visibility,
            ("ParentId" Data..=) Prelude.<$> parentId,
            Prelude.Just ("Text" Data..= text)
          ]
      )

instance Data.ToPath CreateComment where
  toPath CreateComment' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Data.toBS documentId,
        "/versions/",
        Data.toBS versionId,
        "/comment"
      ]

instance Data.ToQuery CreateComment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCommentResponse' smart constructor.
data CreateCommentResponse = CreateCommentResponse'
  { -- | The comment that has been created.
    comment :: Prelude.Maybe Comment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCommentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'createCommentResponse_comment' - The comment that has been created.
--
-- 'httpStatus', 'createCommentResponse_httpStatus' - The response's http status code.
newCreateCommentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCommentResponse
newCreateCommentResponse pHttpStatus_ =
  CreateCommentResponse'
    { comment = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The comment that has been created.
createCommentResponse_comment :: Lens.Lens' CreateCommentResponse (Prelude.Maybe Comment)
createCommentResponse_comment = Lens.lens (\CreateCommentResponse' {comment} -> comment) (\s@CreateCommentResponse' {} a -> s {comment = a} :: CreateCommentResponse)

-- | The response's http status code.
createCommentResponse_httpStatus :: Lens.Lens' CreateCommentResponse Prelude.Int
createCommentResponse_httpStatus = Lens.lens (\CreateCommentResponse' {httpStatus} -> httpStatus) (\s@CreateCommentResponse' {} a -> s {httpStatus = a} :: CreateCommentResponse)

instance Prelude.NFData CreateCommentResponse where
  rnf CreateCommentResponse' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf httpStatus
