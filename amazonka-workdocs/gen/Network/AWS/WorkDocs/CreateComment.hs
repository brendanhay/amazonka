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
-- Module      : Network.AWS.WorkDocs.CreateComment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new comment to the specified document version.
module Network.AWS.WorkDocs.CreateComment
  ( -- * Creating a Request
    CreateComment (..),
    newCreateComment,

    -- * Request Lenses
    createComment_parentId,
    createComment_visibility,
    createComment_authenticationToken,
    createComment_threadId,
    createComment_notifyCollaborators,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newCreateComment' smart constructor.
data CreateComment = CreateComment'
  { -- | The ID of the parent comment.
    parentId :: Core.Maybe Core.Text,
    -- | The visibility of the comment. Options are either PRIVATE, where the
    -- comment is visible only to the comment author and document owner and
    -- co-owners, or PUBLIC, where the comment is visible to document owners,
    -- co-owners, and contributors.
    visibility :: Core.Maybe CommentVisibilityType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the root comment in the thread.
    threadId :: Core.Maybe Core.Text,
    -- | Set this parameter to TRUE to send an email out to the document
    -- collaborators after the comment is created.
    notifyCollaborators :: Core.Maybe Core.Bool,
    -- | The ID of the document.
    documentId :: Core.Text,
    -- | The ID of the document version.
    versionId :: Core.Text,
    -- | The text of the comment.
    text :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateComment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentId', 'createComment_parentId' - The ID of the parent comment.
--
-- 'visibility', 'createComment_visibility' - The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
--
-- 'authenticationToken', 'createComment_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'threadId', 'createComment_threadId' - The ID of the root comment in the thread.
--
-- 'notifyCollaborators', 'createComment_notifyCollaborators' - Set this parameter to TRUE to send an email out to the document
-- collaborators after the comment is created.
--
-- 'documentId', 'createComment_documentId' - The ID of the document.
--
-- 'versionId', 'createComment_versionId' - The ID of the document version.
--
-- 'text', 'createComment_text' - The text of the comment.
newCreateComment ::
  -- | 'documentId'
  Core.Text ->
  -- | 'versionId'
  Core.Text ->
  -- | 'text'
  Core.Text ->
  CreateComment
newCreateComment pDocumentId_ pVersionId_ pText_ =
  CreateComment'
    { parentId = Core.Nothing,
      visibility = Core.Nothing,
      authenticationToken = Core.Nothing,
      threadId = Core.Nothing,
      notifyCollaborators = Core.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_,
      text = Core._Sensitive Lens.# pText_
    }

-- | The ID of the parent comment.
createComment_parentId :: Lens.Lens' CreateComment (Core.Maybe Core.Text)
createComment_parentId = Lens.lens (\CreateComment' {parentId} -> parentId) (\s@CreateComment' {} a -> s {parentId = a} :: CreateComment)

-- | The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
createComment_visibility :: Lens.Lens' CreateComment (Core.Maybe CommentVisibilityType)
createComment_visibility = Lens.lens (\CreateComment' {visibility} -> visibility) (\s@CreateComment' {} a -> s {visibility = a} :: CreateComment)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
createComment_authenticationToken :: Lens.Lens' CreateComment (Core.Maybe Core.Text)
createComment_authenticationToken = Lens.lens (\CreateComment' {authenticationToken} -> authenticationToken) (\s@CreateComment' {} a -> s {authenticationToken = a} :: CreateComment) Core.. Lens.mapping Core._Sensitive

-- | The ID of the root comment in the thread.
createComment_threadId :: Lens.Lens' CreateComment (Core.Maybe Core.Text)
createComment_threadId = Lens.lens (\CreateComment' {threadId} -> threadId) (\s@CreateComment' {} a -> s {threadId = a} :: CreateComment)

-- | Set this parameter to TRUE to send an email out to the document
-- collaborators after the comment is created.
createComment_notifyCollaborators :: Lens.Lens' CreateComment (Core.Maybe Core.Bool)
createComment_notifyCollaborators = Lens.lens (\CreateComment' {notifyCollaborators} -> notifyCollaborators) (\s@CreateComment' {} a -> s {notifyCollaborators = a} :: CreateComment)

-- | The ID of the document.
createComment_documentId :: Lens.Lens' CreateComment Core.Text
createComment_documentId = Lens.lens (\CreateComment' {documentId} -> documentId) (\s@CreateComment' {} a -> s {documentId = a} :: CreateComment)

-- | The ID of the document version.
createComment_versionId :: Lens.Lens' CreateComment Core.Text
createComment_versionId = Lens.lens (\CreateComment' {versionId} -> versionId) (\s@CreateComment' {} a -> s {versionId = a} :: CreateComment)

-- | The text of the comment.
createComment_text :: Lens.Lens' CreateComment Core.Text
createComment_text = Lens.lens (\CreateComment' {text} -> text) (\s@CreateComment' {} a -> s {text = a} :: CreateComment) Core.. Core._Sensitive

instance Core.AWSRequest CreateComment where
  type
    AWSResponse CreateComment =
      CreateCommentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCommentResponse'
            Core.<$> (x Core..?> "Comment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateComment

instance Core.NFData CreateComment

instance Core.ToHeaders CreateComment where
  toHeaders CreateComment' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateComment where
  toJSON CreateComment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ParentId" Core..=) Core.<$> parentId,
            ("Visibility" Core..=) Core.<$> visibility,
            ("ThreadId" Core..=) Core.<$> threadId,
            ("NotifyCollaborators" Core..=)
              Core.<$> notifyCollaborators,
            Core.Just ("Text" Core..= text)
          ]
      )

instance Core.ToPath CreateComment where
  toPath CreateComment' {..} =
    Core.mconcat
      [ "/api/v1/documents/",
        Core.toBS documentId,
        "/versions/",
        Core.toBS versionId,
        "/comment"
      ]

instance Core.ToQuery CreateComment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCommentResponse' smart constructor.
data CreateCommentResponse = CreateCommentResponse'
  { -- | The comment that has been created.
    comment :: Core.Maybe Comment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateCommentResponse
newCreateCommentResponse pHttpStatus_ =
  CreateCommentResponse'
    { comment = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The comment that has been created.
createCommentResponse_comment :: Lens.Lens' CreateCommentResponse (Core.Maybe Comment)
createCommentResponse_comment = Lens.lens (\CreateCommentResponse' {comment} -> comment) (\s@CreateCommentResponse' {} a -> s {comment = a} :: CreateCommentResponse)

-- | The response's http status code.
createCommentResponse_httpStatus :: Lens.Lens' CreateCommentResponse Core.Int
createCommentResponse_httpStatus = Lens.lens (\CreateCommentResponse' {httpStatus} -> httpStatus) (\s@CreateCommentResponse' {} a -> s {httpStatus = a} :: CreateCommentResponse)

instance Core.NFData CreateCommentResponse
