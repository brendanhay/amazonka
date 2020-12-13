{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new comment to the specified document version.
module Network.AWS.WorkDocs.CreateComment
  ( -- * Creating a request
    CreateComment (..),
    mkCreateComment,

    -- ** Request lenses
    ccVersionId,
    ccDocumentId,
    ccNotifyCollaborators,
    ccText,
    ccAuthenticationToken,
    ccVisibility,
    ccThreadId,
    ccParentId,

    -- * Destructuring the response
    CreateCommentResponse (..),
    mkCreateCommentResponse,

    -- ** Response lenses
    ccrsComment,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkCreateComment' smart constructor.
data CreateComment = CreateComment'
  { -- | The ID of the document version.
    versionId :: Lude.Text,
    -- | The ID of the document.
    documentId :: Lude.Text,
    -- | Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
    notifyCollaborators :: Lude.Maybe Lude.Bool,
    -- | The text of the comment.
    text :: Lude.Sensitive Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
    visibility :: Lude.Maybe CommentVisibilityType,
    -- | The ID of the root comment in the thread.
    threadId :: Lude.Maybe Lude.Text,
    -- | The ID of the parent comment.
    parentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateComment' with the minimum fields required to make a request.
--
-- * 'versionId' - The ID of the document version.
-- * 'documentId' - The ID of the document.
-- * 'notifyCollaborators' - Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
-- * 'text' - The text of the comment.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'visibility' - The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
-- * 'threadId' - The ID of the root comment in the thread.
-- * 'parentId' - The ID of the parent comment.
mkCreateComment ::
  -- | 'versionId'
  Lude.Text ->
  -- | 'documentId'
  Lude.Text ->
  -- | 'text'
  Lude.Sensitive Lude.Text ->
  CreateComment
mkCreateComment pVersionId_ pDocumentId_ pText_ =
  CreateComment'
    { versionId = pVersionId_,
      documentId = pDocumentId_,
      notifyCollaborators = Lude.Nothing,
      text = pText_,
      authenticationToken = Lude.Nothing,
      visibility = Lude.Nothing,
      threadId = Lude.Nothing,
      parentId = Lude.Nothing
    }

-- | The ID of the document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVersionId :: Lens.Lens' CreateComment Lude.Text
ccVersionId = Lens.lens (versionId :: CreateComment -> Lude.Text) (\s a -> s {versionId = a} :: CreateComment)
{-# DEPRECATED ccVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDocumentId :: Lens.Lens' CreateComment Lude.Text
ccDocumentId = Lens.lens (documentId :: CreateComment -> Lude.Text) (\s a -> s {documentId = a} :: CreateComment)
{-# DEPRECATED ccDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
--
-- /Note:/ Consider using 'notifyCollaborators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotifyCollaborators :: Lens.Lens' CreateComment (Lude.Maybe Lude.Bool)
ccNotifyCollaborators = Lens.lens (notifyCollaborators :: CreateComment -> Lude.Maybe Lude.Bool) (\s a -> s {notifyCollaborators = a} :: CreateComment)
{-# DEPRECATED ccNotifyCollaborators "Use generic-lens or generic-optics with 'notifyCollaborators' instead." #-}

-- | The text of the comment.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccText :: Lens.Lens' CreateComment (Lude.Sensitive Lude.Text)
ccText = Lens.lens (text :: CreateComment -> Lude.Sensitive Lude.Text) (\s a -> s {text = a} :: CreateComment)
{-# DEPRECATED ccText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthenticationToken :: Lens.Lens' CreateComment (Lude.Maybe (Lude.Sensitive Lude.Text))
ccAuthenticationToken = Lens.lens (authenticationToken :: CreateComment -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: CreateComment)
{-# DEPRECATED ccAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVisibility :: Lens.Lens' CreateComment (Lude.Maybe CommentVisibilityType)
ccVisibility = Lens.lens (visibility :: CreateComment -> Lude.Maybe CommentVisibilityType) (\s a -> s {visibility = a} :: CreateComment)
{-# DEPRECATED ccVisibility "Use generic-lens or generic-optics with 'visibility' instead." #-}

-- | The ID of the root comment in the thread.
--
-- /Note:/ Consider using 'threadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccThreadId :: Lens.Lens' CreateComment (Lude.Maybe Lude.Text)
ccThreadId = Lens.lens (threadId :: CreateComment -> Lude.Maybe Lude.Text) (\s a -> s {threadId = a} :: CreateComment)
{-# DEPRECATED ccThreadId "Use generic-lens or generic-optics with 'threadId' instead." #-}

-- | The ID of the parent comment.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccParentId :: Lens.Lens' CreateComment (Lude.Maybe Lude.Text)
ccParentId = Lens.lens (parentId :: CreateComment -> Lude.Maybe Lude.Text) (\s a -> s {parentId = a} :: CreateComment)
{-# DEPRECATED ccParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

instance Lude.AWSRequest CreateComment where
  type Rs CreateComment = CreateCommentResponse
  request = Req.postJSON workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCommentResponse'
            Lude.<$> (x Lude..?> "Comment") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateComment where
  toHeaders CreateComment' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON CreateComment where
  toJSON CreateComment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotifyCollaborators" Lude..=) Lude.<$> notifyCollaborators,
            Lude.Just ("Text" Lude..= text),
            ("Visibility" Lude..=) Lude.<$> visibility,
            ("ThreadId" Lude..=) Lude.<$> threadId,
            ("ParentId" Lude..=) Lude.<$> parentId
          ]
      )

instance Lude.ToPath CreateComment where
  toPath CreateComment' {..} =
    Lude.mconcat
      [ "/api/v1/documents/",
        Lude.toBS documentId,
        "/versions/",
        Lude.toBS versionId,
        "/comment"
      ]

instance Lude.ToQuery CreateComment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCommentResponse' smart constructor.
data CreateCommentResponse = CreateCommentResponse'
  { -- | The comment that has been created.
    comment :: Lude.Maybe Comment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCommentResponse' with the minimum fields required to make a request.
--
-- * 'comment' - The comment that has been created.
-- * 'responseStatus' - The response status code.
mkCreateCommentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCommentResponse
mkCreateCommentResponse pResponseStatus_ =
  CreateCommentResponse'
    { comment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The comment that has been created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsComment :: Lens.Lens' CreateCommentResponse (Lude.Maybe Comment)
ccrsComment = Lens.lens (comment :: CreateCommentResponse -> Lude.Maybe Comment) (\s a -> s {comment = a} :: CreateCommentResponse)
{-# DEPRECATED ccrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateCommentResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateCommentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCommentResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
