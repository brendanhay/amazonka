{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified comment from the document version.
module Network.AWS.WorkDocs.DeleteComment
  ( -- * Creating a request
    DeleteComment (..),
    mkDeleteComment,

    -- ** Request lenses
    delAuthenticationToken,
    delDocumentId,
    delVersionId,
    delCommentId,

    -- * Destructuring the response
    DeleteCommentResponse (..),
    mkDeleteCommentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDeleteComment' smart constructor.
data DeleteComment = DeleteComment'
  { authenticationToken ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    documentId :: Lude.Text,
    versionId :: Lude.Text,
    commentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteComment' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'commentId' - The ID of the comment.
-- * 'documentId' - The ID of the document.
-- * 'versionId' - The ID of the document version.
mkDeleteComment ::
  -- | 'documentId'
  Lude.Text ->
  -- | 'versionId'
  Lude.Text ->
  -- | 'commentId'
  Lude.Text ->
  DeleteComment
mkDeleteComment pDocumentId_ pVersionId_ pCommentId_ =
  DeleteComment'
    { authenticationToken = Lude.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_,
      commentId = pCommentId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delAuthenticationToken :: Lens.Lens' DeleteComment (Lude.Maybe (Lude.Sensitive Lude.Text))
delAuthenticationToken = Lens.lens (authenticationToken :: DeleteComment -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DeleteComment)
{-# DEPRECATED delAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delDocumentId :: Lens.Lens' DeleteComment Lude.Text
delDocumentId = Lens.lens (documentId :: DeleteComment -> Lude.Text) (\s a -> s {documentId = a} :: DeleteComment)
{-# DEPRECATED delDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | The ID of the document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delVersionId :: Lens.Lens' DeleteComment Lude.Text
delVersionId = Lens.lens (versionId :: DeleteComment -> Lude.Text) (\s a -> s {versionId = a} :: DeleteComment)
{-# DEPRECATED delVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the comment.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delCommentId :: Lens.Lens' DeleteComment Lude.Text
delCommentId = Lens.lens (commentId :: DeleteComment -> Lude.Text) (\s a -> s {commentId = a} :: DeleteComment)
{-# DEPRECATED delCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

instance Lude.AWSRequest DeleteComment where
  type Rs DeleteComment = DeleteCommentResponse
  request = Req.delete workDocsService
  response = Res.receiveNull DeleteCommentResponse'

instance Lude.ToHeaders DeleteComment where
  toHeaders DeleteComment' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DeleteComment where
  toPath DeleteComment' {..} =
    Lude.mconcat
      [ "/api/v1/documents/",
        Lude.toBS documentId,
        "/versions/",
        Lude.toBS versionId,
        "/comment/",
        Lude.toBS commentId
      ]

instance Lude.ToQuery DeleteComment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCommentResponse' smart constructor.
data DeleteCommentResponse = DeleteCommentResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCommentResponse' with the minimum fields required to make a request.
mkDeleteCommentResponse ::
  DeleteCommentResponse
mkDeleteCommentResponse = DeleteCommentResponse'
