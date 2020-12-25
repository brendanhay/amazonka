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
    ccDocumentId,
    ccVersionId,
    ccText,
    ccAuthenticationToken,
    ccNotifyCollaborators,
    ccParentId,
    ccThreadId,
    ccVisibility,

    -- * Destructuring the response
    CreateCommentResponse (..),
    mkCreateCommentResponse,

    -- ** Response lenses
    ccrrsComment,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateComment' smart constructor.
data CreateComment = CreateComment'
  { -- | The ID of the document.
    documentId :: Types.ResourceIdType,
    -- | The ID of the document version.
    versionId :: Types.DocumentVersionIdType,
    -- | The text of the comment.
    text :: Types.CommentTextType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
    notifyCollaborators :: Core.Maybe Core.Bool,
    -- | The ID of the parent comment.
    parentId :: Core.Maybe Types.CommentIdType,
    -- | The ID of the root comment in the thread.
    threadId :: Core.Maybe Types.CommentIdType,
    -- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
    visibility :: Core.Maybe Types.CommentVisibilityType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateComment' value with any optional fields omitted.
mkCreateComment ::
  -- | 'documentId'
  Types.ResourceIdType ->
  -- | 'versionId'
  Types.DocumentVersionIdType ->
  -- | 'text'
  Types.CommentTextType ->
  CreateComment
mkCreateComment documentId versionId text =
  CreateComment'
    { documentId,
      versionId,
      text,
      authenticationToken = Core.Nothing,
      notifyCollaborators = Core.Nothing,
      parentId = Core.Nothing,
      threadId = Core.Nothing,
      visibility = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDocumentId :: Lens.Lens' CreateComment Types.ResourceIdType
ccDocumentId = Lens.field @"documentId"
{-# DEPRECATED ccDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | The ID of the document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVersionId :: Lens.Lens' CreateComment Types.DocumentVersionIdType
ccVersionId = Lens.field @"versionId"
{-# DEPRECATED ccVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The text of the comment.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccText :: Lens.Lens' CreateComment Types.CommentTextType
ccText = Lens.field @"text"
{-# DEPRECATED ccText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthenticationToken :: Lens.Lens' CreateComment (Core.Maybe Types.AuthenticationHeaderType)
ccAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED ccAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
--
-- /Note:/ Consider using 'notifyCollaborators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotifyCollaborators :: Lens.Lens' CreateComment (Core.Maybe Core.Bool)
ccNotifyCollaborators = Lens.field @"notifyCollaborators"
{-# DEPRECATED ccNotifyCollaborators "Use generic-lens or generic-optics with 'notifyCollaborators' instead." #-}

-- | The ID of the parent comment.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccParentId :: Lens.Lens' CreateComment (Core.Maybe Types.CommentIdType)
ccParentId = Lens.field @"parentId"
{-# DEPRECATED ccParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | The ID of the root comment in the thread.
--
-- /Note:/ Consider using 'threadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccThreadId :: Lens.Lens' CreateComment (Core.Maybe Types.CommentIdType)
ccThreadId = Lens.field @"threadId"
{-# DEPRECATED ccThreadId "Use generic-lens or generic-optics with 'threadId' instead." #-}

-- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVisibility :: Lens.Lens' CreateComment (Core.Maybe Types.CommentVisibilityType)
ccVisibility = Lens.field @"visibility"
{-# DEPRECATED ccVisibility "Use generic-lens or generic-optics with 'visibility' instead." #-}

instance Core.FromJSON CreateComment where
  toJSON CreateComment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            ("NotifyCollaborators" Core..=) Core.<$> notifyCollaborators,
            ("ParentId" Core..=) Core.<$> parentId,
            ("ThreadId" Core..=) Core.<$> threadId,
            ("Visibility" Core..=) Core.<$> visibility
          ]
      )

instance Core.AWSRequest CreateComment where
  type Rs CreateComment = CreateCommentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/documents/" Core.<> (Core.toText documentId)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionId)
                Core.<> ("/comment")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCommentResponse'
            Core.<$> (x Core..:? "Comment") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCommentResponse' smart constructor.
data CreateCommentResponse = CreateCommentResponse'
  { -- | The comment that has been created.
    comment :: Core.Maybe Types.Comment,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateCommentResponse' value with any optional fields omitted.
mkCreateCommentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCommentResponse
mkCreateCommentResponse responseStatus =
  CreateCommentResponse' {comment = Core.Nothing, responseStatus}

-- | The comment that has been created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsComment :: Lens.Lens' CreateCommentResponse (Core.Maybe Types.Comment)
ccrrsComment = Lens.field @"comment"
{-# DEPRECATED ccrrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCommentResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
