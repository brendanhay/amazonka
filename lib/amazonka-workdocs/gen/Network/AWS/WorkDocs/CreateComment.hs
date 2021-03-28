{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateComment (..)
    , mkCreateComment
    -- ** Request lenses
    , ccDocumentId
    , ccVersionId
    , ccText
    , ccAuthenticationToken
    , ccNotifyCollaborators
    , ccParentId
    , ccThreadId
    , ccVisibility

    -- * Destructuring the response
    , CreateCommentResponse (..)
    , mkCreateCommentResponse
    -- ** Response lenses
    , ccrrsComment
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateComment' smart constructor.
data CreateComment = CreateComment'
  { documentId :: Types.ResourceIdType
    -- ^ The ID of the document.
  , versionId :: Types.DocumentVersionIdType
    -- ^ The ID of the document version.
  , text :: Types.CommentTextType
    -- ^ The text of the comment.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , notifyCollaborators :: Core.Maybe Core.Bool
    -- ^ Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
  , parentId :: Core.Maybe Types.CommentIdType
    -- ^ The ID of the parent comment.
  , threadId :: Core.Maybe Types.CommentIdType
    -- ^ The ID of the root comment in the thread.
  , visibility :: Core.Maybe Types.CommentVisibilityType
    -- ^ The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateComment' value with any optional fields omitted.
mkCreateComment
    :: Types.ResourceIdType -- ^ 'documentId'
    -> Types.DocumentVersionIdType -- ^ 'versionId'
    -> Types.CommentTextType -- ^ 'text'
    -> CreateComment
mkCreateComment documentId versionId text
  = CreateComment'{documentId, versionId, text,
                   authenticationToken = Core.Nothing,
                   notifyCollaborators = Core.Nothing, parentId = Core.Nothing,
                   threadId = Core.Nothing, visibility = Core.Nothing}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDocumentId :: Lens.Lens' CreateComment Types.ResourceIdType
ccDocumentId = Lens.field @"documentId"
{-# INLINEABLE ccDocumentId #-}
{-# DEPRECATED documentId "Use generic-lens or generic-optics with 'documentId' instead"  #-}

-- | The ID of the document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVersionId :: Lens.Lens' CreateComment Types.DocumentVersionIdType
ccVersionId = Lens.field @"versionId"
{-# INLINEABLE ccVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The text of the comment.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccText :: Lens.Lens' CreateComment Types.CommentTextType
ccText = Lens.field @"text"
{-# INLINEABLE ccText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthenticationToken :: Lens.Lens' CreateComment (Core.Maybe Types.AuthenticationHeaderType)
ccAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE ccAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
--
-- /Note:/ Consider using 'notifyCollaborators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotifyCollaborators :: Lens.Lens' CreateComment (Core.Maybe Core.Bool)
ccNotifyCollaborators = Lens.field @"notifyCollaborators"
{-# INLINEABLE ccNotifyCollaborators #-}
{-# DEPRECATED notifyCollaborators "Use generic-lens or generic-optics with 'notifyCollaborators' instead"  #-}

-- | The ID of the parent comment.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccParentId :: Lens.Lens' CreateComment (Core.Maybe Types.CommentIdType)
ccParentId = Lens.field @"parentId"
{-# INLINEABLE ccParentId #-}
{-# DEPRECATED parentId "Use generic-lens or generic-optics with 'parentId' instead"  #-}

-- | The ID of the root comment in the thread.
--
-- /Note:/ Consider using 'threadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccThreadId :: Lens.Lens' CreateComment (Core.Maybe Types.CommentIdType)
ccThreadId = Lens.field @"threadId"
{-# INLINEABLE ccThreadId #-}
{-# DEPRECATED threadId "Use generic-lens or generic-optics with 'threadId' instead"  #-}

-- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVisibility :: Lens.Lens' CreateComment (Core.Maybe Types.CommentVisibilityType)
ccVisibility = Lens.field @"visibility"
{-# INLINEABLE ccVisibility #-}
{-# DEPRECATED visibility "Use generic-lens or generic-optics with 'visibility' instead"  #-}

instance Core.ToQuery CreateComment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateComment where
        toHeaders CreateComment{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateComment where
        toJSON CreateComment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Text" Core..= text),
                  ("NotifyCollaborators" Core..=) Core.<$> notifyCollaborators,
                  ("ParentId" Core..=) Core.<$> parentId,
                  ("ThreadId" Core..=) Core.<$> threadId,
                  ("Visibility" Core..=) Core.<$> visibility])

instance Core.AWSRequest CreateComment where
        type Rs CreateComment = CreateCommentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/api/v1/documents/" Core.<> Core.toText documentId Core.<>
                             "/versions/"
                             Core.<> Core.toText versionId
                             Core.<> "/comment",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCommentResponse' Core.<$>
                   (x Core..:? "Comment") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCommentResponse' smart constructor.
data CreateCommentResponse = CreateCommentResponse'
  { comment :: Core.Maybe Types.Comment
    -- ^ The comment that has been created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateCommentResponse' value with any optional fields omitted.
mkCreateCommentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCommentResponse
mkCreateCommentResponse responseStatus
  = CreateCommentResponse'{comment = Core.Nothing, responseStatus}

-- | The comment that has been created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsComment :: Lens.Lens' CreateCommentResponse (Core.Maybe Types.Comment)
ccrrsComment = Lens.field @"comment"
{-# INLINEABLE ccrrsComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCommentResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
