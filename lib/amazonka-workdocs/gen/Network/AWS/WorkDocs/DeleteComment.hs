{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteComment (..)
    , mkDeleteComment
    -- ** Request lenses
    , dcfDocumentId
    , dcfVersionId
    , dcfCommentId
    , dcfAuthenticationToken

    -- * Destructuring the response
    , DeleteCommentResponse (..)
    , mkDeleteCommentResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteComment' smart constructor.
data DeleteComment = DeleteComment'
  { documentId :: Types.DocumentId
    -- ^ The ID of the document.
  , versionId :: Types.VersionId
    -- ^ The ID of the document version.
  , commentId :: Types.CommentId
    -- ^ The ID of the comment.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteComment' value with any optional fields omitted.
mkDeleteComment
    :: Types.DocumentId -- ^ 'documentId'
    -> Types.VersionId -- ^ 'versionId'
    -> Types.CommentId -- ^ 'commentId'
    -> DeleteComment
mkDeleteComment documentId versionId commentId
  = DeleteComment'{documentId, versionId, commentId,
                   authenticationToken = Core.Nothing}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfDocumentId :: Lens.Lens' DeleteComment Types.DocumentId
dcfDocumentId = Lens.field @"documentId"
{-# INLINEABLE dcfDocumentId #-}
{-# DEPRECATED documentId "Use generic-lens or generic-optics with 'documentId' instead"  #-}

-- | The ID of the document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfVersionId :: Lens.Lens' DeleteComment Types.VersionId
dcfVersionId = Lens.field @"versionId"
{-# INLINEABLE dcfVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The ID of the comment.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfCommentId :: Lens.Lens' DeleteComment Types.CommentId
dcfCommentId = Lens.field @"commentId"
{-# INLINEABLE dcfCommentId #-}
{-# DEPRECATED commentId "Use generic-lens or generic-optics with 'commentId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfAuthenticationToken :: Lens.Lens' DeleteComment (Core.Maybe Types.AuthenticationHeaderType)
dcfAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE dcfAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

instance Core.ToQuery DeleteComment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteComment where
        toHeaders DeleteComment{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteComment where
        type Rs DeleteComment = DeleteCommentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/api/v1/documents/" Core.<> Core.toText documentId Core.<>
                             "/versions/"
                             Core.<> Core.toText versionId
                             Core.<> "/comment/"
                             Core.<> Core.toText commentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteCommentResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCommentResponse' smart constructor.
data DeleteCommentResponse = DeleteCommentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCommentResponse' value with any optional fields omitted.
mkDeleteCommentResponse
    :: DeleteCommentResponse
mkDeleteCommentResponse = DeleteCommentResponse'
