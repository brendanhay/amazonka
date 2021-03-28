{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteCommentContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content of a comment made on a change, file, or commit in a repository.
module Network.AWS.CodeCommit.DeleteCommentContent
    (
    -- * Creating a request
      DeleteCommentContent (..)
    , mkDeleteCommentContent
    -- ** Request lenses
    , dccCommentId

    -- * Destructuring the response
    , DeleteCommentContentResponse (..)
    , mkDeleteCommentContentResponse
    -- ** Response lenses
    , dccrrsComment
    , dccrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCommentContent' smart constructor.
newtype DeleteCommentContent = DeleteCommentContent'
  { commentId :: Types.CommentId
    -- ^ The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCommentContent' value with any optional fields omitted.
mkDeleteCommentContent
    :: Types.CommentId -- ^ 'commentId'
    -> DeleteCommentContent
mkDeleteCommentContent commentId = DeleteCommentContent'{commentId}

-- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCommentId :: Lens.Lens' DeleteCommentContent Types.CommentId
dccCommentId = Lens.field @"commentId"
{-# INLINEABLE dccCommentId #-}
{-# DEPRECATED commentId "Use generic-lens or generic-optics with 'commentId' instead"  #-}

instance Core.ToQuery DeleteCommentContent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCommentContent where
        toHeaders DeleteCommentContent{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.DeleteCommentContent")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCommentContent where
        toJSON DeleteCommentContent{..}
          = Core.object
              (Core.catMaybes [Core.Just ("commentId" Core..= commentId)])

instance Core.AWSRequest DeleteCommentContent where
        type Rs DeleteCommentContent = DeleteCommentContentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteCommentContentResponse' Core.<$>
                   (x Core..:? "comment") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCommentContentResponse' smart constructor.
data DeleteCommentContentResponse = DeleteCommentContentResponse'
  { comment :: Core.Maybe Types.Comment
    -- ^ Information about the comment you just deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteCommentContentResponse' value with any optional fields omitted.
mkDeleteCommentContentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCommentContentResponse
mkDeleteCommentContentResponse responseStatus
  = DeleteCommentContentResponse'{comment = Core.Nothing,
                                  responseStatus}

-- | Information about the comment you just deleted.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsComment :: Lens.Lens' DeleteCommentContentResponse (Core.Maybe Types.Comment)
dccrrsComment = Lens.field @"comment"
{-# INLINEABLE dccrrsComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsResponseStatus :: Lens.Lens' DeleteCommentContentResponse Core.Int
dccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
