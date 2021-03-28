{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the content of a comment made on a change, file, or commit in a repository. 
module Network.AWS.CodeCommit.GetComment
    (
    -- * Creating a request
      GetComment (..)
    , mkGetComment
    -- ** Request lenses
    , gcCommentId

    -- * Destructuring the response
    , GetCommentResponse (..)
    , mkGetCommentResponse
    -- ** Response lenses
    , grsComment
    , grsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetComment' smart constructor.
newtype GetComment = GetComment'
  { commentId :: Types.CommentId
    -- ^ The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetComment' value with any optional fields omitted.
mkGetComment
    :: Types.CommentId -- ^ 'commentId'
    -> GetComment
mkGetComment commentId = GetComment'{commentId}

-- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCommentId :: Lens.Lens' GetComment Types.CommentId
gcCommentId = Lens.field @"commentId"
{-# INLINEABLE gcCommentId #-}
{-# DEPRECATED commentId "Use generic-lens or generic-optics with 'commentId' instead"  #-}

instance Core.ToQuery GetComment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetComment where
        toHeaders GetComment{..}
          = Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetComment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetComment where
        toJSON GetComment{..}
          = Core.object
              (Core.catMaybes [Core.Just ("commentId" Core..= commentId)])

instance Core.AWSRequest GetComment where
        type Rs GetComment = GetCommentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCommentResponse' Core.<$>
                   (x Core..:? "comment") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCommentResponse' smart constructor.
data GetCommentResponse = GetCommentResponse'
  { comment :: Core.Maybe Types.Comment
    -- ^ The contents of the comment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCommentResponse' value with any optional fields omitted.
mkGetCommentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCommentResponse
mkGetCommentResponse responseStatus
  = GetCommentResponse'{comment = Core.Nothing, responseStatus}

-- | The contents of the comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsComment :: Lens.Lens' GetCommentResponse (Core.Maybe Types.Comment)
grsComment = Lens.field @"comment"
{-# INLINEABLE grsComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCommentResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
