{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of a comment.
module Network.AWS.CodeCommit.UpdateComment
  ( -- * Creating a request
    UpdateComment (..),
    mkUpdateComment,

    -- ** Request lenses
    ucCommentId,
    ucContent,

    -- * Destructuring the response
    UpdateCommentResponse (..),
    mkUpdateCommentResponse,

    -- ** Response lenses
    ucrrsComment,
    ucrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateComment' smart constructor.
data UpdateComment = UpdateComment'
  { -- | The system-generated ID of the comment you want to update. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
    commentId :: Types.CommentId,
    -- | The updated content to replace the existing content of the comment.
    content :: Types.Content
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateComment' value with any optional fields omitted.
mkUpdateComment ::
  -- | 'commentId'
  Types.CommentId ->
  -- | 'content'
  Types.Content ->
  UpdateComment
mkUpdateComment commentId content =
  UpdateComment' {commentId, content}

-- | The system-generated ID of the comment you want to update. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCommentId :: Lens.Lens' UpdateComment Types.CommentId
ucCommentId = Lens.field @"commentId"
{-# DEPRECATED ucCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | The updated content to replace the existing content of the comment.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucContent :: Lens.Lens' UpdateComment Types.Content
ucContent = Lens.field @"content"
{-# DEPRECATED ucContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Core.FromJSON UpdateComment where
  toJSON UpdateComment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("commentId" Core..= commentId),
            Core.Just ("content" Core..= content)
          ]
      )

instance Core.AWSRequest UpdateComment where
  type Rs UpdateComment = UpdateCommentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.UpdateComment")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCommentResponse'
            Core.<$> (x Core..:? "comment") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateCommentResponse' smart constructor.
data UpdateCommentResponse = UpdateCommentResponse'
  { -- | Information about the updated comment.
    comment :: Core.Maybe Types.Comment,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateCommentResponse' value with any optional fields omitted.
mkUpdateCommentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateCommentResponse
mkUpdateCommentResponse responseStatus =
  UpdateCommentResponse' {comment = Core.Nothing, responseStatus}

-- | Information about the updated comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsComment :: Lens.Lens' UpdateCommentResponse (Core.Maybe Types.Comment)
ucrrsComment = Lens.field @"comment"
{-# DEPRECATED ucrrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateCommentResponse Core.Int
ucrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
