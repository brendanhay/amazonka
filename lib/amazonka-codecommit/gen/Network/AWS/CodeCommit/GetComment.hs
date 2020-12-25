{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetComment (..),
    mkGetComment,

    -- ** Request lenses
    gcCommentId,

    -- * Destructuring the response
    GetCommentResponse (..),
    mkGetCommentResponse,

    -- ** Response lenses
    grsComment,
    grsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetComment' smart constructor.
newtype GetComment = GetComment'
  { -- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
    commentId :: Types.CommentId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetComment' value with any optional fields omitted.
mkGetComment ::
  -- | 'commentId'
  Types.CommentId ->
  GetComment
mkGetComment commentId = GetComment' {commentId}

-- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCommentId :: Lens.Lens' GetComment Types.CommentId
gcCommentId = Lens.field @"commentId"
{-# DEPRECATED gcCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

instance Core.FromJSON GetComment where
  toJSON GetComment {..} =
    Core.object
      (Core.catMaybes [Core.Just ("commentId" Core..= commentId)])

instance Core.AWSRequest GetComment where
  type Rs GetComment = GetCommentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.GetComment")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentResponse'
            Core.<$> (x Core..:? "comment") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCommentResponse' smart constructor.
data GetCommentResponse = GetCommentResponse'
  { -- | The contents of the comment.
    comment :: Core.Maybe Types.Comment,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetCommentResponse' value with any optional fields omitted.
mkGetCommentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCommentResponse
mkGetCommentResponse responseStatus =
  GetCommentResponse' {comment = Core.Nothing, responseStatus}

-- | The contents of the comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsComment :: Lens.Lens' GetCommentResponse (Core.Maybe Types.Comment)
grsComment = Lens.field @"comment"
{-# DEPRECATED grsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCommentResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
