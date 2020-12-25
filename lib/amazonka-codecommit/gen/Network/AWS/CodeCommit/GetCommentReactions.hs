{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetCommentReactions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reactions to a specified comment ID. Reactions from users who have been deleted will not be included in the count.
module Network.AWS.CodeCommit.GetCommentReactions
  ( -- * Creating a request
    GetCommentReactions (..),
    mkGetCommentReactions,

    -- ** Request lenses
    gcrCommentId,
    gcrMaxResults,
    gcrNextToken,
    gcrReactionUserArn,

    -- * Destructuring the response
    GetCommentReactionsResponse (..),
    mkGetCommentReactionsResponse,

    -- ** Response lenses
    gcrrrsReactionsForComment,
    gcrrrsNextToken,
    gcrrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCommentReactions' smart constructor.
data GetCommentReactions = GetCommentReactions'
  { -- | The ID of the comment for which you want to get reactions information.
    commentId :: Types.CommentId,
    -- | A non-zero, non-negative integer used to limit the number of returned results. The default is the same as the allowed maximum, 1,000.
    maxResults :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Optional. The Amazon Resource Name (ARN) of the user or identity for which you want to get reaction information.
    reactionUserArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommentReactions' value with any optional fields omitted.
mkGetCommentReactions ::
  -- | 'commentId'
  Types.CommentId ->
  GetCommentReactions
mkGetCommentReactions commentId =
  GetCommentReactions'
    { commentId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      reactionUserArn = Core.Nothing
    }

-- | The ID of the comment for which you want to get reactions information.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrCommentId :: Lens.Lens' GetCommentReactions Types.CommentId
gcrCommentId = Lens.field @"commentId"
{-# DEPRECATED gcrCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is the same as the allowed maximum, 1,000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrMaxResults :: Lens.Lens' GetCommentReactions (Core.Maybe Core.Int)
gcrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrNextToken :: Lens.Lens' GetCommentReactions (Core.Maybe Types.NextToken)
gcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. The Amazon Resource Name (ARN) of the user or identity for which you want to get reaction information.
--
-- /Note:/ Consider using 'reactionUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrReactionUserArn :: Lens.Lens' GetCommentReactions (Core.Maybe Types.Arn)
gcrReactionUserArn = Lens.field @"reactionUserArn"
{-# DEPRECATED gcrReactionUserArn "Use generic-lens or generic-optics with 'reactionUserArn' instead." #-}

instance Core.FromJSON GetCommentReactions where
  toJSON GetCommentReactions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("commentId" Core..= commentId),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("reactionUserArn" Core..=) Core.<$> reactionUserArn
          ]
      )

instance Core.AWSRequest GetCommentReactions where
  type Rs GetCommentReactions = GetCommentReactionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.GetCommentReactions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCommentReactionsResponse'
            Core.<$> (x Core..:? "reactionsForComment" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCommentReactionsResponse' smart constructor.
data GetCommentReactionsResponse = GetCommentReactionsResponse'
  { -- | An array of reactions to the specified comment.
    reactionsForComment :: [Types.ReactionForComment],
    -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommentReactionsResponse' value with any optional fields omitted.
mkGetCommentReactionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCommentReactionsResponse
mkGetCommentReactionsResponse responseStatus =
  GetCommentReactionsResponse'
    { reactionsForComment = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of reactions to the specified comment.
--
-- /Note:/ Consider using 'reactionsForComment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsReactionsForComment :: Lens.Lens' GetCommentReactionsResponse [Types.ReactionForComment]
gcrrrsReactionsForComment = Lens.field @"reactionsForComment"
{-# DEPRECATED gcrrrsReactionsForComment "Use generic-lens or generic-optics with 'reactionsForComment' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsNextToken :: Lens.Lens' GetCommentReactionsResponse (Core.Maybe Types.NextToken)
gcrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrrsResponseStatus :: Lens.Lens' GetCommentReactionsResponse Core.Int
gcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
