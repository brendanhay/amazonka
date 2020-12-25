{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PutCommentReaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a reaction to a specified comment for the user whose identity is used to make the request. You can only add or update a reaction for yourself. You cannot add, modify, or delete a reaction for another user.
module Network.AWS.CodeCommit.PutCommentReaction
  ( -- * Creating a request
    PutCommentReaction (..),
    mkPutCommentReaction,

    -- ** Request lenses
    pcrCommentId,
    pcrReactionValue,

    -- * Destructuring the response
    PutCommentReactionResponse (..),
    mkPutCommentReactionResponse,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutCommentReaction' smart constructor.
data PutCommentReaction = PutCommentReaction'
  { -- | The ID of the comment to which you want to add or update a reaction.
    commentId :: Types.CommentId,
    -- | The emoji reaction you want to add or update. To remove a reaction, provide a value of blank or null. You can also provide the value of none. For information about emoji reaction values supported in AWS CodeCommit, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide> .
    reactionValue :: Types.ReactionValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutCommentReaction' value with any optional fields omitted.
mkPutCommentReaction ::
  -- | 'commentId'
  Types.CommentId ->
  -- | 'reactionValue'
  Types.ReactionValue ->
  PutCommentReaction
mkPutCommentReaction commentId reactionValue =
  PutCommentReaction' {commentId, reactionValue}

-- | The ID of the comment to which you want to add or update a reaction.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrCommentId :: Lens.Lens' PutCommentReaction Types.CommentId
pcrCommentId = Lens.field @"commentId"
{-# DEPRECATED pcrCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | The emoji reaction you want to add or update. To remove a reaction, provide a value of blank or null. You can also provide the value of none. For information about emoji reaction values supported in AWS CodeCommit, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide> .
--
-- /Note:/ Consider using 'reactionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrReactionValue :: Lens.Lens' PutCommentReaction Types.ReactionValue
pcrReactionValue = Lens.field @"reactionValue"
{-# DEPRECATED pcrReactionValue "Use generic-lens or generic-optics with 'reactionValue' instead." #-}

instance Core.FromJSON PutCommentReaction where
  toJSON PutCommentReaction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("commentId" Core..= commentId),
            Core.Just ("reactionValue" Core..= reactionValue)
          ]
      )

instance Core.AWSRequest PutCommentReaction where
  type Rs PutCommentReaction = PutCommentReactionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.PutCommentReaction")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull PutCommentReactionResponse'

-- | /See:/ 'mkPutCommentReactionResponse' smart constructor.
data PutCommentReactionResponse = PutCommentReactionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutCommentReactionResponse' value with any optional fields omitted.
mkPutCommentReactionResponse ::
  PutCommentReactionResponse
mkPutCommentReactionResponse = PutCommentReactionResponse'
