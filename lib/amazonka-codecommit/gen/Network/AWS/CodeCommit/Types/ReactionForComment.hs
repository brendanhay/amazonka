{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReactionForComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReactionForComment
  ( ReactionForComment (..),

    -- * Smart constructor
    mkReactionForComment,

    -- * Lenses
    rfcReaction,
    rfcReactionUsers,
    rfcReactionsFromDeletedUsersCount,
  )
where

import qualified Network.AWS.CodeCommit.Types.Arn as Types
import qualified Network.AWS.CodeCommit.Types.ReactionValueFormats as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the reaction values provided by users on a comment.
--
-- /See:/ 'mkReactionForComment' smart constructor.
data ReactionForComment = ReactionForComment'
  { -- | The reaction for a specified comment.
    reaction :: Core.Maybe Types.ReactionValueFormats,
    -- | The Amazon Resource Names (ARNs) of users who have provided reactions to the comment.
    reactionUsers :: Core.Maybe [Types.Arn],
    -- | A numerical count of users who reacted with the specified emoji whose identities have been subsequently deleted from IAM. While these IAM users or roles no longer exist, the reactions might still appear in total reaction counts.
    reactionsFromDeletedUsersCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReactionForComment' value with any optional fields omitted.
mkReactionForComment ::
  ReactionForComment
mkReactionForComment =
  ReactionForComment'
    { reaction = Core.Nothing,
      reactionUsers = Core.Nothing,
      reactionsFromDeletedUsersCount = Core.Nothing
    }

-- | The reaction for a specified comment.
--
-- /Note:/ Consider using 'reaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcReaction :: Lens.Lens' ReactionForComment (Core.Maybe Types.ReactionValueFormats)
rfcReaction = Lens.field @"reaction"
{-# DEPRECATED rfcReaction "Use generic-lens or generic-optics with 'reaction' instead." #-}

-- | The Amazon Resource Names (ARNs) of users who have provided reactions to the comment.
--
-- /Note:/ Consider using 'reactionUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcReactionUsers :: Lens.Lens' ReactionForComment (Core.Maybe [Types.Arn])
rfcReactionUsers = Lens.field @"reactionUsers"
{-# DEPRECATED rfcReactionUsers "Use generic-lens or generic-optics with 'reactionUsers' instead." #-}

-- | A numerical count of users who reacted with the specified emoji whose identities have been subsequently deleted from IAM. While these IAM users or roles no longer exist, the reactions might still appear in total reaction counts.
--
-- /Note:/ Consider using 'reactionsFromDeletedUsersCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcReactionsFromDeletedUsersCount :: Lens.Lens' ReactionForComment (Core.Maybe Core.Int)
rfcReactionsFromDeletedUsersCount = Lens.field @"reactionsFromDeletedUsersCount"
{-# DEPRECATED rfcReactionsFromDeletedUsersCount "Use generic-lens or generic-optics with 'reactionsFromDeletedUsersCount' instead." #-}

instance Core.FromJSON ReactionForComment where
  parseJSON =
    Core.withObject "ReactionForComment" Core.$
      \x ->
        ReactionForComment'
          Core.<$> (x Core..:? "reaction")
          Core.<*> (x Core..:? "reactionUsers")
          Core.<*> (x Core..:? "reactionsFromDeletedUsersCount")
