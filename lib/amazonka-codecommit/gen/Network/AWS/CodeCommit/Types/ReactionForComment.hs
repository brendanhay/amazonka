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
    rfcReactionUsers,
    rfcReactionsFromDeletedUsersCount,
    rfcReaction,
  )
where

import Network.AWS.CodeCommit.Types.ReactionValueFormats
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the reaction values provided by users on a comment.
--
-- /See:/ 'mkReactionForComment' smart constructor.
data ReactionForComment = ReactionForComment'
  { -- | The Amazon Resource Names (ARNs) of users who have provided reactions to the comment.
    reactionUsers :: Lude.Maybe [Lude.Text],
    -- | A numerical count of users who reacted with the specified emoji whose identities have been subsequently deleted from IAM. While these IAM users or roles no longer exist, the reactions might still appear in total reaction counts.
    reactionsFromDeletedUsersCount :: Lude.Maybe Lude.Int,
    -- | The reaction for a specified comment.
    reaction :: Lude.Maybe ReactionValueFormats
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReactionForComment' with the minimum fields required to make a request.
--
-- * 'reactionUsers' - The Amazon Resource Names (ARNs) of users who have provided reactions to the comment.
-- * 'reactionsFromDeletedUsersCount' - A numerical count of users who reacted with the specified emoji whose identities have been subsequently deleted from IAM. While these IAM users or roles no longer exist, the reactions might still appear in total reaction counts.
-- * 'reaction' - The reaction for a specified comment.
mkReactionForComment ::
  ReactionForComment
mkReactionForComment =
  ReactionForComment'
    { reactionUsers = Lude.Nothing,
      reactionsFromDeletedUsersCount = Lude.Nothing,
      reaction = Lude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of users who have provided reactions to the comment.
--
-- /Note:/ Consider using 'reactionUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcReactionUsers :: Lens.Lens' ReactionForComment (Lude.Maybe [Lude.Text])
rfcReactionUsers = Lens.lens (reactionUsers :: ReactionForComment -> Lude.Maybe [Lude.Text]) (\s a -> s {reactionUsers = a} :: ReactionForComment)
{-# DEPRECATED rfcReactionUsers "Use generic-lens or generic-optics with 'reactionUsers' instead." #-}

-- | A numerical count of users who reacted with the specified emoji whose identities have been subsequently deleted from IAM. While these IAM users or roles no longer exist, the reactions might still appear in total reaction counts.
--
-- /Note:/ Consider using 'reactionsFromDeletedUsersCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcReactionsFromDeletedUsersCount :: Lens.Lens' ReactionForComment (Lude.Maybe Lude.Int)
rfcReactionsFromDeletedUsersCount = Lens.lens (reactionsFromDeletedUsersCount :: ReactionForComment -> Lude.Maybe Lude.Int) (\s a -> s {reactionsFromDeletedUsersCount = a} :: ReactionForComment)
{-# DEPRECATED rfcReactionsFromDeletedUsersCount "Use generic-lens or generic-optics with 'reactionsFromDeletedUsersCount' instead." #-}

-- | The reaction for a specified comment.
--
-- /Note:/ Consider using 'reaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfcReaction :: Lens.Lens' ReactionForComment (Lude.Maybe ReactionValueFormats)
rfcReaction = Lens.lens (reaction :: ReactionForComment -> Lude.Maybe ReactionValueFormats) (\s a -> s {reaction = a} :: ReactionForComment)
{-# DEPRECATED rfcReaction "Use generic-lens or generic-optics with 'reaction' instead." #-}

instance Lude.FromJSON ReactionForComment where
  parseJSON =
    Lude.withObject
      "ReactionForComment"
      ( \x ->
          ReactionForComment'
            Lude.<$> (x Lude..:? "reactionUsers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "reactionsFromDeletedUsersCount")
            Lude.<*> (x Lude..:? "reaction")
      )
