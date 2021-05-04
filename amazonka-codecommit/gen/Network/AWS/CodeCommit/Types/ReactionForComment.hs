{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReactionForComment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReactionForComment where

import Network.AWS.CodeCommit.Types.ReactionValueFormats
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the reaction values provided by users on a comment.
--
-- /See:/ 'newReactionForComment' smart constructor.
data ReactionForComment = ReactionForComment'
  { -- | The reaction for a specified comment.
    reaction :: Prelude.Maybe ReactionValueFormats,
    -- | The Amazon Resource Names (ARNs) of users who have provided reactions to
    -- the comment.
    reactionUsers :: Prelude.Maybe [Prelude.Text],
    -- | A numerical count of users who reacted with the specified emoji whose
    -- identities have been subsequently deleted from IAM. While these IAM
    -- users or roles no longer exist, the reactions might still appear in
    -- total reaction counts.
    reactionsFromDeletedUsersCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReactionForComment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reaction', 'reactionForComment_reaction' - The reaction for a specified comment.
--
-- 'reactionUsers', 'reactionForComment_reactionUsers' - The Amazon Resource Names (ARNs) of users who have provided reactions to
-- the comment.
--
-- 'reactionsFromDeletedUsersCount', 'reactionForComment_reactionsFromDeletedUsersCount' - A numerical count of users who reacted with the specified emoji whose
-- identities have been subsequently deleted from IAM. While these IAM
-- users or roles no longer exist, the reactions might still appear in
-- total reaction counts.
newReactionForComment ::
  ReactionForComment
newReactionForComment =
  ReactionForComment'
    { reaction = Prelude.Nothing,
      reactionUsers = Prelude.Nothing,
      reactionsFromDeletedUsersCount = Prelude.Nothing
    }

-- | The reaction for a specified comment.
reactionForComment_reaction :: Lens.Lens' ReactionForComment (Prelude.Maybe ReactionValueFormats)
reactionForComment_reaction = Lens.lens (\ReactionForComment' {reaction} -> reaction) (\s@ReactionForComment' {} a -> s {reaction = a} :: ReactionForComment)

-- | The Amazon Resource Names (ARNs) of users who have provided reactions to
-- the comment.
reactionForComment_reactionUsers :: Lens.Lens' ReactionForComment (Prelude.Maybe [Prelude.Text])
reactionForComment_reactionUsers = Lens.lens (\ReactionForComment' {reactionUsers} -> reactionUsers) (\s@ReactionForComment' {} a -> s {reactionUsers = a} :: ReactionForComment) Prelude.. Lens.mapping Prelude._Coerce

-- | A numerical count of users who reacted with the specified emoji whose
-- identities have been subsequently deleted from IAM. While these IAM
-- users or roles no longer exist, the reactions might still appear in
-- total reaction counts.
reactionForComment_reactionsFromDeletedUsersCount :: Lens.Lens' ReactionForComment (Prelude.Maybe Prelude.Int)
reactionForComment_reactionsFromDeletedUsersCount = Lens.lens (\ReactionForComment' {reactionsFromDeletedUsersCount} -> reactionsFromDeletedUsersCount) (\s@ReactionForComment' {} a -> s {reactionsFromDeletedUsersCount = a} :: ReactionForComment)

instance Prelude.FromJSON ReactionForComment where
  parseJSON =
    Prelude.withObject
      "ReactionForComment"
      ( \x ->
          ReactionForComment'
            Prelude.<$> (x Prelude..:? "reaction")
            Prelude.<*> ( x Prelude..:? "reactionUsers"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "reactionsFromDeletedUsersCount")
      )

instance Prelude.Hashable ReactionForComment

instance Prelude.NFData ReactionForComment
