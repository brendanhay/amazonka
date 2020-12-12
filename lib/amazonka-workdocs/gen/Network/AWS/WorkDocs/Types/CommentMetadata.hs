{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.CommentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.CommentMetadata
  ( CommentMetadata (..),

    -- * Smart constructor
    mkCommentMetadata,

    -- * Lenses
    cmCommentStatus,
    cmContributor,
    cmCommentId,
    cmCreatedTimestamp,
    cmRecipientId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.CommentStatusType
import Network.AWS.WorkDocs.Types.User

-- | Describes the metadata of a comment.
--
-- /See:/ 'mkCommentMetadata' smart constructor.
data CommentMetadata = CommentMetadata'
  { commentStatus ::
      Lude.Maybe CommentStatusType,
    contributor :: Lude.Maybe User,
    commentId :: Lude.Maybe Lude.Text,
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    recipientId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CommentMetadata' with the minimum fields required to make a request.
--
-- * 'commentId' - The ID of the comment.
-- * 'commentStatus' - The status of the comment.
-- * 'contributor' - The user who made the comment.
-- * 'createdTimestamp' - The timestamp that the comment was created.
-- * 'recipientId' - The ID of the user being replied to.
mkCommentMetadata ::
  CommentMetadata
mkCommentMetadata =
  CommentMetadata'
    { commentStatus = Lude.Nothing,
      contributor = Lude.Nothing,
      commentId = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      recipientId = Lude.Nothing
    }

-- | The status of the comment.
--
-- /Note:/ Consider using 'commentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCommentStatus :: Lens.Lens' CommentMetadata (Lude.Maybe CommentStatusType)
cmCommentStatus = Lens.lens (commentStatus :: CommentMetadata -> Lude.Maybe CommentStatusType) (\s a -> s {commentStatus = a} :: CommentMetadata)
{-# DEPRECATED cmCommentStatus "Use generic-lens or generic-optics with 'commentStatus' instead." #-}

-- | The user who made the comment.
--
-- /Note:/ Consider using 'contributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContributor :: Lens.Lens' CommentMetadata (Lude.Maybe User)
cmContributor = Lens.lens (contributor :: CommentMetadata -> Lude.Maybe User) (\s a -> s {contributor = a} :: CommentMetadata)
{-# DEPRECATED cmContributor "Use generic-lens or generic-optics with 'contributor' instead." #-}

-- | The ID of the comment.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCommentId :: Lens.Lens' CommentMetadata (Lude.Maybe Lude.Text)
cmCommentId = Lens.lens (commentId :: CommentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {commentId = a} :: CommentMetadata)
{-# DEPRECATED cmCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | The timestamp that the comment was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCreatedTimestamp :: Lens.Lens' CommentMetadata (Lude.Maybe Lude.Timestamp)
cmCreatedTimestamp = Lens.lens (createdTimestamp :: CommentMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: CommentMetadata)
{-# DEPRECATED cmCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The ID of the user being replied to.
--
-- /Note:/ Consider using 'recipientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRecipientId :: Lens.Lens' CommentMetadata (Lude.Maybe Lude.Text)
cmRecipientId = Lens.lens (recipientId :: CommentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {recipientId = a} :: CommentMetadata)
{-# DEPRECATED cmRecipientId "Use generic-lens or generic-optics with 'recipientId' instead." #-}

instance Lude.FromJSON CommentMetadata where
  parseJSON =
    Lude.withObject
      "CommentMetadata"
      ( \x ->
          CommentMetadata'
            Lude.<$> (x Lude..:? "CommentStatus")
            Lude.<*> (x Lude..:? "Contributor")
            Lude.<*> (x Lude..:? "CommentId")
            Lude.<*> (x Lude..:? "CreatedTimestamp")
            Lude.<*> (x Lude..:? "RecipientId")
      )
