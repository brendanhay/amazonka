-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Comment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Comment
  ( Comment (..),

    -- * Smart constructor
    mkComment,

    -- * Lenses
    cStatus,
    cText,
    cVisibility,
    cThreadId,
    cContributor,
    cCreatedTimestamp,
    cRecipientId,
    cParentId,
    cCommentId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.CommentStatusType
import Network.AWS.WorkDocs.Types.CommentVisibilityType
import Network.AWS.WorkDocs.Types.User

-- | Describes a comment.
--
-- /See:/ 'mkComment' smart constructor.
data Comment = Comment'
  { status :: Lude.Maybe CommentStatusType,
    text :: Lude.Maybe (Lude.Sensitive Lude.Text),
    visibility :: Lude.Maybe CommentVisibilityType,
    threadId :: Lude.Maybe Lude.Text,
    contributor :: Lude.Maybe User,
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    recipientId :: Lude.Maybe Lude.Text,
    parentId :: Lude.Maybe Lude.Text,
    commentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Comment' with the minimum fields required to make a request.
--
-- * 'commentId' - The ID of the comment.
-- * 'contributor' - The details of the user who made the comment.
-- * 'createdTimestamp' - The time that the comment was created.
-- * 'parentId' - The ID of the parent comment.
-- * 'recipientId' - If the comment is a reply to another user's comment, this field contains the user ID of the user being replied to.
-- * 'status' - The status of the comment.
-- * 'text' - The text of the comment.
-- * 'threadId' - The ID of the root comment in the thread.
-- * 'visibility' - The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
mkComment ::
  -- | 'commentId'
  Lude.Text ->
  Comment
mkComment pCommentId_ =
  Comment'
    { status = Lude.Nothing,
      text = Lude.Nothing,
      visibility = Lude.Nothing,
      threadId = Lude.Nothing,
      contributor = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      recipientId = Lude.Nothing,
      parentId = Lude.Nothing,
      commentId = pCommentId_
    }

-- | The status of the comment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Comment (Lude.Maybe CommentStatusType)
cStatus = Lens.lens (status :: Comment -> Lude.Maybe CommentStatusType) (\s a -> s {status = a} :: Comment)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The text of the comment.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cText :: Lens.Lens' Comment (Lude.Maybe (Lude.Sensitive Lude.Text))
cText = Lens.lens (text :: Comment -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {text = a} :: Comment)
{-# DEPRECATED cText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVisibility :: Lens.Lens' Comment (Lude.Maybe CommentVisibilityType)
cVisibility = Lens.lens (visibility :: Comment -> Lude.Maybe CommentVisibilityType) (\s a -> s {visibility = a} :: Comment)
{-# DEPRECATED cVisibility "Use generic-lens or generic-optics with 'visibility' instead." #-}

-- | The ID of the root comment in the thread.
--
-- /Note:/ Consider using 'threadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cThreadId :: Lens.Lens' Comment (Lude.Maybe Lude.Text)
cThreadId = Lens.lens (threadId :: Comment -> Lude.Maybe Lude.Text) (\s a -> s {threadId = a} :: Comment)
{-# DEPRECATED cThreadId "Use generic-lens or generic-optics with 'threadId' instead." #-}

-- | The details of the user who made the comment.
--
-- /Note:/ Consider using 'contributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContributor :: Lens.Lens' Comment (Lude.Maybe User)
cContributor = Lens.lens (contributor :: Comment -> Lude.Maybe User) (\s a -> s {contributor = a} :: Comment)
{-# DEPRECATED cContributor "Use generic-lens or generic-optics with 'contributor' instead." #-}

-- | The time that the comment was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreatedTimestamp :: Lens.Lens' Comment (Lude.Maybe Lude.Timestamp)
cCreatedTimestamp = Lens.lens (createdTimestamp :: Comment -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: Comment)
{-# DEPRECATED cCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | If the comment is a reply to another user's comment, this field contains the user ID of the user being replied to.
--
-- /Note:/ Consider using 'recipientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRecipientId :: Lens.Lens' Comment (Lude.Maybe Lude.Text)
cRecipientId = Lens.lens (recipientId :: Comment -> Lude.Maybe Lude.Text) (\s a -> s {recipientId = a} :: Comment)
{-# DEPRECATED cRecipientId "Use generic-lens or generic-optics with 'recipientId' instead." #-}

-- | The ID of the parent comment.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParentId :: Lens.Lens' Comment (Lude.Maybe Lude.Text)
cParentId = Lens.lens (parentId :: Comment -> Lude.Maybe Lude.Text) (\s a -> s {parentId = a} :: Comment)
{-# DEPRECATED cParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | The ID of the comment.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommentId :: Lens.Lens' Comment Lude.Text
cCommentId = Lens.lens (commentId :: Comment -> Lude.Text) (\s a -> s {commentId = a} :: Comment)
{-# DEPRECATED cCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

instance Lude.FromJSON Comment where
  parseJSON =
    Lude.withObject
      "Comment"
      ( \x ->
          Comment'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Text")
            Lude.<*> (x Lude..:? "Visibility")
            Lude.<*> (x Lude..:? "ThreadId")
            Lude.<*> (x Lude..:? "Contributor")
            Lude.<*> (x Lude..:? "CreatedTimestamp")
            Lude.<*> (x Lude..:? "RecipientId")
            Lude.<*> (x Lude..:? "ParentId")
            Lude.<*> (x Lude..: "CommentId")
      )
