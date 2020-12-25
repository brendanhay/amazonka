{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cCommentId,
    cContributor,
    cCreatedTimestamp,
    cParentId,
    cRecipientId,
    cStatus,
    cText,
    cThreadId,
    cVisibility,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.CommentIdType as Types
import qualified Network.AWS.WorkDocs.Types.CommentStatusType as Types
import qualified Network.AWS.WorkDocs.Types.CommentVisibilityType as Types
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.Text as Types
import qualified Network.AWS.WorkDocs.Types.User as Types

-- | Describes a comment.
--
-- /See:/ 'mkComment' smart constructor.
data Comment = Comment'
  { -- | The ID of the comment.
    commentId :: Types.CommentIdType,
    -- | The details of the user who made the comment.
    contributor :: Core.Maybe Types.User,
    -- | The time that the comment was created.
    createdTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the parent comment.
    parentId :: Core.Maybe Types.CommentIdType,
    -- | If the comment is a reply to another user's comment, this field contains the user ID of the user being replied to.
    recipientId :: Core.Maybe Types.IdType,
    -- | The status of the comment.
    status :: Core.Maybe Types.CommentStatusType,
    -- | The text of the comment.
    text :: Core.Maybe Types.Text,
    -- | The ID of the root comment in the thread.
    threadId :: Core.Maybe Types.CommentIdType,
    -- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
    visibility :: Core.Maybe Types.CommentVisibilityType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Comment' value with any optional fields omitted.
mkComment ::
  -- | 'commentId'
  Types.CommentIdType ->
  Comment
mkComment commentId =
  Comment'
    { commentId,
      contributor = Core.Nothing,
      createdTimestamp = Core.Nothing,
      parentId = Core.Nothing,
      recipientId = Core.Nothing,
      status = Core.Nothing,
      text = Core.Nothing,
      threadId = Core.Nothing,
      visibility = Core.Nothing
    }

-- | The ID of the comment.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommentId :: Lens.Lens' Comment Types.CommentIdType
cCommentId = Lens.field @"commentId"
{-# DEPRECATED cCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | The details of the user who made the comment.
--
-- /Note:/ Consider using 'contributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContributor :: Lens.Lens' Comment (Core.Maybe Types.User)
cContributor = Lens.field @"contributor"
{-# DEPRECATED cContributor "Use generic-lens or generic-optics with 'contributor' instead." #-}

-- | The time that the comment was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreatedTimestamp :: Lens.Lens' Comment (Core.Maybe Core.NominalDiffTime)
cCreatedTimestamp = Lens.field @"createdTimestamp"
{-# DEPRECATED cCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The ID of the parent comment.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cParentId :: Lens.Lens' Comment (Core.Maybe Types.CommentIdType)
cParentId = Lens.field @"parentId"
{-# DEPRECATED cParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | If the comment is a reply to another user's comment, this field contains the user ID of the user being replied to.
--
-- /Note:/ Consider using 'recipientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRecipientId :: Lens.Lens' Comment (Core.Maybe Types.IdType)
cRecipientId = Lens.field @"recipientId"
{-# DEPRECATED cRecipientId "Use generic-lens or generic-optics with 'recipientId' instead." #-}

-- | The status of the comment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Comment (Core.Maybe Types.CommentStatusType)
cStatus = Lens.field @"status"
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The text of the comment.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cText :: Lens.Lens' Comment (Core.Maybe Types.Text)
cText = Lens.field @"text"
{-# DEPRECATED cText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The ID of the root comment in the thread.
--
-- /Note:/ Consider using 'threadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cThreadId :: Lens.Lens' Comment (Core.Maybe Types.CommentIdType)
cThreadId = Lens.field @"threadId"
{-# DEPRECATED cThreadId "Use generic-lens or generic-optics with 'threadId' instead." #-}

-- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVisibility :: Lens.Lens' Comment (Core.Maybe Types.CommentVisibilityType)
cVisibility = Lens.field @"visibility"
{-# DEPRECATED cVisibility "Use generic-lens or generic-optics with 'visibility' instead." #-}

instance Core.FromJSON Comment where
  parseJSON =
    Core.withObject "Comment" Core.$
      \x ->
        Comment'
          Core.<$> (x Core..: "CommentId")
          Core.<*> (x Core..:? "Contributor")
          Core.<*> (x Core..:? "CreatedTimestamp")
          Core.<*> (x Core..:? "ParentId")
          Core.<*> (x Core..:? "RecipientId")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Text")
          Core.<*> (x Core..:? "ThreadId")
          Core.<*> (x Core..:? "Visibility")
