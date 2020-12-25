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
    cmCommentId,
    cmCommentStatus,
    cmContributor,
    cmCreatedTimestamp,
    cmRecipientId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.CommentIdType as Types
import qualified Network.AWS.WorkDocs.Types.CommentStatusType as Types
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.User as Types

-- | Describes the metadata of a comment.
--
-- /See:/ 'mkCommentMetadata' smart constructor.
data CommentMetadata = CommentMetadata'
  { -- | The ID of the comment.
    commentId :: Core.Maybe Types.CommentIdType,
    -- | The status of the comment.
    commentStatus :: Core.Maybe Types.CommentStatusType,
    -- | The user who made the comment.
    contributor :: Core.Maybe Types.User,
    -- | The timestamp that the comment was created.
    createdTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the user being replied to.
    recipientId :: Core.Maybe Types.IdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CommentMetadata' value with any optional fields omitted.
mkCommentMetadata ::
  CommentMetadata
mkCommentMetadata =
  CommentMetadata'
    { commentId = Core.Nothing,
      commentStatus = Core.Nothing,
      contributor = Core.Nothing,
      createdTimestamp = Core.Nothing,
      recipientId = Core.Nothing
    }

-- | The ID of the comment.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCommentId :: Lens.Lens' CommentMetadata (Core.Maybe Types.CommentIdType)
cmCommentId = Lens.field @"commentId"
{-# DEPRECATED cmCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | The status of the comment.
--
-- /Note:/ Consider using 'commentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCommentStatus :: Lens.Lens' CommentMetadata (Core.Maybe Types.CommentStatusType)
cmCommentStatus = Lens.field @"commentStatus"
{-# DEPRECATED cmCommentStatus "Use generic-lens or generic-optics with 'commentStatus' instead." #-}

-- | The user who made the comment.
--
-- /Note:/ Consider using 'contributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContributor :: Lens.Lens' CommentMetadata (Core.Maybe Types.User)
cmContributor = Lens.field @"contributor"
{-# DEPRECATED cmContributor "Use generic-lens or generic-optics with 'contributor' instead." #-}

-- | The timestamp that the comment was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCreatedTimestamp :: Lens.Lens' CommentMetadata (Core.Maybe Core.NominalDiffTime)
cmCreatedTimestamp = Lens.field @"createdTimestamp"
{-# DEPRECATED cmCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The ID of the user being replied to.
--
-- /Note:/ Consider using 'recipientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRecipientId :: Lens.Lens' CommentMetadata (Core.Maybe Types.IdType)
cmRecipientId = Lens.field @"recipientId"
{-# DEPRECATED cmRecipientId "Use generic-lens or generic-optics with 'recipientId' instead." #-}

instance Core.FromJSON CommentMetadata where
  parseJSON =
    Core.withObject "CommentMetadata" Core.$
      \x ->
        CommentMetadata'
          Core.<$> (x Core..:? "CommentId")
          Core.<*> (x Core..:? "CommentStatus")
          Core.<*> (x Core..:? "Contributor")
          Core.<*> (x Core..:? "CreatedTimestamp")
          Core.<*> (x Core..:? "RecipientId")
