{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.CommentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.CommentMetadata
  ( CommentMetadata (..)
  -- * Smart constructor
  , mkCommentMetadata
  -- * Lenses
  , cmCommentId
  , cmCommentStatus
  , cmContributor
  , cmCreatedTimestamp
  , cmRecipientId
  ) where

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
  { commentId :: Core.Maybe Types.CommentIdType
    -- ^ The ID of the comment.
  , commentStatus :: Core.Maybe Types.CommentStatusType
    -- ^ The status of the comment.
  , contributor :: Core.Maybe Types.User
    -- ^ The user who made the comment.
  , createdTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp that the comment was created.
  , recipientId :: Core.Maybe Types.IdType
    -- ^ The ID of the user being replied to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CommentMetadata' value with any optional fields omitted.
mkCommentMetadata
    :: CommentMetadata
mkCommentMetadata
  = CommentMetadata'{commentId = Core.Nothing,
                     commentStatus = Core.Nothing, contributor = Core.Nothing,
                     createdTimestamp = Core.Nothing, recipientId = Core.Nothing}

-- | The ID of the comment.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCommentId :: Lens.Lens' CommentMetadata (Core.Maybe Types.CommentIdType)
cmCommentId = Lens.field @"commentId"
{-# INLINEABLE cmCommentId #-}
{-# DEPRECATED commentId "Use generic-lens or generic-optics with 'commentId' instead"  #-}

-- | The status of the comment.
--
-- /Note:/ Consider using 'commentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCommentStatus :: Lens.Lens' CommentMetadata (Core.Maybe Types.CommentStatusType)
cmCommentStatus = Lens.field @"commentStatus"
{-# INLINEABLE cmCommentStatus #-}
{-# DEPRECATED commentStatus "Use generic-lens or generic-optics with 'commentStatus' instead"  #-}

-- | The user who made the comment.
--
-- /Note:/ Consider using 'contributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContributor :: Lens.Lens' CommentMetadata (Core.Maybe Types.User)
cmContributor = Lens.field @"contributor"
{-# INLINEABLE cmContributor #-}
{-# DEPRECATED contributor "Use generic-lens or generic-optics with 'contributor' instead"  #-}

-- | The timestamp that the comment was created.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCreatedTimestamp :: Lens.Lens' CommentMetadata (Core.Maybe Core.NominalDiffTime)
cmCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE cmCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The ID of the user being replied to.
--
-- /Note:/ Consider using 'recipientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRecipientId :: Lens.Lens' CommentMetadata (Core.Maybe Types.IdType)
cmRecipientId = Lens.field @"recipientId"
{-# INLINEABLE cmRecipientId #-}
{-# DEPRECATED recipientId "Use generic-lens or generic-optics with 'recipientId' instead"  #-}

instance Core.FromJSON CommentMetadata where
        parseJSON
          = Core.withObject "CommentMetadata" Core.$
              \ x ->
                CommentMetadata' Core.<$>
                  (x Core..:? "CommentId") Core.<*> x Core..:? "CommentStatus"
                    Core.<*> x Core..:? "Contributor"
                    Core.<*> x Core..:? "CreatedTimestamp"
                    Core.<*> x Core..:? "RecipientId"
