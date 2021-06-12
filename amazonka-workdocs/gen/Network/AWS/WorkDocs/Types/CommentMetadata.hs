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
-- Module      : Network.AWS.WorkDocs.Types.CommentMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.CommentMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.CommentStatusType
import Network.AWS.WorkDocs.Types.User

-- | Describes the metadata of a comment.
--
-- /See:/ 'newCommentMetadata' smart constructor.
data CommentMetadata = CommentMetadata'
  { -- | The status of the comment.
    commentStatus :: Core.Maybe CommentStatusType,
    -- | The timestamp that the comment was created.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The user who made the comment.
    contributor :: Core.Maybe User,
    -- | The ID of the user being replied to.
    recipientId :: Core.Maybe Core.Text,
    -- | The ID of the comment.
    commentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CommentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentStatus', 'commentMetadata_commentStatus' - The status of the comment.
--
-- 'createdTimestamp', 'commentMetadata_createdTimestamp' - The timestamp that the comment was created.
--
-- 'contributor', 'commentMetadata_contributor' - The user who made the comment.
--
-- 'recipientId', 'commentMetadata_recipientId' - The ID of the user being replied to.
--
-- 'commentId', 'commentMetadata_commentId' - The ID of the comment.
newCommentMetadata ::
  CommentMetadata
newCommentMetadata =
  CommentMetadata'
    { commentStatus = Core.Nothing,
      createdTimestamp = Core.Nothing,
      contributor = Core.Nothing,
      recipientId = Core.Nothing,
      commentId = Core.Nothing
    }

-- | The status of the comment.
commentMetadata_commentStatus :: Lens.Lens' CommentMetadata (Core.Maybe CommentStatusType)
commentMetadata_commentStatus = Lens.lens (\CommentMetadata' {commentStatus} -> commentStatus) (\s@CommentMetadata' {} a -> s {commentStatus = a} :: CommentMetadata)

-- | The timestamp that the comment was created.
commentMetadata_createdTimestamp :: Lens.Lens' CommentMetadata (Core.Maybe Core.UTCTime)
commentMetadata_createdTimestamp = Lens.lens (\CommentMetadata' {createdTimestamp} -> createdTimestamp) (\s@CommentMetadata' {} a -> s {createdTimestamp = a} :: CommentMetadata) Core.. Lens.mapping Core._Time

-- | The user who made the comment.
commentMetadata_contributor :: Lens.Lens' CommentMetadata (Core.Maybe User)
commentMetadata_contributor = Lens.lens (\CommentMetadata' {contributor} -> contributor) (\s@CommentMetadata' {} a -> s {contributor = a} :: CommentMetadata)

-- | The ID of the user being replied to.
commentMetadata_recipientId :: Lens.Lens' CommentMetadata (Core.Maybe Core.Text)
commentMetadata_recipientId = Lens.lens (\CommentMetadata' {recipientId} -> recipientId) (\s@CommentMetadata' {} a -> s {recipientId = a} :: CommentMetadata)

-- | The ID of the comment.
commentMetadata_commentId :: Lens.Lens' CommentMetadata (Core.Maybe Core.Text)
commentMetadata_commentId = Lens.lens (\CommentMetadata' {commentId} -> commentId) (\s@CommentMetadata' {} a -> s {commentId = a} :: CommentMetadata)

instance Core.FromJSON CommentMetadata where
  parseJSON =
    Core.withObject
      "CommentMetadata"
      ( \x ->
          CommentMetadata'
            Core.<$> (x Core..:? "CommentStatus")
            Core.<*> (x Core..:? "CreatedTimestamp")
            Core.<*> (x Core..:? "Contributor")
            Core.<*> (x Core..:? "RecipientId")
            Core.<*> (x Core..:? "CommentId")
      )

instance Core.Hashable CommentMetadata

instance Core.NFData CommentMetadata
