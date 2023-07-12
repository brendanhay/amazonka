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
-- Module      : Amazonka.WorkDocs.Types.CommentMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.CommentMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.CommentStatusType
import Amazonka.WorkDocs.Types.User

-- | Describes the metadata of a comment.
--
-- /See:/ 'newCommentMetadata' smart constructor.
data CommentMetadata = CommentMetadata'
  { -- | The ID of the comment.
    commentId :: Prelude.Maybe Prelude.Text,
    -- | The status of the comment.
    commentStatus :: Prelude.Maybe CommentStatusType,
    -- | The user who made the comment.
    contributor :: Prelude.Maybe User,
    -- | The timestamp that the comment was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ID of the user being replied to.
    recipientId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommentMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commentId', 'commentMetadata_commentId' - The ID of the comment.
--
-- 'commentStatus', 'commentMetadata_commentStatus' - The status of the comment.
--
-- 'contributor', 'commentMetadata_contributor' - The user who made the comment.
--
-- 'createdTimestamp', 'commentMetadata_createdTimestamp' - The timestamp that the comment was created.
--
-- 'recipientId', 'commentMetadata_recipientId' - The ID of the user being replied to.
newCommentMetadata ::
  CommentMetadata
newCommentMetadata =
  CommentMetadata'
    { commentId = Prelude.Nothing,
      commentStatus = Prelude.Nothing,
      contributor = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      recipientId = Prelude.Nothing
    }

-- | The ID of the comment.
commentMetadata_commentId :: Lens.Lens' CommentMetadata (Prelude.Maybe Prelude.Text)
commentMetadata_commentId = Lens.lens (\CommentMetadata' {commentId} -> commentId) (\s@CommentMetadata' {} a -> s {commentId = a} :: CommentMetadata)

-- | The status of the comment.
commentMetadata_commentStatus :: Lens.Lens' CommentMetadata (Prelude.Maybe CommentStatusType)
commentMetadata_commentStatus = Lens.lens (\CommentMetadata' {commentStatus} -> commentStatus) (\s@CommentMetadata' {} a -> s {commentStatus = a} :: CommentMetadata)

-- | The user who made the comment.
commentMetadata_contributor :: Lens.Lens' CommentMetadata (Prelude.Maybe User)
commentMetadata_contributor = Lens.lens (\CommentMetadata' {contributor} -> contributor) (\s@CommentMetadata' {} a -> s {contributor = a} :: CommentMetadata)

-- | The timestamp that the comment was created.
commentMetadata_createdTimestamp :: Lens.Lens' CommentMetadata (Prelude.Maybe Prelude.UTCTime)
commentMetadata_createdTimestamp = Lens.lens (\CommentMetadata' {createdTimestamp} -> createdTimestamp) (\s@CommentMetadata' {} a -> s {createdTimestamp = a} :: CommentMetadata) Prelude.. Lens.mapping Data._Time

-- | The ID of the user being replied to.
commentMetadata_recipientId :: Lens.Lens' CommentMetadata (Prelude.Maybe Prelude.Text)
commentMetadata_recipientId = Lens.lens (\CommentMetadata' {recipientId} -> recipientId) (\s@CommentMetadata' {} a -> s {recipientId = a} :: CommentMetadata)

instance Data.FromJSON CommentMetadata where
  parseJSON =
    Data.withObject
      "CommentMetadata"
      ( \x ->
          CommentMetadata'
            Prelude.<$> (x Data..:? "CommentId")
            Prelude.<*> (x Data..:? "CommentStatus")
            Prelude.<*> (x Data..:? "Contributor")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "RecipientId")
      )

instance Prelude.Hashable CommentMetadata where
  hashWithSalt _salt CommentMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` commentId
      `Prelude.hashWithSalt` commentStatus
      `Prelude.hashWithSalt` contributor
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` recipientId

instance Prelude.NFData CommentMetadata where
  rnf CommentMetadata' {..} =
    Prelude.rnf commentId
      `Prelude.seq` Prelude.rnf commentStatus
      `Prelude.seq` Prelude.rnf contributor
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf recipientId
