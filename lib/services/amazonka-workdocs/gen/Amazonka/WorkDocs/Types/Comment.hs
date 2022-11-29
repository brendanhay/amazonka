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
-- Module      : Amazonka.WorkDocs.Types.Comment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.Comment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.CommentStatusType
import Amazonka.WorkDocs.Types.CommentVisibilityType
import Amazonka.WorkDocs.Types.User

-- | Describes a comment.
--
-- /See:/ 'newComment' smart constructor.
data Comment = Comment'
  { -- | The time that the comment was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ID of the root comment in the thread.
    threadId :: Prelude.Maybe Prelude.Text,
    -- | The visibility of the comment. Options are either PRIVATE, where the
    -- comment is visible only to the comment author and document owner and
    -- co-owners, or PUBLIC, where the comment is visible to document owners,
    -- co-owners, and contributors.
    visibility :: Prelude.Maybe CommentVisibilityType,
    -- | The ID of the parent comment.
    parentId :: Prelude.Maybe Prelude.Text,
    -- | The status of the comment.
    status :: Prelude.Maybe CommentStatusType,
    -- | The text of the comment.
    text :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The details of the user who made the comment.
    contributor :: Prelude.Maybe User,
    -- | If the comment is a reply to another user\'s comment, this field
    -- contains the user ID of the user being replied to.
    recipientId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the comment.
    commentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Comment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'comment_createdTimestamp' - The time that the comment was created.
--
-- 'threadId', 'comment_threadId' - The ID of the root comment in the thread.
--
-- 'visibility', 'comment_visibility' - The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
--
-- 'parentId', 'comment_parentId' - The ID of the parent comment.
--
-- 'status', 'comment_status' - The status of the comment.
--
-- 'text', 'comment_text' - The text of the comment.
--
-- 'contributor', 'comment_contributor' - The details of the user who made the comment.
--
-- 'recipientId', 'comment_recipientId' - If the comment is a reply to another user\'s comment, this field
-- contains the user ID of the user being replied to.
--
-- 'commentId', 'comment_commentId' - The ID of the comment.
newComment ::
  -- | 'commentId'
  Prelude.Text ->
  Comment
newComment pCommentId_ =
  Comment'
    { createdTimestamp = Prelude.Nothing,
      threadId = Prelude.Nothing,
      visibility = Prelude.Nothing,
      parentId = Prelude.Nothing,
      status = Prelude.Nothing,
      text = Prelude.Nothing,
      contributor = Prelude.Nothing,
      recipientId = Prelude.Nothing,
      commentId = pCommentId_
    }

-- | The time that the comment was created.
comment_createdTimestamp :: Lens.Lens' Comment (Prelude.Maybe Prelude.UTCTime)
comment_createdTimestamp = Lens.lens (\Comment' {createdTimestamp} -> createdTimestamp) (\s@Comment' {} a -> s {createdTimestamp = a} :: Comment) Prelude.. Lens.mapping Core._Time

-- | The ID of the root comment in the thread.
comment_threadId :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_threadId = Lens.lens (\Comment' {threadId} -> threadId) (\s@Comment' {} a -> s {threadId = a} :: Comment)

-- | The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
comment_visibility :: Lens.Lens' Comment (Prelude.Maybe CommentVisibilityType)
comment_visibility = Lens.lens (\Comment' {visibility} -> visibility) (\s@Comment' {} a -> s {visibility = a} :: Comment)

-- | The ID of the parent comment.
comment_parentId :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_parentId = Lens.lens (\Comment' {parentId} -> parentId) (\s@Comment' {} a -> s {parentId = a} :: Comment)

-- | The status of the comment.
comment_status :: Lens.Lens' Comment (Prelude.Maybe CommentStatusType)
comment_status = Lens.lens (\Comment' {status} -> status) (\s@Comment' {} a -> s {status = a} :: Comment)

-- | The text of the comment.
comment_text :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_text = Lens.lens (\Comment' {text} -> text) (\s@Comment' {} a -> s {text = a} :: Comment) Prelude.. Lens.mapping Core._Sensitive

-- | The details of the user who made the comment.
comment_contributor :: Lens.Lens' Comment (Prelude.Maybe User)
comment_contributor = Lens.lens (\Comment' {contributor} -> contributor) (\s@Comment' {} a -> s {contributor = a} :: Comment)

-- | If the comment is a reply to another user\'s comment, this field
-- contains the user ID of the user being replied to.
comment_recipientId :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_recipientId = Lens.lens (\Comment' {recipientId} -> recipientId) (\s@Comment' {} a -> s {recipientId = a} :: Comment)

-- | The ID of the comment.
comment_commentId :: Lens.Lens' Comment Prelude.Text
comment_commentId = Lens.lens (\Comment' {commentId} -> commentId) (\s@Comment' {} a -> s {commentId = a} :: Comment)

instance Core.FromJSON Comment where
  parseJSON =
    Core.withObject
      "Comment"
      ( \x ->
          Comment'
            Prelude.<$> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "ThreadId")
            Prelude.<*> (x Core..:? "Visibility")
            Prelude.<*> (x Core..:? "ParentId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Contributor")
            Prelude.<*> (x Core..:? "RecipientId")
            Prelude.<*> (x Core..: "CommentId")
      )

instance Prelude.Hashable Comment where
  hashWithSalt _salt Comment' {..} =
    _salt `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` threadId
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` contributor
      `Prelude.hashWithSalt` recipientId
      `Prelude.hashWithSalt` commentId

instance Prelude.NFData Comment where
  rnf Comment' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf threadId
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf parentId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf contributor
      `Prelude.seq` Prelude.rnf recipientId
      `Prelude.seq` Prelude.rnf commentId
