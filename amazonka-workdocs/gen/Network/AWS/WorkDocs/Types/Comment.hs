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
-- Module      : Network.AWS.WorkDocs.Types.Comment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Comment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.CommentStatusType
import Network.AWS.WorkDocs.Types.CommentVisibilityType
import Network.AWS.WorkDocs.Types.User

-- | Describes a comment.
--
-- /See:/ 'newComment' smart constructor.
data Comment = Comment'
  { -- | The status of the comment.
    status :: Core.Maybe CommentStatusType,
    -- | The time that the comment was created.
    createdTimestamp :: Core.Maybe Core.POSIX,
    -- | The details of the user who made the comment.
    contributor :: Core.Maybe User,
    -- | The ID of the parent comment.
    parentId :: Core.Maybe Core.Text,
    -- | If the comment is a reply to another user\'s comment, this field
    -- contains the user ID of the user being replied to.
    recipientId :: Core.Maybe Core.Text,
    -- | The visibility of the comment. Options are either PRIVATE, where the
    -- comment is visible only to the comment author and document owner and
    -- co-owners, or PUBLIC, where the comment is visible to document owners,
    -- co-owners, and contributors.
    visibility :: Core.Maybe CommentVisibilityType,
    -- | The ID of the root comment in the thread.
    threadId :: Core.Maybe Core.Text,
    -- | The text of the comment.
    text :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the comment.
    commentId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'Comment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'comment_status' - The status of the comment.
--
-- 'createdTimestamp', 'comment_createdTimestamp' - The time that the comment was created.
--
-- 'contributor', 'comment_contributor' - The details of the user who made the comment.
--
-- 'parentId', 'comment_parentId' - The ID of the parent comment.
--
-- 'recipientId', 'comment_recipientId' - If the comment is a reply to another user\'s comment, this field
-- contains the user ID of the user being replied to.
--
-- 'visibility', 'comment_visibility' - The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
--
-- 'threadId', 'comment_threadId' - The ID of the root comment in the thread.
--
-- 'text', 'comment_text' - The text of the comment.
--
-- 'commentId', 'comment_commentId' - The ID of the comment.
newComment ::
  -- | 'commentId'
  Core.Text ->
  Comment
newComment pCommentId_ =
  Comment'
    { status = Core.Nothing,
      createdTimestamp = Core.Nothing,
      contributor = Core.Nothing,
      parentId = Core.Nothing,
      recipientId = Core.Nothing,
      visibility = Core.Nothing,
      threadId = Core.Nothing,
      text = Core.Nothing,
      commentId = pCommentId_
    }

-- | The status of the comment.
comment_status :: Lens.Lens' Comment (Core.Maybe CommentStatusType)
comment_status = Lens.lens (\Comment' {status} -> status) (\s@Comment' {} a -> s {status = a} :: Comment)

-- | The time that the comment was created.
comment_createdTimestamp :: Lens.Lens' Comment (Core.Maybe Core.UTCTime)
comment_createdTimestamp = Lens.lens (\Comment' {createdTimestamp} -> createdTimestamp) (\s@Comment' {} a -> s {createdTimestamp = a} :: Comment) Core.. Lens.mapping Core._Time

-- | The details of the user who made the comment.
comment_contributor :: Lens.Lens' Comment (Core.Maybe User)
comment_contributor = Lens.lens (\Comment' {contributor} -> contributor) (\s@Comment' {} a -> s {contributor = a} :: Comment)

-- | The ID of the parent comment.
comment_parentId :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_parentId = Lens.lens (\Comment' {parentId} -> parentId) (\s@Comment' {} a -> s {parentId = a} :: Comment)

-- | If the comment is a reply to another user\'s comment, this field
-- contains the user ID of the user being replied to.
comment_recipientId :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_recipientId = Lens.lens (\Comment' {recipientId} -> recipientId) (\s@Comment' {} a -> s {recipientId = a} :: Comment)

-- | The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
comment_visibility :: Lens.Lens' Comment (Core.Maybe CommentVisibilityType)
comment_visibility = Lens.lens (\Comment' {visibility} -> visibility) (\s@Comment' {} a -> s {visibility = a} :: Comment)

-- | The ID of the root comment in the thread.
comment_threadId :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_threadId = Lens.lens (\Comment' {threadId} -> threadId) (\s@Comment' {} a -> s {threadId = a} :: Comment)

-- | The text of the comment.
comment_text :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_text = Lens.lens (\Comment' {text} -> text) (\s@Comment' {} a -> s {text = a} :: Comment) Core.. Lens.mapping Core._Sensitive

-- | The ID of the comment.
comment_commentId :: Lens.Lens' Comment Core.Text
comment_commentId = Lens.lens (\Comment' {commentId} -> commentId) (\s@Comment' {} a -> s {commentId = a} :: Comment)

instance Core.FromJSON Comment where
  parseJSON =
    Core.withObject
      "Comment"
      ( \x ->
          Comment'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreatedTimestamp")
            Core.<*> (x Core..:? "Contributor")
            Core.<*> (x Core..:? "ParentId")
            Core.<*> (x Core..:? "RecipientId")
            Core.<*> (x Core..:? "Visibility")
            Core.<*> (x Core..:? "ThreadId")
            Core.<*> (x Core..:? "Text")
            Core.<*> (x Core..: "CommentId")
      )

instance Core.Hashable Comment

instance Core.NFData Comment
