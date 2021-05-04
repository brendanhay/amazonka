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
-- Module      : Network.AWS.WorkDocs.Types.Comment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Comment where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.CommentStatusType
import Network.AWS.WorkDocs.Types.CommentVisibilityType
import Network.AWS.WorkDocs.Types.User

-- | Describes a comment.
--
-- /See:/ 'newComment' smart constructor.
data Comment = Comment'
  { -- | The status of the comment.
    status :: Prelude.Maybe CommentStatusType,
    -- | The time that the comment was created.
    createdTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The details of the user who made the comment.
    contributor :: Prelude.Maybe User,
    -- | The ID of the parent comment.
    parentId :: Prelude.Maybe Prelude.Text,
    -- | If the comment is a reply to another user\'s comment, this field
    -- contains the user ID of the user being replied to.
    recipientId :: Prelude.Maybe Prelude.Text,
    -- | The visibility of the comment. Options are either PRIVATE, where the
    -- comment is visible only to the comment author and document owner and
    -- co-owners, or PUBLIC, where the comment is visible to document owners,
    -- co-owners, and contributors.
    visibility :: Prelude.Maybe CommentVisibilityType,
    -- | The ID of the root comment in the thread.
    threadId :: Prelude.Maybe Prelude.Text,
    -- | The text of the comment.
    text :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the comment.
    commentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  Comment
newComment pCommentId_ =
  Comment'
    { status = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      contributor = Prelude.Nothing,
      parentId = Prelude.Nothing,
      recipientId = Prelude.Nothing,
      visibility = Prelude.Nothing,
      threadId = Prelude.Nothing,
      text = Prelude.Nothing,
      commentId = pCommentId_
    }

-- | The status of the comment.
comment_status :: Lens.Lens' Comment (Prelude.Maybe CommentStatusType)
comment_status = Lens.lens (\Comment' {status} -> status) (\s@Comment' {} a -> s {status = a} :: Comment)

-- | The time that the comment was created.
comment_createdTimestamp :: Lens.Lens' Comment (Prelude.Maybe Prelude.UTCTime)
comment_createdTimestamp = Lens.lens (\Comment' {createdTimestamp} -> createdTimestamp) (\s@Comment' {} a -> s {createdTimestamp = a} :: Comment) Prelude.. Lens.mapping Prelude._Time

-- | The details of the user who made the comment.
comment_contributor :: Lens.Lens' Comment (Prelude.Maybe User)
comment_contributor = Lens.lens (\Comment' {contributor} -> contributor) (\s@Comment' {} a -> s {contributor = a} :: Comment)

-- | The ID of the parent comment.
comment_parentId :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_parentId = Lens.lens (\Comment' {parentId} -> parentId) (\s@Comment' {} a -> s {parentId = a} :: Comment)

-- | If the comment is a reply to another user\'s comment, this field
-- contains the user ID of the user being replied to.
comment_recipientId :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_recipientId = Lens.lens (\Comment' {recipientId} -> recipientId) (\s@Comment' {} a -> s {recipientId = a} :: Comment)

-- | The visibility of the comment. Options are either PRIVATE, where the
-- comment is visible only to the comment author and document owner and
-- co-owners, or PUBLIC, where the comment is visible to document owners,
-- co-owners, and contributors.
comment_visibility :: Lens.Lens' Comment (Prelude.Maybe CommentVisibilityType)
comment_visibility = Lens.lens (\Comment' {visibility} -> visibility) (\s@Comment' {} a -> s {visibility = a} :: Comment)

-- | The ID of the root comment in the thread.
comment_threadId :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_threadId = Lens.lens (\Comment' {threadId} -> threadId) (\s@Comment' {} a -> s {threadId = a} :: Comment)

-- | The text of the comment.
comment_text :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_text = Lens.lens (\Comment' {text} -> text) (\s@Comment' {} a -> s {text = a} :: Comment) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the comment.
comment_commentId :: Lens.Lens' Comment Prelude.Text
comment_commentId = Lens.lens (\Comment' {commentId} -> commentId) (\s@Comment' {} a -> s {commentId = a} :: Comment)

instance Prelude.FromJSON Comment where
  parseJSON =
    Prelude.withObject
      "Comment"
      ( \x ->
          Comment'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "CreatedTimestamp")
            Prelude.<*> (x Prelude..:? "Contributor")
            Prelude.<*> (x Prelude..:? "ParentId")
            Prelude.<*> (x Prelude..:? "RecipientId")
            Prelude.<*> (x Prelude..:? "Visibility")
            Prelude.<*> (x Prelude..:? "ThreadId")
            Prelude.<*> (x Prelude..:? "Text")
            Prelude.<*> (x Prelude..: "CommentId")
      )

instance Prelude.Hashable Comment

instance Prelude.NFData Comment
