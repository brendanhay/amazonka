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
-- Module      : Network.AWS.CodeCommit.Types.Comment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Comment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a specific comment.
--
-- /See:/ 'newComment' smart constructor.
data Comment = Comment'
  { -- | The emoji reactions to a comment, if any, submitted by the user whose
    -- credentials are associated with the call to the API.
    callerReactions :: Core.Maybe [Core.Text],
    -- | The date and time the comment was most recently modified, in timestamp
    -- format.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The date and time the comment was created, in timestamp format.
    creationDate :: Core.Maybe Core.POSIX,
    -- | A string to integer map that represents the number of individual users
    -- who have responded to a comment with the specified reactions.
    reactionCounts :: Core.Maybe (Core.HashMap Core.Text Core.Int),
    -- | The content of the comment.
    content :: Core.Maybe Core.Text,
    -- | The system-generated comment ID.
    commentId :: Core.Maybe Core.Text,
    -- | The ID of the comment for which this comment is a reply, if any.
    inReplyTo :: Core.Maybe Core.Text,
    -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the person who posted the comment.
    authorArn :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether the comment has been deleted.
    deleted :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Comment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callerReactions', 'comment_callerReactions' - The emoji reactions to a comment, if any, submitted by the user whose
-- credentials are associated with the call to the API.
--
-- 'lastModifiedDate', 'comment_lastModifiedDate' - The date and time the comment was most recently modified, in timestamp
-- format.
--
-- 'creationDate', 'comment_creationDate' - The date and time the comment was created, in timestamp format.
--
-- 'reactionCounts', 'comment_reactionCounts' - A string to integer map that represents the number of individual users
-- who have responded to a comment with the specified reactions.
--
-- 'content', 'comment_content' - The content of the comment.
--
-- 'commentId', 'comment_commentId' - The system-generated comment ID.
--
-- 'inReplyTo', 'comment_inReplyTo' - The ID of the comment for which this comment is a reply, if any.
--
-- 'clientRequestToken', 'comment_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- 'authorArn', 'comment_authorArn' - The Amazon Resource Name (ARN) of the person who posted the comment.
--
-- 'deleted', 'comment_deleted' - A Boolean value indicating whether the comment has been deleted.
newComment ::
  Comment
newComment =
  Comment'
    { callerReactions = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      creationDate = Core.Nothing,
      reactionCounts = Core.Nothing,
      content = Core.Nothing,
      commentId = Core.Nothing,
      inReplyTo = Core.Nothing,
      clientRequestToken = Core.Nothing,
      authorArn = Core.Nothing,
      deleted = Core.Nothing
    }

-- | The emoji reactions to a comment, if any, submitted by the user whose
-- credentials are associated with the call to the API.
comment_callerReactions :: Lens.Lens' Comment (Core.Maybe [Core.Text])
comment_callerReactions = Lens.lens (\Comment' {callerReactions} -> callerReactions) (\s@Comment' {} a -> s {callerReactions = a} :: Comment) Core.. Lens.mapping Lens._Coerce

-- | The date and time the comment was most recently modified, in timestamp
-- format.
comment_lastModifiedDate :: Lens.Lens' Comment (Core.Maybe Core.UTCTime)
comment_lastModifiedDate = Lens.lens (\Comment' {lastModifiedDate} -> lastModifiedDate) (\s@Comment' {} a -> s {lastModifiedDate = a} :: Comment) Core.. Lens.mapping Core._Time

-- | The date and time the comment was created, in timestamp format.
comment_creationDate :: Lens.Lens' Comment (Core.Maybe Core.UTCTime)
comment_creationDate = Lens.lens (\Comment' {creationDate} -> creationDate) (\s@Comment' {} a -> s {creationDate = a} :: Comment) Core.. Lens.mapping Core._Time

-- | A string to integer map that represents the number of individual users
-- who have responded to a comment with the specified reactions.
comment_reactionCounts :: Lens.Lens' Comment (Core.Maybe (Core.HashMap Core.Text Core.Int))
comment_reactionCounts = Lens.lens (\Comment' {reactionCounts} -> reactionCounts) (\s@Comment' {} a -> s {reactionCounts = a} :: Comment) Core.. Lens.mapping Lens._Coerce

-- | The content of the comment.
comment_content :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_content = Lens.lens (\Comment' {content} -> content) (\s@Comment' {} a -> s {content = a} :: Comment)

-- | The system-generated comment ID.
comment_commentId :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_commentId = Lens.lens (\Comment' {commentId} -> commentId) (\s@Comment' {} a -> s {commentId = a} :: Comment)

-- | The ID of the comment for which this comment is a reply, if any.
comment_inReplyTo :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_inReplyTo = Lens.lens (\Comment' {inReplyTo} -> inReplyTo) (\s@Comment' {} a -> s {inReplyTo = a} :: Comment)

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
comment_clientRequestToken :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_clientRequestToken = Lens.lens (\Comment' {clientRequestToken} -> clientRequestToken) (\s@Comment' {} a -> s {clientRequestToken = a} :: Comment)

-- | The Amazon Resource Name (ARN) of the person who posted the comment.
comment_authorArn :: Lens.Lens' Comment (Core.Maybe Core.Text)
comment_authorArn = Lens.lens (\Comment' {authorArn} -> authorArn) (\s@Comment' {} a -> s {authorArn = a} :: Comment)

-- | A Boolean value indicating whether the comment has been deleted.
comment_deleted :: Lens.Lens' Comment (Core.Maybe Core.Bool)
comment_deleted = Lens.lens (\Comment' {deleted} -> deleted) (\s@Comment' {} a -> s {deleted = a} :: Comment)

instance Core.FromJSON Comment where
  parseJSON =
    Core.withObject
      "Comment"
      ( \x ->
          Comment'
            Core.<$> (x Core..:? "callerReactions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "reactionCounts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "content")
            Core.<*> (x Core..:? "commentId")
            Core.<*> (x Core..:? "inReplyTo")
            Core.<*> (x Core..:? "clientRequestToken")
            Core.<*> (x Core..:? "authorArn")
            Core.<*> (x Core..:? "deleted")
      )

instance Core.Hashable Comment

instance Core.NFData Comment
