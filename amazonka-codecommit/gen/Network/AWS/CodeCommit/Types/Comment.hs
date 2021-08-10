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
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a specific comment.
--
-- /See:/ 'newComment' smart constructor.
data Comment = Comment'
  { -- | The emoji reactions to a comment, if any, submitted by the user whose
    -- credentials are associated with the call to the API.
    callerReactions :: Prelude.Maybe [Prelude.Text],
    -- | The date and time the comment was most recently modified, in timestamp
    -- format.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time the comment was created, in timestamp format.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | A string to integer map that represents the number of individual users
    -- who have responded to a comment with the specified reactions.
    reactionCounts :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int),
    -- | The content of the comment.
    content :: Prelude.Maybe Prelude.Text,
    -- | The system-generated comment ID.
    commentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the comment for which this comment is a reply, if any.
    inReplyTo :: Prelude.Maybe Prelude.Text,
    -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the person who posted the comment.
    authorArn :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the comment has been deleted.
    deleted :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { callerReactions = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      reactionCounts = Prelude.Nothing,
      content = Prelude.Nothing,
      commentId = Prelude.Nothing,
      inReplyTo = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      authorArn = Prelude.Nothing,
      deleted = Prelude.Nothing
    }

-- | The emoji reactions to a comment, if any, submitted by the user whose
-- credentials are associated with the call to the API.
comment_callerReactions :: Lens.Lens' Comment (Prelude.Maybe [Prelude.Text])
comment_callerReactions = Lens.lens (\Comment' {callerReactions} -> callerReactions) (\s@Comment' {} a -> s {callerReactions = a} :: Comment) Prelude.. Lens.mapping Lens._Coerce

-- | The date and time the comment was most recently modified, in timestamp
-- format.
comment_lastModifiedDate :: Lens.Lens' Comment (Prelude.Maybe Prelude.UTCTime)
comment_lastModifiedDate = Lens.lens (\Comment' {lastModifiedDate} -> lastModifiedDate) (\s@Comment' {} a -> s {lastModifiedDate = a} :: Comment) Prelude.. Lens.mapping Core._Time

-- | The date and time the comment was created, in timestamp format.
comment_creationDate :: Lens.Lens' Comment (Prelude.Maybe Prelude.UTCTime)
comment_creationDate = Lens.lens (\Comment' {creationDate} -> creationDate) (\s@Comment' {} a -> s {creationDate = a} :: Comment) Prelude.. Lens.mapping Core._Time

-- | A string to integer map that represents the number of individual users
-- who have responded to a comment with the specified reactions.
comment_reactionCounts :: Lens.Lens' Comment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
comment_reactionCounts = Lens.lens (\Comment' {reactionCounts} -> reactionCounts) (\s@Comment' {} a -> s {reactionCounts = a} :: Comment) Prelude.. Lens.mapping Lens._Coerce

-- | The content of the comment.
comment_content :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_content = Lens.lens (\Comment' {content} -> content) (\s@Comment' {} a -> s {content = a} :: Comment)

-- | The system-generated comment ID.
comment_commentId :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_commentId = Lens.lens (\Comment' {commentId} -> commentId) (\s@Comment' {} a -> s {commentId = a} :: Comment)

-- | The ID of the comment for which this comment is a reply, if any.
comment_inReplyTo :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_inReplyTo = Lens.lens (\Comment' {inReplyTo} -> inReplyTo) (\s@Comment' {} a -> s {inReplyTo = a} :: Comment)

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
comment_clientRequestToken :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_clientRequestToken = Lens.lens (\Comment' {clientRequestToken} -> clientRequestToken) (\s@Comment' {} a -> s {clientRequestToken = a} :: Comment)

-- | The Amazon Resource Name (ARN) of the person who posted the comment.
comment_authorArn :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_authorArn = Lens.lens (\Comment' {authorArn} -> authorArn) (\s@Comment' {} a -> s {authorArn = a} :: Comment)

-- | A Boolean value indicating whether the comment has been deleted.
comment_deleted :: Lens.Lens' Comment (Prelude.Maybe Prelude.Bool)
comment_deleted = Lens.lens (\Comment' {deleted} -> deleted) (\s@Comment' {} a -> s {deleted = a} :: Comment)

instance Core.FromJSON Comment where
  parseJSON =
    Core.withObject
      "Comment"
      ( \x ->
          Comment'
            Prelude.<$> ( x Core..:? "callerReactions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lastModifiedDate")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "reactionCounts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "content")
            Prelude.<*> (x Core..:? "commentId")
            Prelude.<*> (x Core..:? "inReplyTo")
            Prelude.<*> (x Core..:? "clientRequestToken")
            Prelude.<*> (x Core..:? "authorArn")
            Prelude.<*> (x Core..:? "deleted")
      )

instance Prelude.Hashable Comment

instance Prelude.NFData Comment
