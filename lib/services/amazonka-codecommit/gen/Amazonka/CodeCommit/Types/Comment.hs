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
-- Module      : Amazonka.CodeCommit.Types.Comment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Comment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a specific comment.
--
-- /See:/ 'newComment' smart constructor.
data Comment = Comment'
  { -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request is received with the same parameters and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A string to integer map that represents the number of individual users
    -- who have responded to a comment with the specified reactions.
    reactionCounts :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int),
    -- | The date and time the comment was most recently modified, in timestamp
    -- format.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | A Boolean value indicating whether the comment has been deleted.
    deleted :: Prelude.Maybe Prelude.Bool,
    -- | The date and time the comment was created, in timestamp format.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The system-generated comment ID.
    commentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the comment for which this comment is a reply, if any.
    inReplyTo :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the person who posted the comment.
    authorArn :: Prelude.Maybe Prelude.Text,
    -- | The content of the comment.
    content :: Prelude.Maybe Prelude.Text,
    -- | The emoji reactions to a comment, if any, submitted by the user whose
    -- credentials are associated with the call to the API.
    callerReactions :: Prelude.Maybe [Prelude.Text]
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
-- 'clientRequestToken', 'comment_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- 'reactionCounts', 'comment_reactionCounts' - A string to integer map that represents the number of individual users
-- who have responded to a comment with the specified reactions.
--
-- 'lastModifiedDate', 'comment_lastModifiedDate' - The date and time the comment was most recently modified, in timestamp
-- format.
--
-- 'deleted', 'comment_deleted' - A Boolean value indicating whether the comment has been deleted.
--
-- 'creationDate', 'comment_creationDate' - The date and time the comment was created, in timestamp format.
--
-- 'commentId', 'comment_commentId' - The system-generated comment ID.
--
-- 'inReplyTo', 'comment_inReplyTo' - The ID of the comment for which this comment is a reply, if any.
--
-- 'authorArn', 'comment_authorArn' - The Amazon Resource Name (ARN) of the person who posted the comment.
--
-- 'content', 'comment_content' - The content of the comment.
--
-- 'callerReactions', 'comment_callerReactions' - The emoji reactions to a comment, if any, submitted by the user whose
-- credentials are associated with the call to the API.
newComment ::
  Comment
newComment =
  Comment'
    { clientRequestToken = Prelude.Nothing,
      reactionCounts = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      deleted = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      commentId = Prelude.Nothing,
      inReplyTo = Prelude.Nothing,
      authorArn = Prelude.Nothing,
      content = Prelude.Nothing,
      callerReactions = Prelude.Nothing
    }

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request is received with the same parameters and a token
-- is included, the request returns information about the initial request
-- that used that token.
comment_clientRequestToken :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_clientRequestToken = Lens.lens (\Comment' {clientRequestToken} -> clientRequestToken) (\s@Comment' {} a -> s {clientRequestToken = a} :: Comment)

-- | A string to integer map that represents the number of individual users
-- who have responded to a comment with the specified reactions.
comment_reactionCounts :: Lens.Lens' Comment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
comment_reactionCounts = Lens.lens (\Comment' {reactionCounts} -> reactionCounts) (\s@Comment' {} a -> s {reactionCounts = a} :: Comment) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the comment was most recently modified, in timestamp
-- format.
comment_lastModifiedDate :: Lens.Lens' Comment (Prelude.Maybe Prelude.UTCTime)
comment_lastModifiedDate = Lens.lens (\Comment' {lastModifiedDate} -> lastModifiedDate) (\s@Comment' {} a -> s {lastModifiedDate = a} :: Comment) Prelude.. Lens.mapping Data._Time

-- | A Boolean value indicating whether the comment has been deleted.
comment_deleted :: Lens.Lens' Comment (Prelude.Maybe Prelude.Bool)
comment_deleted = Lens.lens (\Comment' {deleted} -> deleted) (\s@Comment' {} a -> s {deleted = a} :: Comment)

-- | The date and time the comment was created, in timestamp format.
comment_creationDate :: Lens.Lens' Comment (Prelude.Maybe Prelude.UTCTime)
comment_creationDate = Lens.lens (\Comment' {creationDate} -> creationDate) (\s@Comment' {} a -> s {creationDate = a} :: Comment) Prelude.. Lens.mapping Data._Time

-- | The system-generated comment ID.
comment_commentId :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_commentId = Lens.lens (\Comment' {commentId} -> commentId) (\s@Comment' {} a -> s {commentId = a} :: Comment)

-- | The ID of the comment for which this comment is a reply, if any.
comment_inReplyTo :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_inReplyTo = Lens.lens (\Comment' {inReplyTo} -> inReplyTo) (\s@Comment' {} a -> s {inReplyTo = a} :: Comment)

-- | The Amazon Resource Name (ARN) of the person who posted the comment.
comment_authorArn :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_authorArn = Lens.lens (\Comment' {authorArn} -> authorArn) (\s@Comment' {} a -> s {authorArn = a} :: Comment)

-- | The content of the comment.
comment_content :: Lens.Lens' Comment (Prelude.Maybe Prelude.Text)
comment_content = Lens.lens (\Comment' {content} -> content) (\s@Comment' {} a -> s {content = a} :: Comment)

-- | The emoji reactions to a comment, if any, submitted by the user whose
-- credentials are associated with the call to the API.
comment_callerReactions :: Lens.Lens' Comment (Prelude.Maybe [Prelude.Text])
comment_callerReactions = Lens.lens (\Comment' {callerReactions} -> callerReactions) (\s@Comment' {} a -> s {callerReactions = a} :: Comment) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Comment where
  parseJSON =
    Data.withObject
      "Comment"
      ( \x ->
          Comment'
            Prelude.<$> (x Data..:? "clientRequestToken")
            Prelude.<*> (x Data..:? "reactionCounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "deleted")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "commentId")
            Prelude.<*> (x Data..:? "inReplyTo")
            Prelude.<*> (x Data..:? "authorArn")
            Prelude.<*> (x Data..:? "content")
            Prelude.<*> ( x Data..:? "callerReactions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Comment where
  hashWithSalt _salt Comment' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` reactionCounts
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` deleted
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` commentId
      `Prelude.hashWithSalt` inReplyTo
      `Prelude.hashWithSalt` authorArn
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` callerReactions

instance Prelude.NFData Comment where
  rnf Comment' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf reactionCounts
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf deleted
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf commentId
      `Prelude.seq` Prelude.rnf inReplyTo
      `Prelude.seq` Prelude.rnf authorArn
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf callerReactions
