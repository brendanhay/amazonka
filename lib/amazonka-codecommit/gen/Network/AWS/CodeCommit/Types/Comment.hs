-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Comment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Comment
  ( Comment (..),

    -- * Smart constructor
    mkComment,

    -- * Lenses
    cLastModifiedDate,
    cAuthorARN,
    cContent,
    cCallerReactions,
    cCreationDate,
    cDeleted,
    cClientRequestToken,
    cCommentId,
    cInReplyTo,
    cReactionCounts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a specific comment.
--
-- /See:/ 'mkComment' smart constructor.
data Comment = Comment'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    authorARN :: Lude.Maybe Lude.Text,
    content :: Lude.Maybe Lude.Text,
    callerReactions :: Lude.Maybe [Lude.Text],
    creationDate :: Lude.Maybe Lude.Timestamp,
    deleted :: Lude.Maybe Lude.Bool,
    clientRequestToken :: Lude.Maybe Lude.Text,
    commentId :: Lude.Maybe Lude.Text,
    inReplyTo :: Lude.Maybe Lude.Text,
    reactionCounts :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Comment' with the minimum fields required to make a request.
--
-- * 'authorARN' - The Amazon Resource Name (ARN) of the person who posted the comment.
-- * 'callerReactions' - The emoji reactions to a comment, if any, submitted by the user whose credentials are associated with the call to the API.
-- * 'clientRequestToken' - A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
-- * 'commentId' - The system-generated comment ID.
-- * 'content' - The content of the comment.
-- * 'creationDate' - The date and time the comment was created, in timestamp format.
-- * 'deleted' - A Boolean value indicating whether the comment has been deleted.
-- * 'inReplyTo' - The ID of the comment for which this comment is a reply, if any.
-- * 'lastModifiedDate' - The date and time the comment was most recently modified, in timestamp format.
-- * 'reactionCounts' - A string to integer map that represents the number of individual users who have responded to a comment with the specified reactions.
mkComment ::
  Comment
mkComment =
  Comment'
    { lastModifiedDate = Lude.Nothing,
      authorARN = Lude.Nothing,
      content = Lude.Nothing,
      callerReactions = Lude.Nothing,
      creationDate = Lude.Nothing,
      deleted = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      commentId = Lude.Nothing,
      inReplyTo = Lude.Nothing,
      reactionCounts = Lude.Nothing
    }

-- | The date and time the comment was most recently modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastModifiedDate :: Lens.Lens' Comment (Lude.Maybe Lude.Timestamp)
cLastModifiedDate = Lens.lens (lastModifiedDate :: Comment -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: Comment)
{-# DEPRECATED cLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the person who posted the comment.
--
-- /Note:/ Consider using 'authorARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAuthorARN :: Lens.Lens' Comment (Lude.Maybe Lude.Text)
cAuthorARN = Lens.lens (authorARN :: Comment -> Lude.Maybe Lude.Text) (\s a -> s {authorARN = a} :: Comment)
{-# DEPRECATED cAuthorARN "Use generic-lens or generic-optics with 'authorARN' instead." #-}

-- | The content of the comment.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContent :: Lens.Lens' Comment (Lude.Maybe Lude.Text)
cContent = Lens.lens (content :: Comment -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: Comment)
{-# DEPRECATED cContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The emoji reactions to a comment, if any, submitted by the user whose credentials are associated with the call to the API.
--
-- /Note:/ Consider using 'callerReactions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCallerReactions :: Lens.Lens' Comment (Lude.Maybe [Lude.Text])
cCallerReactions = Lens.lens (callerReactions :: Comment -> Lude.Maybe [Lude.Text]) (\s a -> s {callerReactions = a} :: Comment)
{-# DEPRECATED cCallerReactions "Use generic-lens or generic-optics with 'callerReactions' instead." #-}

-- | The date and time the comment was created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreationDate :: Lens.Lens' Comment (Lude.Maybe Lude.Timestamp)
cCreationDate = Lens.lens (creationDate :: Comment -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: Comment)
{-# DEPRECATED cCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A Boolean value indicating whether the comment has been deleted.
--
-- /Note:/ Consider using 'deleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDeleted :: Lens.Lens' Comment (Lude.Maybe Lude.Bool)
cDeleted = Lens.lens (deleted :: Comment -> Lude.Maybe Lude.Bool) (\s a -> s {deleted = a} :: Comment)
{-# DEPRECATED cDeleted "Use generic-lens or generic-optics with 'deleted' instead." #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClientRequestToken :: Lens.Lens' Comment (Lude.Maybe Lude.Text)
cClientRequestToken = Lens.lens (clientRequestToken :: Comment -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: Comment)
{-# DEPRECATED cClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The system-generated comment ID.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommentId :: Lens.Lens' Comment (Lude.Maybe Lude.Text)
cCommentId = Lens.lens (commentId :: Comment -> Lude.Maybe Lude.Text) (\s a -> s {commentId = a} :: Comment)
{-# DEPRECATED cCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | The ID of the comment for which this comment is a reply, if any.
--
-- /Note:/ Consider using 'inReplyTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInReplyTo :: Lens.Lens' Comment (Lude.Maybe Lude.Text)
cInReplyTo = Lens.lens (inReplyTo :: Comment -> Lude.Maybe Lude.Text) (\s a -> s {inReplyTo = a} :: Comment)
{-# DEPRECATED cInReplyTo "Use generic-lens or generic-optics with 'inReplyTo' instead." #-}

-- | A string to integer map that represents the number of individual users who have responded to a comment with the specified reactions.
--
-- /Note:/ Consider using 'reactionCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReactionCounts :: Lens.Lens' Comment (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)))
cReactionCounts = Lens.lens (reactionCounts :: Comment -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))) (\s a -> s {reactionCounts = a} :: Comment)
{-# DEPRECATED cReactionCounts "Use generic-lens or generic-optics with 'reactionCounts' instead." #-}

instance Lude.FromJSON Comment where
  parseJSON =
    Lude.withObject
      "Comment"
      ( \x ->
          Comment'
            Lude.<$> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "authorArn")
            Lude.<*> (x Lude..:? "content")
            Lude.<*> (x Lude..:? "callerReactions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "deleted")
            Lude.<*> (x Lude..:? "clientRequestToken")
            Lude.<*> (x Lude..:? "commentId")
            Lude.<*> (x Lude..:? "inReplyTo")
            Lude.<*> (x Lude..:? "reactionCounts" Lude..!= Lude.mempty)
      )
