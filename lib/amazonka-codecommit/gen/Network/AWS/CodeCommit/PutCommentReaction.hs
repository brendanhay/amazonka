{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PutCommentReaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a reaction to a specified comment for the user whose identity is used to make the request. You can only add or update a reaction for yourself. You cannot add, modify, or delete a reaction for another user.
module Network.AWS.CodeCommit.PutCommentReaction
  ( -- * Creating a request
    PutCommentReaction (..),
    mkPutCommentReaction,

    -- ** Request lenses
    pcrCommentId,
    pcrReactionValue,

    -- * Destructuring the response
    PutCommentReactionResponse (..),
    mkPutCommentReactionResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutCommentReaction' smart constructor.
data PutCommentReaction = PutCommentReaction'
  { commentId ::
      Lude.Text,
    reactionValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutCommentReaction' with the minimum fields required to make a request.
--
-- * 'commentId' - The ID of the comment to which you want to add or update a reaction.
-- * 'reactionValue' - The emoji reaction you want to add or update. To remove a reaction, provide a value of blank or null. You can also provide the value of none. For information about emoji reaction values supported in AWS CodeCommit, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide> .
mkPutCommentReaction ::
  -- | 'commentId'
  Lude.Text ->
  -- | 'reactionValue'
  Lude.Text ->
  PutCommentReaction
mkPutCommentReaction pCommentId_ pReactionValue_ =
  PutCommentReaction'
    { commentId = pCommentId_,
      reactionValue = pReactionValue_
    }

-- | The ID of the comment to which you want to add or update a reaction.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrCommentId :: Lens.Lens' PutCommentReaction Lude.Text
pcrCommentId = Lens.lens (commentId :: PutCommentReaction -> Lude.Text) (\s a -> s {commentId = a} :: PutCommentReaction)
{-# DEPRECATED pcrCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | The emoji reaction you want to add or update. To remove a reaction, provide a value of blank or null. You can also provide the value of none. For information about emoji reaction values supported in AWS CodeCommit, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/how-to-commit-comment.html#emoji-reaction-table AWS CodeCommit User Guide> .
--
-- /Note:/ Consider using 'reactionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrReactionValue :: Lens.Lens' PutCommentReaction Lude.Text
pcrReactionValue = Lens.lens (reactionValue :: PutCommentReaction -> Lude.Text) (\s a -> s {reactionValue = a} :: PutCommentReaction)
{-# DEPRECATED pcrReactionValue "Use generic-lens or generic-optics with 'reactionValue' instead." #-}

instance Lude.AWSRequest PutCommentReaction where
  type Rs PutCommentReaction = PutCommentReactionResponse
  request = Req.postJSON codeCommitService
  response = Res.receiveNull PutCommentReactionResponse'

instance Lude.ToHeaders PutCommentReaction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.PutCommentReaction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutCommentReaction where
  toJSON PutCommentReaction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("commentId" Lude..= commentId),
            Lude.Just ("reactionValue" Lude..= reactionValue)
          ]
      )

instance Lude.ToPath PutCommentReaction where
  toPath = Lude.const "/"

instance Lude.ToQuery PutCommentReaction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutCommentReactionResponse' smart constructor.
data PutCommentReactionResponse = PutCommentReactionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutCommentReactionResponse' with the minimum fields required to make a request.
mkPutCommentReactionResponse ::
  PutCommentReactionResponse
mkPutCommentReactionResponse = PutCommentReactionResponse'
