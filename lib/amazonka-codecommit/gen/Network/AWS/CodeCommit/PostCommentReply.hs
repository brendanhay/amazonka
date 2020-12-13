{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PostCommentReply
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment in reply to an existing comment on a comparison between commits or a pull request.
module Network.AWS.CodeCommit.PostCommentReply
  ( -- * Creating a request
    PostCommentReply (..),
    mkPostCommentReply,

    -- ** Request lenses
    pcrContent,
    pcrClientRequestToken,
    pcrInReplyTo,

    -- * Destructuring the response
    PostCommentReplyResponse (..),
    mkPostCommentReplyResponse,

    -- ** Response lenses
    pcrrsComment,
    pcrrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPostCommentReply' smart constructor.
data PostCommentReply = PostCommentReply'
  { -- | The contents of your reply to a comment.
    content :: Lude.Text,
    -- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The system-generated ID of the comment to which you want to reply. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
    inReplyTo :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostCommentReply' with the minimum fields required to make a request.
--
-- * 'content' - The contents of your reply to a comment.
-- * 'clientRequestToken' - A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
-- * 'inReplyTo' - The system-generated ID of the comment to which you want to reply. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
mkPostCommentReply ::
  -- | 'content'
  Lude.Text ->
  -- | 'inReplyTo'
  Lude.Text ->
  PostCommentReply
mkPostCommentReply pContent_ pInReplyTo_ =
  PostCommentReply'
    { content = pContent_,
      clientRequestToken = Lude.Nothing,
      inReplyTo = pInReplyTo_
    }

-- | The contents of your reply to a comment.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrContent :: Lens.Lens' PostCommentReply Lude.Text
pcrContent = Lens.lens (content :: PostCommentReply -> Lude.Text) (\s a -> s {content = a} :: PostCommentReply)
{-# DEPRECATED pcrContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrClientRequestToken :: Lens.Lens' PostCommentReply (Lude.Maybe Lude.Text)
pcrClientRequestToken = Lens.lens (clientRequestToken :: PostCommentReply -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: PostCommentReply)
{-# DEPRECATED pcrClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The system-generated ID of the comment to which you want to reply. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- /Note:/ Consider using 'inReplyTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrInReplyTo :: Lens.Lens' PostCommentReply Lude.Text
pcrInReplyTo = Lens.lens (inReplyTo :: PostCommentReply -> Lude.Text) (\s a -> s {inReplyTo = a} :: PostCommentReply)
{-# DEPRECATED pcrInReplyTo "Use generic-lens or generic-optics with 'inReplyTo' instead." #-}

instance Lude.AWSRequest PostCommentReply where
  type Rs PostCommentReply = PostCommentReplyResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          PostCommentReplyResponse'
            Lude.<$> (x Lude..?> "comment") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PostCommentReply where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.PostCommentReply" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PostCommentReply where
  toJSON PostCommentReply' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("content" Lude..= content),
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("inReplyTo" Lude..= inReplyTo)
          ]
      )

instance Lude.ToPath PostCommentReply where
  toPath = Lude.const "/"

instance Lude.ToQuery PostCommentReply where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPostCommentReplyResponse' smart constructor.
data PostCommentReplyResponse = PostCommentReplyResponse'
  { -- | Information about the reply to a comment.
    comment :: Lude.Maybe Comment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostCommentReplyResponse' with the minimum fields required to make a request.
--
-- * 'comment' - Information about the reply to a comment.
-- * 'responseStatus' - The response status code.
mkPostCommentReplyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PostCommentReplyResponse
mkPostCommentReplyResponse pResponseStatus_ =
  PostCommentReplyResponse'
    { comment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the reply to a comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsComment :: Lens.Lens' PostCommentReplyResponse (Lude.Maybe Comment)
pcrrsComment = Lens.lens (comment :: PostCommentReplyResponse -> Lude.Maybe Comment) (\s a -> s {comment = a} :: PostCommentReplyResponse)
{-# DEPRECATED pcrrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsResponseStatus :: Lens.Lens' PostCommentReplyResponse Lude.Int
pcrrsResponseStatus = Lens.lens (responseStatus :: PostCommentReplyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PostCommentReplyResponse)
{-# DEPRECATED pcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
