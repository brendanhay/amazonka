{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the content of a comment made on a change, file, or commit in a repository.
module Network.AWS.CodeCommit.GetComment
  ( -- * Creating a request
    GetComment (..),
    mkGetComment,

    -- ** Request lenses
    gcCommentId,

    -- * Destructuring the response
    GetCommentResponse (..),
    mkGetCommentResponse,

    -- ** Response lenses
    gcrsComment,
    gcrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetComment' smart constructor.
newtype GetComment = GetComment'
  { -- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
    commentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComment' with the minimum fields required to make a request.
--
-- * 'commentId' - The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
mkGetComment ::
  -- | 'commentId'
  Lude.Text ->
  GetComment
mkGetComment pCommentId_ = GetComment' {commentId = pCommentId_}

-- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCommentId :: Lens.Lens' GetComment Lude.Text
gcCommentId = Lens.lens (commentId :: GetComment -> Lude.Text) (\s a -> s {commentId = a} :: GetComment)
{-# DEPRECATED gcCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

instance Lude.AWSRequest GetComment where
  type Rs GetComment = GetCommentResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCommentResponse'
            Lude.<$> (x Lude..?> "comment") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetComment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetComment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetComment where
  toJSON GetComment' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("commentId" Lude..= commentId)])

instance Lude.ToPath GetComment where
  toPath = Lude.const "/"

instance Lude.ToQuery GetComment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCommentResponse' smart constructor.
data GetCommentResponse = GetCommentResponse'
  { -- | The contents of the comment.
    comment :: Lude.Maybe Comment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommentResponse' with the minimum fields required to make a request.
--
-- * 'comment' - The contents of the comment.
-- * 'responseStatus' - The response status code.
mkGetCommentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCommentResponse
mkGetCommentResponse pResponseStatus_ =
  GetCommentResponse'
    { comment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The contents of the comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsComment :: Lens.Lens' GetCommentResponse (Lude.Maybe Comment)
gcrsComment = Lens.lens (comment :: GetCommentResponse -> Lude.Maybe Comment) (\s a -> s {comment = a} :: GetCommentResponse)
{-# DEPRECATED gcrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsResponseStatus :: Lens.Lens' GetCommentResponse Lude.Int
gcrsResponseStatus = Lens.lens (responseStatus :: GetCommentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCommentResponse)
{-# DEPRECATED gcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
