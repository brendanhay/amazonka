{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateComment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of a comment.
module Network.AWS.CodeCommit.UpdateComment
  ( -- * Creating a request
    UpdateComment (..),
    mkUpdateComment,

    -- ** Request lenses
    ucContent,
    ucCommentId,

    -- * Destructuring the response
    UpdateCommentResponse (..),
    mkUpdateCommentResponse,

    -- ** Response lenses
    ucrsComment,
    ucrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateComment' smart constructor.
data UpdateComment = UpdateComment'
  { -- | The updated content to replace the existing content of the comment.
    content :: Lude.Text,
    -- | The system-generated ID of the comment you want to update. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
    commentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateComment' with the minimum fields required to make a request.
--
-- * 'content' - The updated content to replace the existing content of the comment.
-- * 'commentId' - The system-generated ID of the comment you want to update. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
mkUpdateComment ::
  -- | 'content'
  Lude.Text ->
  -- | 'commentId'
  Lude.Text ->
  UpdateComment
mkUpdateComment pContent_ pCommentId_ =
  UpdateComment' {content = pContent_, commentId = pCommentId_}

-- | The updated content to replace the existing content of the comment.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucContent :: Lens.Lens' UpdateComment Lude.Text
ucContent = Lens.lens (content :: UpdateComment -> Lude.Text) (\s a -> s {content = a} :: UpdateComment)
{-# DEPRECATED ucContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The system-generated ID of the comment you want to update. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCommentId :: Lens.Lens' UpdateComment Lude.Text
ucCommentId = Lens.lens (commentId :: UpdateComment -> Lude.Text) (\s a -> s {commentId = a} :: UpdateComment)
{-# DEPRECATED ucCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

instance Lude.AWSRequest UpdateComment where
  type Rs UpdateComment = UpdateCommentResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateCommentResponse'
            Lude.<$> (x Lude..?> "comment") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateComment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.UpdateComment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateComment where
  toJSON UpdateComment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("content" Lude..= content),
            Lude.Just ("commentId" Lude..= commentId)
          ]
      )

instance Lude.ToPath UpdateComment where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateComment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCommentResponse' smart constructor.
data UpdateCommentResponse = UpdateCommentResponse'
  { -- | Information about the updated comment.
    comment :: Lude.Maybe Comment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCommentResponse' with the minimum fields required to make a request.
--
-- * 'comment' - Information about the updated comment.
-- * 'responseStatus' - The response status code.
mkUpdateCommentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCommentResponse
mkUpdateCommentResponse pResponseStatus_ =
  UpdateCommentResponse'
    { comment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the updated comment.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsComment :: Lens.Lens' UpdateCommentResponse (Lude.Maybe Comment)
ucrsComment = Lens.lens (comment :: UpdateCommentResponse -> Lude.Maybe Comment) (\s a -> s {comment = a} :: UpdateCommentResponse)
{-# DEPRECATED ucrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateCommentResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateCommentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCommentResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
