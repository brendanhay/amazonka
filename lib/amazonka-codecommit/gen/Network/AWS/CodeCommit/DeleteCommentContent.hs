{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteCommentContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content of a comment made on a change, file, or commit in a repository.
module Network.AWS.CodeCommit.DeleteCommentContent
  ( -- * Creating a request
    DeleteCommentContent (..),
    mkDeleteCommentContent,

    -- ** Request lenses
    dccCommentId,

    -- * Destructuring the response
    DeleteCommentContentResponse (..),
    mkDeleteCommentContentResponse,

    -- ** Response lenses
    dccrsComment,
    dccrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCommentContent' smart constructor.
newtype DeleteCommentContent = DeleteCommentContent'
  { commentId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCommentContent' with the minimum fields required to make a request.
--
-- * 'commentId' - The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
mkDeleteCommentContent ::
  -- | 'commentId'
  Lude.Text ->
  DeleteCommentContent
mkDeleteCommentContent pCommentId_ =
  DeleteCommentContent' {commentId = pCommentId_}

-- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccCommentId :: Lens.Lens' DeleteCommentContent Lude.Text
dccCommentId = Lens.lens (commentId :: DeleteCommentContent -> Lude.Text) (\s a -> s {commentId = a} :: DeleteCommentContent)
{-# DEPRECATED dccCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

instance Lude.AWSRequest DeleteCommentContent where
  type Rs DeleteCommentContent = DeleteCommentContentResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteCommentContentResponse'
            Lude.<$> (x Lude..?> "comment") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCommentContent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.DeleteCommentContent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCommentContent where
  toJSON DeleteCommentContent' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("commentId" Lude..= commentId)])

instance Lude.ToPath DeleteCommentContent where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCommentContent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCommentContentResponse' smart constructor.
data DeleteCommentContentResponse = DeleteCommentContentResponse'
  { comment ::
      Lude.Maybe Comment,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCommentContentResponse' with the minimum fields required to make a request.
--
-- * 'comment' - Information about the comment you just deleted.
-- * 'responseStatus' - The response status code.
mkDeleteCommentContentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCommentContentResponse
mkDeleteCommentContentResponse pResponseStatus_ =
  DeleteCommentContentResponse'
    { comment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the comment you just deleted.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsComment :: Lens.Lens' DeleteCommentContentResponse (Lude.Maybe Comment)
dccrsComment = Lens.lens (comment :: DeleteCommentContentResponse -> Lude.Maybe Comment) (\s a -> s {comment = a} :: DeleteCommentContentResponse)
{-# DEPRECATED dccrsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsResponseStatus :: Lens.Lens' DeleteCommentContentResponse Lude.Int
dccrsResponseStatus = Lens.lens (responseStatus :: DeleteCommentContentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCommentContentResponse)
{-# DEPRECATED dccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
