{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetCommentReactions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reactions to a specified comment ID. Reactions from users who have been deleted will not be included in the count.
module Network.AWS.CodeCommit.GetCommentReactions
  ( -- * Creating a request
    GetCommentReactions (..),
    mkGetCommentReactions,

    -- ** Request lenses
    gcrNextToken,
    gcrReactionUserARN,
    gcrCommentId,
    gcrMaxResults,

    -- * Destructuring the response
    GetCommentReactionsResponse (..),
    mkGetCommentReactionsResponse,

    -- ** Response lenses
    gcrrsReactionsForComment,
    gcrrsNextToken,
    gcrrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCommentReactions' smart constructor.
data GetCommentReactions = GetCommentReactions'
  { -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Optional. The Amazon Resource Name (ARN) of the user or identity for which you want to get reaction information.
    reactionUserARN :: Lude.Maybe Lude.Text,
    -- | The ID of the comment for which you want to get reactions information.
    commentId :: Lude.Text,
    -- | A non-zero, non-negative integer used to limit the number of returned results. The default is the same as the allowed maximum, 1,000.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommentReactions' with the minimum fields required to make a request.
--
-- * 'nextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
-- * 'reactionUserARN' - Optional. The Amazon Resource Name (ARN) of the user or identity for which you want to get reaction information.
-- * 'commentId' - The ID of the comment for which you want to get reactions information.
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results. The default is the same as the allowed maximum, 1,000.
mkGetCommentReactions ::
  -- | 'commentId'
  Lude.Text ->
  GetCommentReactions
mkGetCommentReactions pCommentId_ =
  GetCommentReactions'
    { nextToken = Lude.Nothing,
      reactionUserARN = Lude.Nothing,
      commentId = pCommentId_,
      maxResults = Lude.Nothing
    }

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrNextToken :: Lens.Lens' GetCommentReactions (Lude.Maybe Lude.Text)
gcrNextToken = Lens.lens (nextToken :: GetCommentReactions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCommentReactions)
{-# DEPRECATED gcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. The Amazon Resource Name (ARN) of the user or identity for which you want to get reaction information.
--
-- /Note:/ Consider using 'reactionUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrReactionUserARN :: Lens.Lens' GetCommentReactions (Lude.Maybe Lude.Text)
gcrReactionUserARN = Lens.lens (reactionUserARN :: GetCommentReactions -> Lude.Maybe Lude.Text) (\s a -> s {reactionUserARN = a} :: GetCommentReactions)
{-# DEPRECATED gcrReactionUserARN "Use generic-lens or generic-optics with 'reactionUserARN' instead." #-}

-- | The ID of the comment for which you want to get reactions information.
--
-- /Note:/ Consider using 'commentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrCommentId :: Lens.Lens' GetCommentReactions Lude.Text
gcrCommentId = Lens.lens (commentId :: GetCommentReactions -> Lude.Text) (\s a -> s {commentId = a} :: GetCommentReactions)
{-# DEPRECATED gcrCommentId "Use generic-lens or generic-optics with 'commentId' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is the same as the allowed maximum, 1,000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrMaxResults :: Lens.Lens' GetCommentReactions (Lude.Maybe Lude.Int)
gcrMaxResults = Lens.lens (maxResults :: GetCommentReactions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetCommentReactions)
{-# DEPRECATED gcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest GetCommentReactions where
  type Rs GetCommentReactions = GetCommentReactionsResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCommentReactionsResponse'
            Lude.<$> (x Lude..?> "reactionsForComment" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCommentReactions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetCommentReactions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCommentReactions where
  toJSON GetCommentReactions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("reactionUserArn" Lude..=) Lude.<$> reactionUserARN,
            Lude.Just ("commentId" Lude..= commentId),
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetCommentReactions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCommentReactions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCommentReactionsResponse' smart constructor.
data GetCommentReactionsResponse = GetCommentReactionsResponse'
  { -- | An array of reactions to the specified comment.
    reactionsForComment :: [ReactionForComment],
    -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommentReactionsResponse' with the minimum fields required to make a request.
--
-- * 'reactionsForComment' - An array of reactions to the specified comment.
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'responseStatus' - The response status code.
mkGetCommentReactionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCommentReactionsResponse
mkGetCommentReactionsResponse pResponseStatus_ =
  GetCommentReactionsResponse'
    { reactionsForComment = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of reactions to the specified comment.
--
-- /Note:/ Consider using 'reactionsForComment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsReactionsForComment :: Lens.Lens' GetCommentReactionsResponse [ReactionForComment]
gcrrsReactionsForComment = Lens.lens (reactionsForComment :: GetCommentReactionsResponse -> [ReactionForComment]) (\s a -> s {reactionsForComment = a} :: GetCommentReactionsResponse)
{-# DEPRECATED gcrrsReactionsForComment "Use generic-lens or generic-optics with 'reactionsForComment' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsNextToken :: Lens.Lens' GetCommentReactionsResponse (Lude.Maybe Lude.Text)
gcrrsNextToken = Lens.lens (nextToken :: GetCommentReactionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCommentReactionsResponse)
{-# DEPRECATED gcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCommentReactionsResponse Lude.Int
gcrrsResponseStatus = Lens.lens (responseStatus :: GetCommentReactionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCommentReactionsResponse)
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
