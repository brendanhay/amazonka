{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetCommentsForComparedCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about comments made on the comparison between two commits.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetCommentsForComparedCommit
  ( -- * Creating a request
    GetCommentsForComparedCommit (..),
    mkGetCommentsForComparedCommit,

    -- ** Request lenses
    gcfccNextToken,
    gcfccBeforeCommitId,
    gcfccMaxResults,
    gcfccRepositoryName,
    gcfccAfterCommitId,

    -- * Destructuring the response
    GetCommentsForComparedCommitResponse (..),
    mkGetCommentsForComparedCommitResponse,

    -- ** Response lenses
    gcfccrsCommentsForComparedCommitData,
    gcfccrsNextToken,
    gcfccrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCommentsForComparedCommit' smart constructor.
data GetCommentsForComparedCommit = GetCommentsForComparedCommit'
  { nextToken ::
      Lude.Maybe Lude.Text,
    beforeCommitId ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    repositoryName :: Lude.Text,
    afterCommitId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommentsForComparedCommit' with the minimum fields required to make a request.
--
-- * 'afterCommitId' - To establish the directionality of the comparison, the full commit ID of the after commit.
-- * 'beforeCommitId' - To establish the directionality of the comparison, the full commit ID of the before commit.
-- * 'maxResults' - A non-zero, non-negative integer used to limit the number of returned results. The default is 100 comments, but you can configure up to 500.
-- * 'nextToken' - An enumeration token that when provided in a request, returns the next batch of the results.
-- * 'repositoryName' - The name of the repository where you want to compare commits.
mkGetCommentsForComparedCommit ::
  -- | 'repositoryName'
  Lude.Text ->
  -- | 'afterCommitId'
  Lude.Text ->
  GetCommentsForComparedCommit
mkGetCommentsForComparedCommit pRepositoryName_ pAfterCommitId_ =
  GetCommentsForComparedCommit'
    { nextToken = Lude.Nothing,
      beforeCommitId = Lude.Nothing,
      maxResults = Lude.Nothing,
      repositoryName = pRepositoryName_,
      afterCommitId = pAfterCommitId_
    }

-- | An enumeration token that when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfccNextToken :: Lens.Lens' GetCommentsForComparedCommit (Lude.Maybe Lude.Text)
gcfccNextToken = Lens.lens (nextToken :: GetCommentsForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCommentsForComparedCommit)
{-# DEPRECATED gcfccNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | To establish the directionality of the comparison, the full commit ID of the before commit.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfccBeforeCommitId :: Lens.Lens' GetCommentsForComparedCommit (Lude.Maybe Lude.Text)
gcfccBeforeCommitId = Lens.lens (beforeCommitId :: GetCommentsForComparedCommit -> Lude.Maybe Lude.Text) (\s a -> s {beforeCommitId = a} :: GetCommentsForComparedCommit)
{-# DEPRECATED gcfccBeforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is 100 comments, but you can configure up to 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfccMaxResults :: Lens.Lens' GetCommentsForComparedCommit (Lude.Maybe Lude.Int)
gcfccMaxResults = Lens.lens (maxResults :: GetCommentsForComparedCommit -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetCommentsForComparedCommit)
{-# DEPRECATED gcfccMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the repository where you want to compare commits.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfccRepositoryName :: Lens.Lens' GetCommentsForComparedCommit Lude.Text
gcfccRepositoryName = Lens.lens (repositoryName :: GetCommentsForComparedCommit -> Lude.Text) (\s a -> s {repositoryName = a} :: GetCommentsForComparedCommit)
{-# DEPRECATED gcfccRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | To establish the directionality of the comparison, the full commit ID of the after commit.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfccAfterCommitId :: Lens.Lens' GetCommentsForComparedCommit Lude.Text
gcfccAfterCommitId = Lens.lens (afterCommitId :: GetCommentsForComparedCommit -> Lude.Text) (\s a -> s {afterCommitId = a} :: GetCommentsForComparedCommit)
{-# DEPRECATED gcfccAfterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead." #-}

instance Page.AWSPager GetCommentsForComparedCommit where
  page rq rs
    | Page.stop (rs Lens.^. gcfccrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gcfccrsCommentsForComparedCommitData) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gcfccNextToken Lens..~ rs Lens.^. gcfccrsNextToken

instance Lude.AWSRequest GetCommentsForComparedCommit where
  type
    Rs GetCommentsForComparedCommit =
      GetCommentsForComparedCommitResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCommentsForComparedCommitResponse'
            Lude.<$> (x Lude..?> "commentsForComparedCommitData" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCommentsForComparedCommit where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.GetCommentsForComparedCommit" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCommentsForComparedCommit where
  toJSON GetCommentsForComparedCommit' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("beforeCommitId" Lude..=) Lude.<$> beforeCommitId,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            Lude.Just ("afterCommitId" Lude..= afterCommitId)
          ]
      )

instance Lude.ToPath GetCommentsForComparedCommit where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCommentsForComparedCommit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCommentsForComparedCommitResponse' smart constructor.
data GetCommentsForComparedCommitResponse = GetCommentsForComparedCommitResponse'
  { commentsForComparedCommitData ::
      Lude.Maybe
        [CommentsForComparedCommit],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCommentsForComparedCommitResponse' with the minimum fields required to make a request.
--
-- * 'commentsForComparedCommitData' - A list of comment objects on the compared commit.
-- * 'nextToken' - An enumeration token that can be used in a request to return the next batch of the results.
-- * 'responseStatus' - The response status code.
mkGetCommentsForComparedCommitResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCommentsForComparedCommitResponse
mkGetCommentsForComparedCommitResponse pResponseStatus_ =
  GetCommentsForComparedCommitResponse'
    { commentsForComparedCommitData =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of comment objects on the compared commit.
--
-- /Note:/ Consider using 'commentsForComparedCommitData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfccrsCommentsForComparedCommitData :: Lens.Lens' GetCommentsForComparedCommitResponse (Lude.Maybe [CommentsForComparedCommit])
gcfccrsCommentsForComparedCommitData = Lens.lens (commentsForComparedCommitData :: GetCommentsForComparedCommitResponse -> Lude.Maybe [CommentsForComparedCommit]) (\s a -> s {commentsForComparedCommitData = a} :: GetCommentsForComparedCommitResponse)
{-# DEPRECATED gcfccrsCommentsForComparedCommitData "Use generic-lens or generic-optics with 'commentsForComparedCommitData' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfccrsNextToken :: Lens.Lens' GetCommentsForComparedCommitResponse (Lude.Maybe Lude.Text)
gcfccrsNextToken = Lens.lens (nextToken :: GetCommentsForComparedCommitResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCommentsForComparedCommitResponse)
{-# DEPRECATED gcfccrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfccrsResponseStatus :: Lens.Lens' GetCommentsForComparedCommitResponse Lude.Int
gcfccrsResponseStatus = Lens.lens (responseStatus :: GetCommentsForComparedCommitResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCommentsForComparedCommitResponse)
{-# DEPRECATED gcfccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
