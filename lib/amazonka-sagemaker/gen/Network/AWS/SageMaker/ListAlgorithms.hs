{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListAlgorithms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the machine learning algorithms that have been created.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAlgorithms
  ( -- * Creating a request
    ListAlgorithms (..),
    mkListAlgorithms,

    -- ** Request lenses
    lNameContains,
    lCreationTimeAfter,
    lNextToken,
    lSortOrder,
    lCreationTimeBefore,
    lMaxResults,
    lSortBy,

    -- * Destructuring the response
    ListAlgorithmsResponse (..),
    mkListAlgorithmsResponse,

    -- ** Response lenses
    lasrsNextToken,
    lasrsResponseStatus,
    lasrsAlgorithmSummaryList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListAlgorithms' smart constructor.
data ListAlgorithms = ListAlgorithms'
  { nameContains ::
      Lude.Maybe Lude.Text,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe AlgorithmSortBy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAlgorithms' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only algorithms created after the specified time (timestamp).
-- * 'creationTimeBefore' - A filter that returns only algorithms created before the specified time (timestamp).
-- * 'maxResults' - The maximum number of algorithms to return in the response.
-- * 'nameContains' - A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
-- * 'nextToken' - If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
-- * 'sortBy' - The parameter by which to sort the results. The default is @CreationTime@ .
-- * 'sortOrder' - The sort order for the results. The default is @Ascending@ .
mkListAlgorithms ::
  ListAlgorithms
mkListAlgorithms =
  ListAlgorithms'
    { nameContains = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNameContains :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Text)
lNameContains = Lens.lens (nameContains :: ListAlgorithms -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListAlgorithms)
{-# DEPRECATED lNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only algorithms created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCreationTimeAfter :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Timestamp)
lCreationTimeAfter = Lens.lens (creationTimeAfter :: ListAlgorithms -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListAlgorithms)
{-# DEPRECATED lCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListAlgorithms -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAlgorithms)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for the results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSortOrder :: Lens.Lens' ListAlgorithms (Lude.Maybe SortOrder)
lSortOrder = Lens.lens (sortOrder :: ListAlgorithms -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListAlgorithms)
{-# DEPRECATED lSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only algorithms created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCreationTimeBefore :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Timestamp)
lCreationTimeBefore = Lens.lens (creationTimeBefore :: ListAlgorithms -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListAlgorithms)
{-# DEPRECATED lCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of algorithms to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListAlgorithms -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAlgorithms)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter by which to sort the results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSortBy :: Lens.Lens' ListAlgorithms (Lude.Maybe AlgorithmSortBy)
lSortBy = Lens.lens (sortBy :: ListAlgorithms -> Lude.Maybe AlgorithmSortBy) (\s a -> s {sortBy = a} :: ListAlgorithms)
{-# DEPRECATED lSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListAlgorithms where
  page rq rs
    | Page.stop (rs Lens.^. lasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lasrsAlgorithmSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lasrsNextToken

instance Lude.AWSRequest ListAlgorithms where
  type Rs ListAlgorithms = ListAlgorithmsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAlgorithmsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "AlgorithmSummaryList" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListAlgorithms where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListAlgorithms" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAlgorithms where
  toJSON ListAlgorithms' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListAlgorithms where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAlgorithms where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAlgorithmsResponse' smart constructor.
data ListAlgorithmsResponse = ListAlgorithmsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    algorithmSummaryList :: [AlgorithmSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAlgorithmsResponse' with the minimum fields required to make a request.
--
-- * 'algorithmSummaryList' - >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
-- * 'responseStatus' - The response status code.
mkListAlgorithmsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAlgorithmsResponse
mkListAlgorithmsResponse pResponseStatus_ =
  ListAlgorithmsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      algorithmSummaryList = Lude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsNextToken :: Lens.Lens' ListAlgorithmsResponse (Lude.Maybe Lude.Text)
lasrsNextToken = Lens.lens (nextToken :: ListAlgorithmsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAlgorithmsResponse)
{-# DEPRECATED lasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsResponseStatus :: Lens.Lens' ListAlgorithmsResponse Lude.Int
lasrsResponseStatus = Lens.lens (responseStatus :: ListAlgorithmsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAlgorithmsResponse)
{-# DEPRECATED lasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
--
-- /Note:/ Consider using 'algorithmSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsAlgorithmSummaryList :: Lens.Lens' ListAlgorithmsResponse [AlgorithmSummary]
lasrsAlgorithmSummaryList = Lens.lens (algorithmSummaryList :: ListAlgorithmsResponse -> [AlgorithmSummary]) (\s a -> s {algorithmSummaryList = a} :: ListAlgorithmsResponse)
{-# DEPRECATED lasrsAlgorithmSummaryList "Use generic-lens or generic-optics with 'algorithmSummaryList' instead." #-}
