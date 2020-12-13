{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    laNameContains,
    laCreationTimeAfter,
    laNextToken,
    laSortOrder,
    laCreationTimeBefore,
    laMaxResults,
    laSortBy,

    -- * Destructuring the response
    ListAlgorithmsResponse (..),
    mkListAlgorithmsResponse,

    -- ** Response lenses
    lasrsAlgorithmSummaryList,
    lasrsNextToken,
    lasrsResponseStatus,
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
  { -- | A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only algorithms created after the specified time (timestamp).
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for the results. The default is @Ascending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only algorithms created before the specified time (timestamp).
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | The maximum number of algorithms to return in the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The parameter by which to sort the results. The default is @CreationTime@ .
    sortBy :: Lude.Maybe AlgorithmSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAlgorithms' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
-- * 'creationTimeAfter' - A filter that returns only algorithms created after the specified time (timestamp).
-- * 'nextToken' - If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
-- * 'sortOrder' - The sort order for the results. The default is @Ascending@ .
-- * 'creationTimeBefore' - A filter that returns only algorithms created before the specified time (timestamp).
-- * 'maxResults' - The maximum number of algorithms to return in the response.
-- * 'sortBy' - The parameter by which to sort the results. The default is @CreationTime@ .
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
laNameContains :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Text)
laNameContains = Lens.lens (nameContains :: ListAlgorithms -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListAlgorithms)
{-# DEPRECATED laNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only algorithms created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laCreationTimeAfter :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Timestamp)
laCreationTimeAfter = Lens.lens (creationTimeAfter :: ListAlgorithms -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListAlgorithms)
{-# DEPRECATED laCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Text)
laNextToken = Lens.lens (nextToken :: ListAlgorithms -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAlgorithms)
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for the results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSortOrder :: Lens.Lens' ListAlgorithms (Lude.Maybe SortOrder)
laSortOrder = Lens.lens (sortOrder :: ListAlgorithms -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListAlgorithms)
{-# DEPRECATED laSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only algorithms created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laCreationTimeBefore :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Timestamp)
laCreationTimeBefore = Lens.lens (creationTimeBefore :: ListAlgorithms -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListAlgorithms)
{-# DEPRECATED laCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of algorithms to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListAlgorithms (Lude.Maybe Lude.Natural)
laMaxResults = Lens.lens (maxResults :: ListAlgorithms -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAlgorithms)
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter by which to sort the results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laSortBy :: Lens.Lens' ListAlgorithms (Lude.Maybe AlgorithmSortBy)
laSortBy = Lens.lens (sortBy :: ListAlgorithms -> Lude.Maybe AlgorithmSortBy) (\s a -> s {sortBy = a} :: ListAlgorithms)
{-# DEPRECATED laSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListAlgorithms where
  page rq rs
    | Page.stop (rs Lens.^. lasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lasrsAlgorithmSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laNextToken Lens..~ rs Lens.^. lasrsNextToken

instance Lude.AWSRequest ListAlgorithms where
  type Rs ListAlgorithms = ListAlgorithmsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAlgorithmsResponse'
            Lude.<$> (x Lude..?> "AlgorithmSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
    algorithmSummaryList :: [AlgorithmSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
    { algorithmSummaryList = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
--
-- /Note:/ Consider using 'algorithmSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrsAlgorithmSummaryList :: Lens.Lens' ListAlgorithmsResponse [AlgorithmSummary]
lasrsAlgorithmSummaryList = Lens.lens (algorithmSummaryList :: ListAlgorithmsResponse -> [AlgorithmSummary]) (\s a -> s {algorithmSummaryList = a} :: ListAlgorithmsResponse)
{-# DEPRECATED lasrsAlgorithmSummaryList "Use generic-lens or generic-optics with 'algorithmSummaryList' instead." #-}

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
