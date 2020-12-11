{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListExperiments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the experiments in your account. The list can be filtered to show only experiments that were created in a specific time range. The list can be sorted by experiment name or creation time.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListExperiments
  ( -- * Creating a request
    ListExperiments (..),
    mkListExperiments,

    -- ** Request lenses
    leCreatedAfter,
    leNextToken,
    leSortOrder,
    leMaxResults,
    leCreatedBefore,
    leSortBy,

    -- * Destructuring the response
    ListExperimentsResponse (..),
    mkListExperimentsResponse,

    -- ** Response lenses
    lersExperimentSummaries,
    lersNextToken,
    lersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListExperiments' smart constructor.
data ListExperiments = ListExperiments'
  { createdAfter ::
      Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    maxResults :: Lude.Maybe Lude.Natural,
    createdBefore :: Lude.Maybe Lude.Timestamp,
    sortBy :: Lude.Maybe SortExperimentsBy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListExperiments' with the minimum fields required to make a request.
--
-- * 'createdAfter' - A filter that returns only experiments created after the specified time.
-- * 'createdBefore' - A filter that returns only experiments created before the specified time.
-- * 'maxResults' - The maximum number of experiments to return in the response. The default value is 10.
-- * 'nextToken' - If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
-- * 'sortBy' - The property used to sort results. The default value is @CreationTime@ .
-- * 'sortOrder' - The sort order. The default value is @Descending@ .
mkListExperiments ::
  ListExperiments
mkListExperiments =
  ListExperiments'
    { createdAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      maxResults = Lude.Nothing,
      createdBefore = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A filter that returns only experiments created after the specified time.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreatedAfter :: Lens.Lens' ListExperiments (Lude.Maybe Lude.Timestamp)
leCreatedAfter = Lens.lens (createdAfter :: ListExperiments -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAfter = a} :: ListExperiments)
{-# DEPRECATED leCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leNextToken :: Lens.Lens' ListExperiments (Lude.Maybe Lude.Text)
leNextToken = Lens.lens (nextToken :: ListExperiments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExperiments)
{-# DEPRECATED leNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leSortOrder :: Lens.Lens' ListExperiments (Lude.Maybe SortOrder)
leSortOrder = Lens.lens (sortOrder :: ListExperiments -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListExperiments)
{-# DEPRECATED leSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of experiments to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMaxResults :: Lens.Lens' ListExperiments (Lude.Maybe Lude.Natural)
leMaxResults = Lens.lens (maxResults :: ListExperiments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListExperiments)
{-# DEPRECATED leMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter that returns only experiments created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCreatedBefore :: Lens.Lens' ListExperiments (Lude.Maybe Lude.Timestamp)
leCreatedBefore = Lens.lens (createdBefore :: ListExperiments -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdBefore = a} :: ListExperiments)
{-# DEPRECATED leCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leSortBy :: Lens.Lens' ListExperiments (Lude.Maybe SortExperimentsBy)
leSortBy = Lens.lens (sortBy :: ListExperiments -> Lude.Maybe SortExperimentsBy) (\s a -> s {sortBy = a} :: ListExperiments)
{-# DEPRECATED leSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListExperiments where
  page rq rs
    | Page.stop (rs Lens.^. lersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lersExperimentSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& leNextToken Lens..~ rs Lens.^. lersNextToken

instance Lude.AWSRequest ListExperiments where
  type Rs ListExperiments = ListExperimentsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListExperimentsResponse'
            Lude.<$> (x Lude..?> "ExperimentSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListExperiments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListExperiments" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListExperiments where
  toJSON ListExperiments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreatedAfter" Lude..=) Lude.<$> createdAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("CreatedBefore" Lude..=) Lude.<$> createdBefore,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListExperiments where
  toPath = Lude.const "/"

instance Lude.ToQuery ListExperiments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListExperimentsResponse' smart constructor.
data ListExperimentsResponse = ListExperimentsResponse'
  { experimentSummaries ::
      Lude.Maybe [ExperimentSummary],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListExperimentsResponse' with the minimum fields required to make a request.
--
-- * 'experimentSummaries' - A list of the summaries of your experiments.
-- * 'nextToken' - A token for getting the next set of experiments, if there are any.
-- * 'responseStatus' - The response status code.
mkListExperimentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListExperimentsResponse
mkListExperimentsResponse pResponseStatus_ =
  ListExperimentsResponse'
    { experimentSummaries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the summaries of your experiments.
--
-- /Note:/ Consider using 'experimentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersExperimentSummaries :: Lens.Lens' ListExperimentsResponse (Lude.Maybe [ExperimentSummary])
lersExperimentSummaries = Lens.lens (experimentSummaries :: ListExperimentsResponse -> Lude.Maybe [ExperimentSummary]) (\s a -> s {experimentSummaries = a} :: ListExperimentsResponse)
{-# DEPRECATED lersExperimentSummaries "Use generic-lens or generic-optics with 'experimentSummaries' instead." #-}

-- | A token for getting the next set of experiments, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersNextToken :: Lens.Lens' ListExperimentsResponse (Lude.Maybe Lude.Text)
lersNextToken = Lens.lens (nextToken :: ListExperimentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExperimentsResponse)
{-# DEPRECATED lersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lersResponseStatus :: Lens.Lens' ListExperimentsResponse Lude.Int
lersResponseStatus = Lens.lens (responseStatus :: ListExperimentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListExperimentsResponse)
{-# DEPRECATED lersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
