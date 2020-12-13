{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    lesCreatedAfter,
    lesNextToken,
    lesSortOrder,
    lesMaxResults,
    lesCreatedBefore,
    lesSortBy,

    -- * Destructuring the response
    ListExperimentsResponse (..),
    mkListExperimentsResponse,

    -- ** Response lenses
    lrsExperimentSummaries,
    lrsNextToken,
    lrsResponseStatus,
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
  { -- | A filter that returns only experiments created after the specified time.
    createdAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order. The default value is @Descending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | The maximum number of experiments to return in the response. The default value is 10.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | A filter that returns only experiments created before the specified time.
    createdBefore :: Lude.Maybe Lude.Timestamp,
    -- | The property used to sort results. The default value is @CreationTime@ .
    sortBy :: Lude.Maybe SortExperimentsBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListExperiments' with the minimum fields required to make a request.
--
-- * 'createdAfter' - A filter that returns only experiments created after the specified time.
-- * 'nextToken' - If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
-- * 'sortOrder' - The sort order. The default value is @Descending@ .
-- * 'maxResults' - The maximum number of experiments to return in the response. The default value is 10.
-- * 'createdBefore' - A filter that returns only experiments created before the specified time.
-- * 'sortBy' - The property used to sort results. The default value is @CreationTime@ .
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
lesCreatedAfter :: Lens.Lens' ListExperiments (Lude.Maybe Lude.Timestamp)
lesCreatedAfter = Lens.lens (createdAfter :: ListExperiments -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAfter = a} :: ListExperiments)
{-# DEPRECATED lesCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | If the previous call to @ListExperiments@ didn't return the full set of experiments, the call returns a token for getting the next set of experiments.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNextToken :: Lens.Lens' ListExperiments (Lude.Maybe Lude.Text)
lesNextToken = Lens.lens (nextToken :: ListExperiments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExperiments)
{-# DEPRECATED lesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesSortOrder :: Lens.Lens' ListExperiments (Lude.Maybe SortOrder)
lesSortOrder = Lens.lens (sortOrder :: ListExperiments -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListExperiments)
{-# DEPRECATED lesSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum number of experiments to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesMaxResults :: Lens.Lens' ListExperiments (Lude.Maybe Lude.Natural)
lesMaxResults = Lens.lens (maxResults :: ListExperiments -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListExperiments)
{-# DEPRECATED lesMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter that returns only experiments created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesCreatedBefore :: Lens.Lens' ListExperiments (Lude.Maybe Lude.Timestamp)
lesCreatedBefore = Lens.lens (createdBefore :: ListExperiments -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdBefore = a} :: ListExperiments)
{-# DEPRECATED lesCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesSortBy :: Lens.Lens' ListExperiments (Lude.Maybe SortExperimentsBy)
lesSortBy = Lens.lens (sortBy :: ListExperiments -> Lude.Maybe SortExperimentsBy) (\s a -> s {sortBy = a} :: ListExperiments)
{-# DEPRECATED lesSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListExperiments where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsExperimentSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lesNextToken Lens..~ rs Lens.^. lrsNextToken

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
  { -- | A list of the summaries of your experiments.
    experimentSummaries :: Lude.Maybe [ExperimentSummary],
    -- | A token for getting the next set of experiments, if there are any.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
lrsExperimentSummaries :: Lens.Lens' ListExperimentsResponse (Lude.Maybe [ExperimentSummary])
lrsExperimentSummaries = Lens.lens (experimentSummaries :: ListExperimentsResponse -> Lude.Maybe [ExperimentSummary]) (\s a -> s {experimentSummaries = a} :: ListExperimentsResponse)
{-# DEPRECATED lrsExperimentSummaries "Use generic-lens or generic-optics with 'experimentSummaries' instead." #-}

-- | A token for getting the next set of experiments, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListExperimentsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListExperimentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListExperimentsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListExperimentsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListExperimentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListExperimentsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
