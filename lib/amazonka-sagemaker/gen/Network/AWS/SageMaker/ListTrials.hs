{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trials in your account. Specify an experiment name to limit the list to the trials that are part of that experiment. Specify a trial component name to limit the list to the trials that associated with that trial component. The list can be filtered to show only trials that were created in a specific time range. The list can be sorted by trial name or creation time.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrials
  ( -- * Creating a request
    ListTrials (..),
    mkListTrials,

    -- ** Request lenses
    ltCreatedAfter,
    ltExperimentName,
    ltNextToken,
    ltSortOrder,
    ltTrialComponentName,
    ltMaxResults,
    ltCreatedBefore,
    ltSortBy,

    -- * Destructuring the response
    ListTrialsResponse (..),
    mkListTrialsResponse,

    -- ** Response lenses
    ltsrsNextToken,
    ltsrsTrialSummaries,
    ltsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListTrials' smart constructor.
data ListTrials = ListTrials'
  { -- | A filter that returns only trials created after the specified time.
    createdAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only trials that are part of the specified experiment.
    experimentName :: Lude.Maybe Lude.Text,
    -- | If the previous call to @ListTrials@ didn't return the full set of trials, the call returns a token for getting the next set of trials.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order. The default value is @Descending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only trials that are associated with the specified trial component.
    trialComponentName :: Lude.Maybe Lude.Text,
    -- | The maximum number of trials to return in the response. The default value is 10.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | A filter that returns only trials created before the specified time.
    createdBefore :: Lude.Maybe Lude.Timestamp,
    -- | The property used to sort results. The default value is @CreationTime@ .
    sortBy :: Lude.Maybe SortTrialsBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrials' with the minimum fields required to make a request.
--
-- * 'createdAfter' - A filter that returns only trials created after the specified time.
-- * 'experimentName' - A filter that returns only trials that are part of the specified experiment.
-- * 'nextToken' - If the previous call to @ListTrials@ didn't return the full set of trials, the call returns a token for getting the next set of trials.
-- * 'sortOrder' - The sort order. The default value is @Descending@ .
-- * 'trialComponentName' - A filter that returns only trials that are associated with the specified trial component.
-- * 'maxResults' - The maximum number of trials to return in the response. The default value is 10.
-- * 'createdBefore' - A filter that returns only trials created before the specified time.
-- * 'sortBy' - The property used to sort results. The default value is @CreationTime@ .
mkListTrials ::
  ListTrials
mkListTrials =
  ListTrials'
    { createdAfter = Lude.Nothing,
      experimentName = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      trialComponentName = Lude.Nothing,
      maxResults = Lude.Nothing,
      createdBefore = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A filter that returns only trials created after the specified time.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCreatedAfter :: Lens.Lens' ListTrials (Lude.Maybe Lude.Timestamp)
ltCreatedAfter = Lens.lens (createdAfter :: ListTrials -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAfter = a} :: ListTrials)
{-# DEPRECATED ltCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | A filter that returns only trials that are part of the specified experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltExperimentName :: Lens.Lens' ListTrials (Lude.Maybe Lude.Text)
ltExperimentName = Lens.lens (experimentName :: ListTrials -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: ListTrials)
{-# DEPRECATED ltExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | If the previous call to @ListTrials@ didn't return the full set of trials, the call returns a token for getting the next set of trials.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTrials (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTrials -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrials)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltSortOrder :: Lens.Lens' ListTrials (Lude.Maybe SortOrder)
ltSortOrder = Lens.lens (sortOrder :: ListTrials -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListTrials)
{-# DEPRECATED ltSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only trials that are associated with the specified trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTrialComponentName :: Lens.Lens' ListTrials (Lude.Maybe Lude.Text)
ltTrialComponentName = Lens.lens (trialComponentName :: ListTrials -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentName = a} :: ListTrials)
{-# DEPRECATED ltTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The maximum number of trials to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTrials (Lude.Maybe Lude.Natural)
ltMaxResults = Lens.lens (maxResults :: ListTrials -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTrials)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter that returns only trials created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCreatedBefore :: Lens.Lens' ListTrials (Lude.Maybe Lude.Timestamp)
ltCreatedBefore = Lens.lens (createdBefore :: ListTrials -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdBefore = a} :: ListTrials)
{-# DEPRECATED ltCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltSortBy :: Lens.Lens' ListTrials (Lude.Maybe SortTrialsBy)
ltSortBy = Lens.lens (sortBy :: ListTrials -> Lude.Maybe SortTrialsBy) (\s a -> s {sortBy = a} :: ListTrials)
{-# DEPRECATED ltSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListTrials where
  page rq rs
    | Page.stop (rs Lens.^. ltsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltsrsTrialSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltsrsNextToken

instance Lude.AWSRequest ListTrials where
  type Rs ListTrials = ListTrialsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTrialsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "TrialSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTrials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListTrials" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTrials where
  toJSON ListTrials' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreatedAfter" Lude..=) Lude.<$> createdAfter,
            ("ExperimentName" Lude..=) Lude.<$> experimentName,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("TrialComponentName" Lude..=) Lude.<$> trialComponentName,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("CreatedBefore" Lude..=) Lude.<$> createdBefore,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListTrials where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTrials where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTrialsResponse' smart constructor.
data ListTrialsResponse = ListTrialsResponse'
  { -- | A token for getting the next set of trials, if there are any.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of the summaries of your trials.
    trialSummaries :: Lude.Maybe [TrialSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrialsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token for getting the next set of trials, if there are any.
-- * 'trialSummaries' - A list of the summaries of your trials.
-- * 'responseStatus' - The response status code.
mkListTrialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTrialsResponse
mkListTrialsResponse pResponseStatus_ =
  ListTrialsResponse'
    { nextToken = Lude.Nothing,
      trialSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A token for getting the next set of trials, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsrsNextToken :: Lens.Lens' ListTrialsResponse (Lude.Maybe Lude.Text)
ltsrsNextToken = Lens.lens (nextToken :: ListTrialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrialsResponse)
{-# DEPRECATED ltsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the summaries of your trials.
--
-- /Note:/ Consider using 'trialSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsrsTrialSummaries :: Lens.Lens' ListTrialsResponse (Lude.Maybe [TrialSummary])
ltsrsTrialSummaries = Lens.lens (trialSummaries :: ListTrialsResponse -> Lude.Maybe [TrialSummary]) (\s a -> s {trialSummaries = a} :: ListTrialsResponse)
{-# DEPRECATED ltsrsTrialSummaries "Use generic-lens or generic-optics with 'trialSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsrsResponseStatus :: Lens.Lens' ListTrialsResponse Lude.Int
ltsrsResponseStatus = Lens.lens (responseStatus :: ListTrialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrialsResponse)
{-# DEPRECATED ltsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
