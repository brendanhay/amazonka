{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrialComponents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trial components in your account. You can sort the list by trial component name or creation time. You can filter the list to show only components that were created in a specific time range. You can also filter on one of the following:
--
--
--     * @ExperimentName@
--
--
--     * @SourceArn@
--
--
--     * @TrialName@
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrialComponents
  ( -- * Creating a request
    ListTrialComponents (..),
    mkListTrialComponents,

    -- ** Request lenses
    ltcCreatedAfter,
    ltcSourceARN,
    ltcExperimentName,
    ltcNextToken,
    ltcSortOrder,
    ltcTrialName,
    ltcMaxResults,
    ltcCreatedBefore,
    ltcSortBy,

    -- * Destructuring the response
    ListTrialComponentsResponse (..),
    mkListTrialComponentsResponse,

    -- ** Response lenses
    ltcrsTrialComponentSummaries,
    ltcrsNextToken,
    ltcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListTrialComponents' smart constructor.
data ListTrialComponents = ListTrialComponents'
  { createdAfter ::
      Lude.Maybe Lude.Timestamp,
    sourceARN :: Lude.Maybe Lude.Text,
    experimentName :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    trialName :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    createdBefore :: Lude.Maybe Lude.Timestamp,
    sortBy :: Lude.Maybe SortTrialComponentsBy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrialComponents' with the minimum fields required to make a request.
--
-- * 'createdAfter' - A filter that returns only components created after the specified time.
-- * 'createdBefore' - A filter that returns only components created before the specified time.
-- * 'experimentName' - A filter that returns only components that are part of the specified experiment. If you specify @ExperimentName@ , you can't filter by @SourceArn@ or @TrialName@ .
-- * 'maxResults' - The maximum number of components to return in the response. The default value is 10.
-- * 'nextToken' - If the previous call to @ListTrialComponents@ didn't return the full set of components, the call returns a token for getting the next set of components.
-- * 'sortBy' - The property used to sort results. The default value is @CreationTime@ .
-- * 'sortOrder' - The sort order. The default value is @Descending@ .
-- * 'sourceARN' - A filter that returns only components that have the specified source Amazon Resource Name (ARN). If you specify @SourceArn@ , you can't filter by @ExperimentName@ or @TrialName@ .
-- * 'trialName' - A filter that returns only components that are part of the specified trial. If you specify @TrialName@ , you can't filter by @ExperimentName@ or @SourceArn@ .
mkListTrialComponents ::
  ListTrialComponents
mkListTrialComponents =
  ListTrialComponents'
    { createdAfter = Lude.Nothing,
      sourceARN = Lude.Nothing,
      experimentName = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      trialName = Lude.Nothing,
      maxResults = Lude.Nothing,
      createdBefore = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A filter that returns only components created after the specified time.
--
-- /Note:/ Consider using 'createdAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcCreatedAfter :: Lens.Lens' ListTrialComponents (Lude.Maybe Lude.Timestamp)
ltcCreatedAfter = Lens.lens (createdAfter :: ListTrialComponents -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAfter = a} :: ListTrialComponents)
{-# DEPRECATED ltcCreatedAfter "Use generic-lens or generic-optics with 'createdAfter' instead." #-}

-- | A filter that returns only components that have the specified source Amazon Resource Name (ARN). If you specify @SourceArn@ , you can't filter by @ExperimentName@ or @TrialName@ .
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSourceARN :: Lens.Lens' ListTrialComponents (Lude.Maybe Lude.Text)
ltcSourceARN = Lens.lens (sourceARN :: ListTrialComponents -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: ListTrialComponents)
{-# DEPRECATED ltcSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | A filter that returns only components that are part of the specified experiment. If you specify @ExperimentName@ , you can't filter by @SourceArn@ or @TrialName@ .
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcExperimentName :: Lens.Lens' ListTrialComponents (Lude.Maybe Lude.Text)
ltcExperimentName = Lens.lens (experimentName :: ListTrialComponents -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: ListTrialComponents)
{-# DEPRECATED ltcExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | If the previous call to @ListTrialComponents@ didn't return the full set of components, the call returns a token for getting the next set of components.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcNextToken :: Lens.Lens' ListTrialComponents (Lude.Maybe Lude.Text)
ltcNextToken = Lens.lens (nextToken :: ListTrialComponents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrialComponents)
{-# DEPRECATED ltcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order. The default value is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSortOrder :: Lens.Lens' ListTrialComponents (Lude.Maybe SortOrder)
ltcSortOrder = Lens.lens (sortOrder :: ListTrialComponents -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListTrialComponents)
{-# DEPRECATED ltcSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only components that are part of the specified trial. If you specify @TrialName@ , you can't filter by @ExperimentName@ or @SourceArn@ .
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcTrialName :: Lens.Lens' ListTrialComponents (Lude.Maybe Lude.Text)
ltcTrialName = Lens.lens (trialName :: ListTrialComponents -> Lude.Maybe Lude.Text) (\s a -> s {trialName = a} :: ListTrialComponents)
{-# DEPRECATED ltcTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

-- | The maximum number of components to return in the response. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcMaxResults :: Lens.Lens' ListTrialComponents (Lude.Maybe Lude.Natural)
ltcMaxResults = Lens.lens (maxResults :: ListTrialComponents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTrialComponents)
{-# DEPRECATED ltcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A filter that returns only components created before the specified time.
--
-- /Note:/ Consider using 'createdBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcCreatedBefore :: Lens.Lens' ListTrialComponents (Lude.Maybe Lude.Timestamp)
ltcCreatedBefore = Lens.lens (createdBefore :: ListTrialComponents -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdBefore = a} :: ListTrialComponents)
{-# DEPRECATED ltcCreatedBefore "Use generic-lens or generic-optics with 'createdBefore' instead." #-}

-- | The property used to sort results. The default value is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcSortBy :: Lens.Lens' ListTrialComponents (Lude.Maybe SortTrialComponentsBy)
ltcSortBy = Lens.lens (sortBy :: ListTrialComponents -> Lude.Maybe SortTrialComponentsBy) (\s a -> s {sortBy = a} :: ListTrialComponents)
{-# DEPRECATED ltcSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListTrialComponents where
  page rq rs
    | Page.stop (rs Lens.^. ltcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltcrsTrialComponentSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltcNextToken Lens..~ rs Lens.^. ltcrsNextToken

instance Lude.AWSRequest ListTrialComponents where
  type Rs ListTrialComponents = ListTrialComponentsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTrialComponentsResponse'
            Lude.<$> (x Lude..?> "TrialComponentSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTrialComponents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListTrialComponents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTrialComponents where
  toJSON ListTrialComponents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreatedAfter" Lude..=) Lude.<$> createdAfter,
            ("SourceArn" Lude..=) Lude.<$> sourceARN,
            ("ExperimentName" Lude..=) Lude.<$> experimentName,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("TrialName" Lude..=) Lude.<$> trialName,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("CreatedBefore" Lude..=) Lude.<$> createdBefore,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListTrialComponents where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTrialComponents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTrialComponentsResponse' smart constructor.
data ListTrialComponentsResponse = ListTrialComponentsResponse'
  { trialComponentSummaries ::
      Lude.Maybe [TrialComponentSummary],
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

-- | Creates a value of 'ListTrialComponentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token for getting the next set of components, if there are any.
-- * 'responseStatus' - The response status code.
-- * 'trialComponentSummaries' - A list of the summaries of your trial components.
mkListTrialComponentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTrialComponentsResponse
mkListTrialComponentsResponse pResponseStatus_ =
  ListTrialComponentsResponse'
    { trialComponentSummaries =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the summaries of your trial components.
--
-- /Note:/ Consider using 'trialComponentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrsTrialComponentSummaries :: Lens.Lens' ListTrialComponentsResponse (Lude.Maybe [TrialComponentSummary])
ltcrsTrialComponentSummaries = Lens.lens (trialComponentSummaries :: ListTrialComponentsResponse -> Lude.Maybe [TrialComponentSummary]) (\s a -> s {trialComponentSummaries = a} :: ListTrialComponentsResponse)
{-# DEPRECATED ltcrsTrialComponentSummaries "Use generic-lens or generic-optics with 'trialComponentSummaries' instead." #-}

-- | A token for getting the next set of components, if there are any.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrsNextToken :: Lens.Lens' ListTrialComponentsResponse (Lude.Maybe Lude.Text)
ltcrsNextToken = Lens.lens (nextToken :: ListTrialComponentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrialComponentsResponse)
{-# DEPRECATED ltcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrsResponseStatus :: Lens.Lens' ListTrialComponentsResponse Lude.Int
ltcrsResponseStatus = Lens.lens (responseStatus :: ListTrialComponentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrialComponentsResponse)
{-# DEPRECATED ltcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
