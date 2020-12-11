{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListLabelingJobsForWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of labeling jobs assigned to a specified work team.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListLabelingJobsForWorkteam
  ( -- * Creating a request
    ListLabelingJobsForWorkteam (..),
    mkListLabelingJobsForWorkteam,

    -- ** Request lenses
    lljfwJobReferenceCodeContains,
    lljfwCreationTimeAfter,
    lljfwNextToken,
    lljfwSortOrder,
    lljfwCreationTimeBefore,
    lljfwMaxResults,
    lljfwSortBy,
    lljfwWorkteamARN,

    -- * Destructuring the response
    ListLabelingJobsForWorkteamResponse (..),
    mkListLabelingJobsForWorkteamResponse,

    -- ** Response lenses
    lljfwrsNextToken,
    lljfwrsResponseStatus,
    lljfwrsLabelingJobSummaryList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListLabelingJobsForWorkteam' smart constructor.
data ListLabelingJobsForWorkteam = ListLabelingJobsForWorkteam'
  { jobReferenceCodeContains ::
      Lude.Maybe Lude.Text,
    creationTimeAfter ::
      Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    creationTimeBefore ::
      Lude.Maybe Lude.Timestamp,
    maxResults ::
      Lude.Maybe Lude.Natural,
    sortBy ::
      Lude.Maybe
        ListLabelingJobsForWorkteamSortByOptions,
    workteamARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLabelingJobsForWorkteam' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only labeling jobs created after the specified time (timestamp).
-- * 'creationTimeBefore' - A filter that returns only labeling jobs created before the specified time (timestamp).
-- * 'jobReferenceCodeContains' - A filter the limits jobs to only the ones whose job reference code contains the specified string.
-- * 'maxResults' - The maximum number of labeling jobs to return in each page of the response.
-- * 'nextToken' - If the result of the previous @ListLabelingJobsForWorkteam@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
-- * 'sortBy' - The field to sort results by. The default is @CreationTime@ .
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'workteamARN' - The Amazon Resource Name (ARN) of the work team for which you want to see labeling jobs for.
mkListLabelingJobsForWorkteam ::
  -- | 'workteamARN'
  Lude.Text ->
  ListLabelingJobsForWorkteam
mkListLabelingJobsForWorkteam pWorkteamARN_ =
  ListLabelingJobsForWorkteam'
    { jobReferenceCodeContains =
        Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing,
      workteamARN = pWorkteamARN_
    }

-- | A filter the limits jobs to only the ones whose job reference code contains the specified string.
--
-- /Note:/ Consider using 'jobReferenceCodeContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwJobReferenceCodeContains :: Lens.Lens' ListLabelingJobsForWorkteam (Lude.Maybe Lude.Text)
lljfwJobReferenceCodeContains = Lens.lens (jobReferenceCodeContains :: ListLabelingJobsForWorkteam -> Lude.Maybe Lude.Text) (\s a -> s {jobReferenceCodeContains = a} :: ListLabelingJobsForWorkteam)
{-# DEPRECATED lljfwJobReferenceCodeContains "Use generic-lens or generic-optics with 'jobReferenceCodeContains' instead." #-}

-- | A filter that returns only labeling jobs created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwCreationTimeAfter :: Lens.Lens' ListLabelingJobsForWorkteam (Lude.Maybe Lude.Timestamp)
lljfwCreationTimeAfter = Lens.lens (creationTimeAfter :: ListLabelingJobsForWorkteam -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListLabelingJobsForWorkteam)
{-# DEPRECATED lljfwCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of the previous @ListLabelingJobsForWorkteam@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwNextToken :: Lens.Lens' ListLabelingJobsForWorkteam (Lude.Maybe Lude.Text)
lljfwNextToken = Lens.lens (nextToken :: ListLabelingJobsForWorkteam -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLabelingJobsForWorkteam)
{-# DEPRECATED lljfwNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwSortOrder :: Lens.Lens' ListLabelingJobsForWorkteam (Lude.Maybe SortOrder)
lljfwSortOrder = Lens.lens (sortOrder :: ListLabelingJobsForWorkteam -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListLabelingJobsForWorkteam)
{-# DEPRECATED lljfwSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only labeling jobs created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwCreationTimeBefore :: Lens.Lens' ListLabelingJobsForWorkteam (Lude.Maybe Lude.Timestamp)
lljfwCreationTimeBefore = Lens.lens (creationTimeBefore :: ListLabelingJobsForWorkteam -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListLabelingJobsForWorkteam)
{-# DEPRECATED lljfwCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of labeling jobs to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwMaxResults :: Lens.Lens' ListLabelingJobsForWorkteam (Lude.Maybe Lude.Natural)
lljfwMaxResults = Lens.lens (maxResults :: ListLabelingJobsForWorkteam -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListLabelingJobsForWorkteam)
{-# DEPRECATED lljfwMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwSortBy :: Lens.Lens' ListLabelingJobsForWorkteam (Lude.Maybe ListLabelingJobsForWorkteamSortByOptions)
lljfwSortBy = Lens.lens (sortBy :: ListLabelingJobsForWorkteam -> Lude.Maybe ListLabelingJobsForWorkteamSortByOptions) (\s a -> s {sortBy = a} :: ListLabelingJobsForWorkteam)
{-# DEPRECATED lljfwSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The Amazon Resource Name (ARN) of the work team for which you want to see labeling jobs for.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwWorkteamARN :: Lens.Lens' ListLabelingJobsForWorkteam Lude.Text
lljfwWorkteamARN = Lens.lens (workteamARN :: ListLabelingJobsForWorkteam -> Lude.Text) (\s a -> s {workteamARN = a} :: ListLabelingJobsForWorkteam)
{-# DEPRECATED lljfwWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

instance Page.AWSPager ListLabelingJobsForWorkteam where
  page rq rs
    | Page.stop (rs Lens.^. lljfwrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lljfwrsLabelingJobSummaryList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lljfwNextToken Lens..~ rs Lens.^. lljfwrsNextToken

instance Lude.AWSRequest ListLabelingJobsForWorkteam where
  type
    Rs ListLabelingJobsForWorkteam =
      ListLabelingJobsForWorkteamResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLabelingJobsForWorkteamResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "LabelingJobSummaryList" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListLabelingJobsForWorkteam where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListLabelingJobsForWorkteam" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListLabelingJobsForWorkteam where
  toJSON ListLabelingJobsForWorkteam' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobReferenceCodeContains" Lude..=)
              Lude.<$> jobReferenceCodeContains,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy,
            Lude.Just ("WorkteamArn" Lude..= workteamARN)
          ]
      )

instance Lude.ToPath ListLabelingJobsForWorkteam where
  toPath = Lude.const "/"

instance Lude.ToQuery ListLabelingJobsForWorkteam where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListLabelingJobsForWorkteamResponse' smart constructor.
data ListLabelingJobsForWorkteamResponse = ListLabelingJobsForWorkteamResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    labelingJobSummaryList ::
      [LabelingJobForWorkteamSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLabelingJobsForWorkteamResponse' with the minimum fields required to make a request.
--
-- * 'labelingJobSummaryList' - An array of @LabelingJobSummary@ objects, each describing a labeling job.
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
-- * 'responseStatus' - The response status code.
mkListLabelingJobsForWorkteamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLabelingJobsForWorkteamResponse
mkListLabelingJobsForWorkteamResponse pResponseStatus_ =
  ListLabelingJobsForWorkteamResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      labelingJobSummaryList = Lude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrsNextToken :: Lens.Lens' ListLabelingJobsForWorkteamResponse (Lude.Maybe Lude.Text)
lljfwrsNextToken = Lens.lens (nextToken :: ListLabelingJobsForWorkteamResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLabelingJobsForWorkteamResponse)
{-# DEPRECATED lljfwrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrsResponseStatus :: Lens.Lens' ListLabelingJobsForWorkteamResponse Lude.Int
lljfwrsResponseStatus = Lens.lens (responseStatus :: ListLabelingJobsForWorkteamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLabelingJobsForWorkteamResponse)
{-# DEPRECATED lljfwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
--
-- /Note:/ Consider using 'labelingJobSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljfwrsLabelingJobSummaryList :: Lens.Lens' ListLabelingJobsForWorkteamResponse [LabelingJobForWorkteamSummary]
lljfwrsLabelingJobSummaryList = Lens.lens (labelingJobSummaryList :: ListLabelingJobsForWorkteamResponse -> [LabelingJobForWorkteamSummary]) (\s a -> s {labelingJobSummaryList = a} :: ListLabelingJobsForWorkteamResponse)
{-# DEPRECATED lljfwrsLabelingJobSummaryList "Use generic-lens or generic-optics with 'labelingJobSummaryList' instead." #-}
