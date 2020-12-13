{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrainingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists training jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrainingJobs
  ( -- * Creating a request
    ListTrainingJobs (..),
    mkListTrainingJobs,

    -- ** Request lenses
    ltjsNameContains,
    ltjsLastModifiedTimeBefore,
    ltjsCreationTimeAfter,
    ltjsNextToken,
    ltjsSortOrder,
    ltjsLastModifiedTimeAfter,
    ltjsCreationTimeBefore,
    ltjsStatusEquals,
    ltjsMaxResults,
    ltjsSortBy,

    -- * Destructuring the response
    ListTrainingJobsResponse (..),
    mkListTrainingJobsResponse,

    -- ** Response lenses
    ltjrsTrainingJobSummaries,
    ltjrsNextToken,
    ltjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListTrainingJobs' smart constructor.
data ListTrainingJobs = ListTrainingJobs'
  { -- | A string in the training job name. This filter returns only training jobs whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only training jobs modified before the specified time (timestamp).
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only training jobs created after the specified time (timestamp).
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only training jobs modified after the specified time (timestamp).
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only training jobs created before the specified time (timestamp).
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that retrieves only training jobs with a specific status.
    statusEquals :: Lude.Maybe TrainingJobStatus,
    -- | The maximum number of training jobs to return in the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Lude.Maybe SortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrainingJobs' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in the training job name. This filter returns only training jobs whose name contains the specified string.
-- * 'lastModifiedTimeBefore' - A filter that returns only training jobs modified before the specified time (timestamp).
-- * 'creationTimeAfter' - A filter that returns only training jobs created after the specified time (timestamp).
-- * 'nextToken' - If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'lastModifiedTimeAfter' - A filter that returns only training jobs modified after the specified time (timestamp).
-- * 'creationTimeBefore' - A filter that returns only training jobs created before the specified time (timestamp).
-- * 'statusEquals' - A filter that retrieves only training jobs with a specific status.
-- * 'maxResults' - The maximum number of training jobs to return in the response.
-- * 'sortBy' - The field to sort results by. The default is @CreationTime@ .
mkListTrainingJobs ::
  ListTrainingJobs
mkListTrainingJobs =
  ListTrainingJobs'
    { nameContains = Lude.Nothing,
      lastModifiedTimeBefore = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      lastModifiedTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing,
      statusEquals = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | A string in the training job name. This filter returns only training jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsNameContains :: Lens.Lens' ListTrainingJobs (Lude.Maybe Lude.Text)
ltjsNameContains = Lens.lens (nameContains :: ListTrainingJobs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only training jobs modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsLastModifiedTimeBefore :: Lens.Lens' ListTrainingJobs (Lude.Maybe Lude.Timestamp)
ltjsLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListTrainingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only training jobs created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsCreationTimeAfter :: Lens.Lens' ListTrainingJobs (Lude.Maybe Lude.Timestamp)
ltjsCreationTimeAfter = Lens.lens (creationTimeAfter :: ListTrainingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsNextToken :: Lens.Lens' ListTrainingJobs (Lude.Maybe Lude.Text)
ltjsNextToken = Lens.lens (nextToken :: ListTrainingJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsSortOrder :: Lens.Lens' ListTrainingJobs (Lude.Maybe SortOrder)
ltjsSortOrder = Lens.lens (sortOrder :: ListTrainingJobs -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only training jobs modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsLastModifiedTimeAfter :: Lens.Lens' ListTrainingJobs (Lude.Maybe Lude.Timestamp)
ltjsLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListTrainingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only training jobs created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsCreationTimeBefore :: Lens.Lens' ListTrainingJobs (Lude.Maybe Lude.Timestamp)
ltjsCreationTimeBefore = Lens.lens (creationTimeBefore :: ListTrainingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that retrieves only training jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsStatusEquals :: Lens.Lens' ListTrainingJobs (Lude.Maybe TrainingJobStatus)
ltjsStatusEquals = Lens.lens (statusEquals :: ListTrainingJobs -> Lude.Maybe TrainingJobStatus) (\s a -> s {statusEquals = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of training jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsMaxResults :: Lens.Lens' ListTrainingJobs (Lude.Maybe Lude.Natural)
ltjsMaxResults = Lens.lens (maxResults :: ListTrainingJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsSortBy :: Lens.Lens' ListTrainingJobs (Lude.Maybe SortBy)
ltjsSortBy = Lens.lens (sortBy :: ListTrainingJobs -> Lude.Maybe SortBy) (\s a -> s {sortBy = a} :: ListTrainingJobs)
{-# DEPRECATED ltjsSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListTrainingJobs where
  page rq rs
    | Page.stop (rs Lens.^. ltjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltjrsTrainingJobSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltjsNextToken Lens..~ rs Lens.^. ltjrsNextToken

instance Lude.AWSRequest ListTrainingJobs where
  type Rs ListTrainingJobs = ListTrainingJobsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTrainingJobsResponse'
            Lude.<$> (x Lude..?> "TrainingJobSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTrainingJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListTrainingJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTrainingJobs where
  toJSON ListTrainingJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NameContains" Lude..=) Lude.<$> nameContains,
            ("LastModifiedTimeBefore" Lude..=) Lude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("LastModifiedTimeAfter" Lude..=) Lude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore,
            ("StatusEquals" Lude..=) Lude.<$> statusEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListTrainingJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTrainingJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTrainingJobsResponse' smart constructor.
data ListTrainingJobsResponse = ListTrainingJobsResponse'
  { -- | An array of @TrainingJobSummary@ objects, each listing a training job.
    trainingJobSummaries :: [TrainingJobSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrainingJobsResponse' with the minimum fields required to make a request.
--
-- * 'trainingJobSummaries' - An array of @TrainingJobSummary@ objects, each listing a training job.
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
-- * 'responseStatus' - The response status code.
mkListTrainingJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTrainingJobsResponse
mkListTrainingJobsResponse pResponseStatus_ =
  ListTrainingJobsResponse'
    { trainingJobSummaries = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @TrainingJobSummary@ objects, each listing a training job.
--
-- /Note:/ Consider using 'trainingJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrsTrainingJobSummaries :: Lens.Lens' ListTrainingJobsResponse [TrainingJobSummary]
ltjrsTrainingJobSummaries = Lens.lens (trainingJobSummaries :: ListTrainingJobsResponse -> [TrainingJobSummary]) (\s a -> s {trainingJobSummaries = a} :: ListTrainingJobsResponse)
{-# DEPRECATED ltjrsTrainingJobSummaries "Use generic-lens or generic-optics with 'trainingJobSummaries' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrsNextToken :: Lens.Lens' ListTrainingJobsResponse (Lude.Maybe Lude.Text)
ltjrsNextToken = Lens.lens (nextToken :: ListTrainingJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrainingJobsResponse)
{-# DEPRECATED ltjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjrsResponseStatus :: Lens.Lens' ListTrainingJobsResponse Lude.Int
ltjrsResponseStatus = Lens.lens (responseStatus :: ListTrainingJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrainingJobsResponse)
{-# DEPRECATED ltjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
