{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListLabelingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of labeling jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListLabelingJobs
  ( -- * Creating a request
    ListLabelingJobs (..),
    mkListLabelingJobs,

    -- ** Request lenses
    lljNameContains,
    lljLastModifiedTimeBefore,
    lljCreationTimeAfter,
    lljNextToken,
    lljSortOrder,
    lljLastModifiedTimeAfter,
    lljCreationTimeBefore,
    lljStatusEquals,
    lljMaxResults,
    lljSortBy,

    -- * Destructuring the response
    ListLabelingJobsResponse (..),
    mkListLabelingJobsResponse,

    -- ** Response lenses
    lljrsLabelingJobSummaryList,
    lljrsNextToken,
    lljrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListLabelingJobs' smart constructor.
data ListLabelingJobs = ListLabelingJobs'
  { -- | A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns only labeling jobs modified before the specified time (timestamp).
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only labeling jobs created after the specified time (timestamp).
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns only labeling jobs modified after the specified time (timestamp).
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns only labeling jobs created before the specified time (timestamp).
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that retrieves only labeling jobs with a specific status.
    statusEquals :: Lude.Maybe LabelingJobStatus,
    -- | The maximum number of labeling jobs to return in each page of the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Lude.Maybe SortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLabelingJobs' with the minimum fields required to make a request.
--
-- * 'nameContains' - A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
-- * 'lastModifiedTimeBefore' - A filter that returns only labeling jobs modified before the specified time (timestamp).
-- * 'creationTimeAfter' - A filter that returns only labeling jobs created after the specified time (timestamp).
-- * 'nextToken' - If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'lastModifiedTimeAfter' - A filter that returns only labeling jobs modified after the specified time (timestamp).
-- * 'creationTimeBefore' - A filter that returns only labeling jobs created before the specified time (timestamp).
-- * 'statusEquals' - A filter that retrieves only labeling jobs with a specific status.
-- * 'maxResults' - The maximum number of labeling jobs to return in each page of the response.
-- * 'sortBy' - The field to sort results by. The default is @CreationTime@ .
mkListLabelingJobs ::
  ListLabelingJobs
mkListLabelingJobs =
  ListLabelingJobs'
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

-- | A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljNameContains :: Lens.Lens' ListLabelingJobs (Lude.Maybe Lude.Text)
lljNameContains = Lens.lens (nameContains :: ListLabelingJobs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListLabelingJobs)
{-# DEPRECATED lljNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only labeling jobs modified before the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljLastModifiedTimeBefore :: Lens.Lens' ListLabelingJobs (Lude.Maybe Lude.Timestamp)
lljLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListLabelingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListLabelingJobs)
{-# DEPRECATED lljLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only labeling jobs created after the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljCreationTimeAfter :: Lens.Lens' ListLabelingJobs (Lude.Maybe Lude.Timestamp)
lljCreationTimeAfter = Lens.lens (creationTimeAfter :: ListLabelingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListLabelingJobs)
{-# DEPRECATED lljCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljNextToken :: Lens.Lens' ListLabelingJobs (Lude.Maybe Lude.Text)
lljNextToken = Lens.lens (nextToken :: ListLabelingJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLabelingJobs)
{-# DEPRECATED lljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljSortOrder :: Lens.Lens' ListLabelingJobs (Lude.Maybe SortOrder)
lljSortOrder = Lens.lens (sortOrder :: ListLabelingJobs -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListLabelingJobs)
{-# DEPRECATED lljSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only labeling jobs modified after the specified time (timestamp).
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljLastModifiedTimeAfter :: Lens.Lens' ListLabelingJobs (Lude.Maybe Lude.Timestamp)
lljLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListLabelingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListLabelingJobs)
{-# DEPRECATED lljLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only labeling jobs created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljCreationTimeBefore :: Lens.Lens' ListLabelingJobs (Lude.Maybe Lude.Timestamp)
lljCreationTimeBefore = Lens.lens (creationTimeBefore :: ListLabelingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListLabelingJobs)
{-# DEPRECATED lljCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that retrieves only labeling jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljStatusEquals :: Lens.Lens' ListLabelingJobs (Lude.Maybe LabelingJobStatus)
lljStatusEquals = Lens.lens (statusEquals :: ListLabelingJobs -> Lude.Maybe LabelingJobStatus) (\s a -> s {statusEquals = a} :: ListLabelingJobs)
{-# DEPRECATED lljStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of labeling jobs to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljMaxResults :: Lens.Lens' ListLabelingJobs (Lude.Maybe Lude.Natural)
lljMaxResults = Lens.lens (maxResults :: ListLabelingJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListLabelingJobs)
{-# DEPRECATED lljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljSortBy :: Lens.Lens' ListLabelingJobs (Lude.Maybe SortBy)
lljSortBy = Lens.lens (sortBy :: ListLabelingJobs -> Lude.Maybe SortBy) (\s a -> s {sortBy = a} :: ListLabelingJobs)
{-# DEPRECATED lljSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListLabelingJobs where
  page rq rs
    | Page.stop (rs Lens.^. lljrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lljrsLabelingJobSummaryList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lljNextToken Lens..~ rs Lens.^. lljrsNextToken

instance Lude.AWSRequest ListLabelingJobs where
  type Rs ListLabelingJobs = ListLabelingJobsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLabelingJobsResponse'
            Lude.<$> (x Lude..?> "LabelingJobSummaryList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLabelingJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListLabelingJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListLabelingJobs where
  toJSON ListLabelingJobs' {..} =
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

instance Lude.ToPath ListLabelingJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListLabelingJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListLabelingJobsResponse' smart constructor.
data ListLabelingJobsResponse = ListLabelingJobsResponse'
  { -- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
    labelingJobSummaryList :: Lude.Maybe [LabelingJobSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLabelingJobsResponse' with the minimum fields required to make a request.
--
-- * 'labelingJobSummaryList' - An array of @LabelingJobSummary@ objects, each describing a labeling job.
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
-- * 'responseStatus' - The response status code.
mkListLabelingJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLabelingJobsResponse
mkListLabelingJobsResponse pResponseStatus_ =
  ListLabelingJobsResponse'
    { labelingJobSummaryList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
--
-- /Note:/ Consider using 'labelingJobSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrsLabelingJobSummaryList :: Lens.Lens' ListLabelingJobsResponse (Lude.Maybe [LabelingJobSummary])
lljrsLabelingJobSummaryList = Lens.lens (labelingJobSummaryList :: ListLabelingJobsResponse -> Lude.Maybe [LabelingJobSummary]) (\s a -> s {labelingJobSummaryList = a} :: ListLabelingJobsResponse)
{-# DEPRECATED lljrsLabelingJobSummaryList "Use generic-lens or generic-optics with 'labelingJobSummaryList' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrsNextToken :: Lens.Lens' ListLabelingJobsResponse (Lude.Maybe Lude.Text)
lljrsNextToken = Lens.lens (nextToken :: ListLabelingJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLabelingJobsResponse)
{-# DEPRECATED lljrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lljrsResponseStatus :: Lens.Lens' ListLabelingJobsResponse Lude.Int
lljrsResponseStatus = Lens.lens (responseStatus :: ListLabelingJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLabelingJobsResponse)
{-# DEPRECATED lljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
