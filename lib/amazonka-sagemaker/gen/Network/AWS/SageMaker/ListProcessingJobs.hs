{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListProcessingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists processing jobs that satisfy various filters.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListProcessingJobs
  ( -- * Creating a request
    ListProcessingJobs (..),
    mkListProcessingJobs,

    -- ** Request lenses
    lpjNameContains,
    lpjLastModifiedTimeBefore,
    lpjCreationTimeAfter,
    lpjNextToken,
    lpjSortOrder,
    lpjLastModifiedTimeAfter,
    lpjCreationTimeBefore,
    lpjStatusEquals,
    lpjMaxResults,
    lpjSortBy,

    -- * Destructuring the response
    ListProcessingJobsResponse (..),
    mkListProcessingJobsResponse,

    -- ** Response lenses
    lpjrsNextToken,
    lpjrsResponseStatus,
    lpjrsProcessingJobSummaries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListProcessingJobs' smart constructor.
data ListProcessingJobs = ListProcessingJobs'
  { nameContains ::
      Lude.Maybe Lude.Text,
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    statusEquals :: Lude.Maybe ProcessingJobStatus,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe SortBy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProcessingJobs' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only processing jobs created after the specified time.
-- * 'creationTimeBefore' - A filter that returns only processing jobs created after the specified time.
-- * 'lastModifiedTimeAfter' - A filter that returns only processing jobs modified after the specified time.
-- * 'lastModifiedTimeBefore' - A filter that returns only processing jobs modified before the specified time.
-- * 'maxResults' - The maximum number of processing jobs to return in the response.
-- * 'nameContains' - A string in the processing job name. This filter returns only processing jobs whose name contains the specified string.
-- * 'nextToken' - If the result of the previous @ListProcessingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of processing jobs, use the token in the next request.
-- * 'sortBy' - The field to sort results by. The default is @CreationTime@ .
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'statusEquals' - A filter that retrieves only processing jobs with a specific status.
mkListProcessingJobs ::
  ListProcessingJobs
mkListProcessingJobs =
  ListProcessingJobs'
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

-- | A string in the processing job name. This filter returns only processing jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjNameContains :: Lens.Lens' ListProcessingJobs (Lude.Maybe Lude.Text)
lpjNameContains = Lens.lens (nameContains :: ListProcessingJobs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListProcessingJobs)
{-# DEPRECATED lpjNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only processing jobs modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjLastModifiedTimeBefore :: Lens.Lens' ListProcessingJobs (Lude.Maybe Lude.Timestamp)
lpjLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListProcessingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListProcessingJobs)
{-# DEPRECATED lpjLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only processing jobs created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjCreationTimeAfter :: Lens.Lens' ListProcessingJobs (Lude.Maybe Lude.Timestamp)
lpjCreationTimeAfter = Lens.lens (creationTimeAfter :: ListProcessingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListProcessingJobs)
{-# DEPRECATED lpjCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of the previous @ListProcessingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of processing jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjNextToken :: Lens.Lens' ListProcessingJobs (Lude.Maybe Lude.Text)
lpjNextToken = Lens.lens (nextToken :: ListProcessingJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProcessingJobs)
{-# DEPRECATED lpjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjSortOrder :: Lens.Lens' ListProcessingJobs (Lude.Maybe SortOrder)
lpjSortOrder = Lens.lens (sortOrder :: ListProcessingJobs -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListProcessingJobs)
{-# DEPRECATED lpjSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only processing jobs modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjLastModifiedTimeAfter :: Lens.Lens' ListProcessingJobs (Lude.Maybe Lude.Timestamp)
lpjLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListProcessingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListProcessingJobs)
{-# DEPRECATED lpjLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only processing jobs created after the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjCreationTimeBefore :: Lens.Lens' ListProcessingJobs (Lude.Maybe Lude.Timestamp)
lpjCreationTimeBefore = Lens.lens (creationTimeBefore :: ListProcessingJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListProcessingJobs)
{-# DEPRECATED lpjCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that retrieves only processing jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjStatusEquals :: Lens.Lens' ListProcessingJobs (Lude.Maybe ProcessingJobStatus)
lpjStatusEquals = Lens.lens (statusEquals :: ListProcessingJobs -> Lude.Maybe ProcessingJobStatus) (\s a -> s {statusEquals = a} :: ListProcessingJobs)
{-# DEPRECATED lpjStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of processing jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjMaxResults :: Lens.Lens' ListProcessingJobs (Lude.Maybe Lude.Natural)
lpjMaxResults = Lens.lens (maxResults :: ListProcessingJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListProcessingJobs)
{-# DEPRECATED lpjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjSortBy :: Lens.Lens' ListProcessingJobs (Lude.Maybe SortBy)
lpjSortBy = Lens.lens (sortBy :: ListProcessingJobs -> Lude.Maybe SortBy) (\s a -> s {sortBy = a} :: ListProcessingJobs)
{-# DEPRECATED lpjSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListProcessingJobs where
  page rq rs
    | Page.stop (rs Lens.^. lpjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpjrsProcessingJobSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpjNextToken Lens..~ rs Lens.^. lpjrsNextToken

instance Lude.AWSRequest ListProcessingJobs where
  type Rs ListProcessingJobs = ListProcessingJobsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProcessingJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "ProcessingJobSummaries" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListProcessingJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListProcessingJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListProcessingJobs where
  toJSON ListProcessingJobs' {..} =
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

instance Lude.ToPath ListProcessingJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListProcessingJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListProcessingJobsResponse' smart constructor.
data ListProcessingJobsResponse = ListProcessingJobsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    processingJobSummaries ::
      [ProcessingJobSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProcessingJobsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of processing jobs, use it in the subsequent request.
-- * 'processingJobSummaries' - An array of @ProcessingJobSummary@ objects, each listing a processing job.
-- * 'responseStatus' - The response status code.
mkListProcessingJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProcessingJobsResponse
mkListProcessingJobsResponse pResponseStatus_ =
  ListProcessingJobsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      processingJobSummaries = Lude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of processing jobs, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrsNextToken :: Lens.Lens' ListProcessingJobsResponse (Lude.Maybe Lude.Text)
lpjrsNextToken = Lens.lens (nextToken :: ListProcessingJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProcessingJobsResponse)
{-# DEPRECATED lpjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrsResponseStatus :: Lens.Lens' ListProcessingJobsResponse Lude.Int
lpjrsResponseStatus = Lens.lens (responseStatus :: ListProcessingJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProcessingJobsResponse)
{-# DEPRECATED lpjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array of @ProcessingJobSummary@ objects, each listing a processing job.
--
-- /Note:/ Consider using 'processingJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpjrsProcessingJobSummaries :: Lens.Lens' ListProcessingJobsResponse [ProcessingJobSummary]
lpjrsProcessingJobSummaries = Lens.lens (processingJobSummaries :: ListProcessingJobsResponse -> [ProcessingJobSummary]) (\s a -> s {processingJobSummaries = a} :: ListProcessingJobsResponse)
{-# DEPRECATED lpjrsProcessingJobSummaries "Use generic-lens or generic-optics with 'processingJobSummaries' instead." #-}
