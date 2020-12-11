{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListAutoMLJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAutoMLJobs
  ( -- * Creating a request
    ListAutoMLJobs (..),
    mkListAutoMLJobs,

    -- ** Request lenses
    lamljNameContains,
    lamljLastModifiedTimeBefore,
    lamljCreationTimeAfter,
    lamljNextToken,
    lamljSortOrder,
    lamljLastModifiedTimeAfter,
    lamljCreationTimeBefore,
    lamljStatusEquals,
    lamljMaxResults,
    lamljSortBy,

    -- * Destructuring the response
    ListAutoMLJobsResponse (..),
    mkListAutoMLJobsResponse,

    -- ** Response lenses
    lamljrsNextToken,
    lamljrsResponseStatus,
    lamljrsAutoMLJobSummaries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListAutoMLJobs' smart constructor.
data ListAutoMLJobs = ListAutoMLJobs'
  { nameContains ::
      Lude.Maybe Lude.Text,
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe AutoMLSortOrder,
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    statusEquals :: Lude.Maybe AutoMLJobStatus,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy :: Lude.Maybe AutoMLSortBy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAutoMLJobs' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - Request a list of jobs, using a filter for time.
-- * 'creationTimeBefore' - Request a list of jobs, using a filter for time.
-- * 'lastModifiedTimeAfter' - Request a list of jobs, using a filter for time.
-- * 'lastModifiedTimeBefore' - Request a list of jobs, using a filter for time.
-- * 'maxResults' - Request a list of jobs up to a specified limit.
-- * 'nameContains' - Request a list of jobs, using a search filter for name.
-- * 'nextToken' - If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
-- * 'sortBy' - The parameter by which to sort the results. The default is AutoMLJobName.
-- * 'sortOrder' - The sort order for the results. The default is Descending.
-- * 'statusEquals' - Request a list of jobs, using a filter for status.
mkListAutoMLJobs ::
  ListAutoMLJobs
mkListAutoMLJobs =
  ListAutoMLJobs'
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

-- | Request a list of jobs, using a search filter for name.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljNameContains :: Lens.Lens' ListAutoMLJobs (Lude.Maybe Lude.Text)
lamljNameContains = Lens.lens (nameContains :: ListAutoMLJobs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | Request a list of jobs, using a filter for time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljLastModifiedTimeBefore :: Lens.Lens' ListAutoMLJobs (Lude.Maybe Lude.Timestamp)
lamljLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListAutoMLJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | Request a list of jobs, using a filter for time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljCreationTimeAfter :: Lens.Lens' ListAutoMLJobs (Lude.Maybe Lude.Timestamp)
lamljCreationTimeAfter = Lens.lens (creationTimeAfter :: ListAutoMLJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljNextToken :: Lens.Lens' ListAutoMLJobs (Lude.Maybe Lude.Text)
lamljNextToken = Lens.lens (nextToken :: ListAutoMLJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for the results. The default is Descending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljSortOrder :: Lens.Lens' ListAutoMLJobs (Lude.Maybe AutoMLSortOrder)
lamljSortOrder = Lens.lens (sortOrder :: ListAutoMLJobs -> Lude.Maybe AutoMLSortOrder) (\s a -> s {sortOrder = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | Request a list of jobs, using a filter for time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljLastModifiedTimeAfter :: Lens.Lens' ListAutoMLJobs (Lude.Maybe Lude.Timestamp)
lamljLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListAutoMLJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | Request a list of jobs, using a filter for time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljCreationTimeBefore :: Lens.Lens' ListAutoMLJobs (Lude.Maybe Lude.Timestamp)
lamljCreationTimeBefore = Lens.lens (creationTimeBefore :: ListAutoMLJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | Request a list of jobs, using a filter for status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljStatusEquals :: Lens.Lens' ListAutoMLJobs (Lude.Maybe AutoMLJobStatus)
lamljStatusEquals = Lens.lens (statusEquals :: ListAutoMLJobs -> Lude.Maybe AutoMLJobStatus) (\s a -> s {statusEquals = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | Request a list of jobs up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljMaxResults :: Lens.Lens' ListAutoMLJobs (Lude.Maybe Lude.Natural)
lamljMaxResults = Lens.lens (maxResults :: ListAutoMLJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter by which to sort the results. The default is AutoMLJobName.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljSortBy :: Lens.Lens' ListAutoMLJobs (Lude.Maybe AutoMLSortBy)
lamljSortBy = Lens.lens (sortBy :: ListAutoMLJobs -> Lude.Maybe AutoMLSortBy) (\s a -> s {sortBy = a} :: ListAutoMLJobs)
{-# DEPRECATED lamljSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListAutoMLJobs where
  page rq rs
    | Page.stop (rs Lens.^. lamljrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lamljrsAutoMLJobSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lamljNextToken Lens..~ rs Lens.^. lamljrsNextToken

instance Lude.AWSRequest ListAutoMLJobs where
  type Rs ListAutoMLJobs = ListAutoMLJobsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAutoMLJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "AutoMLJobSummaries" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListAutoMLJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListAutoMLJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAutoMLJobs where
  toJSON ListAutoMLJobs' {..} =
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

instance Lude.ToPath ListAutoMLJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAutoMLJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAutoMLJobsResponse' smart constructor.
data ListAutoMLJobsResponse = ListAutoMLJobsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    autoMLJobSummaries :: [AutoMLJobSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAutoMLJobsResponse' with the minimum fields required to make a request.
--
-- * 'autoMLJobSummaries' - Returns a summary list of jobs.
-- * 'nextToken' - If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
-- * 'responseStatus' - The response status code.
mkListAutoMLJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAutoMLJobsResponse
mkListAutoMLJobsResponse pResponseStatus_ =
  ListAutoMLJobsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      autoMLJobSummaries = Lude.mempty
    }

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljrsNextToken :: Lens.Lens' ListAutoMLJobsResponse (Lude.Maybe Lude.Text)
lamljrsNextToken = Lens.lens (nextToken :: ListAutoMLJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAutoMLJobsResponse)
{-# DEPRECATED lamljrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljrsResponseStatus :: Lens.Lens' ListAutoMLJobsResponse Lude.Int
lamljrsResponseStatus = Lens.lens (responseStatus :: ListAutoMLJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAutoMLJobsResponse)
{-# DEPRECATED lamljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Returns a summary list of jobs.
--
-- /Note:/ Consider using 'autoMLJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lamljrsAutoMLJobSummaries :: Lens.Lens' ListAutoMLJobsResponse [AutoMLJobSummary]
lamljrsAutoMLJobSummaries = Lens.lens (autoMLJobSummaries :: ListAutoMLJobsResponse -> [AutoMLJobSummary]) (\s a -> s {autoMLJobSummaries = a} :: ListAutoMLJobsResponse)
{-# DEPRECATED lamljrsAutoMLJobSummaries "Use generic-lens or generic-optics with 'autoMLJobSummaries' instead." #-}
