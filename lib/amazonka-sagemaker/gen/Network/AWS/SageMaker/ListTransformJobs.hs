{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTransformJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transform jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTransformJobs
  ( -- * Creating a request
    ListTransformJobs (..),
    mkListTransformJobs,

    -- ** Request lenses
    ltjNameContains,
    ltjLastModifiedTimeBefore,
    ltjCreationTimeAfter,
    ltjNextToken,
    ltjSortOrder,
    ltjLastModifiedTimeAfter,
    ltjCreationTimeBefore,
    ltjStatusEquals,
    ltjMaxResults,
    ltjSortBy,

    -- * Destructuring the response
    ListTransformJobsResponse (..),
    mkListTransformJobsResponse,

    -- ** Response lenses
    ltjsrsNextToken,
    ltjsrsResponseStatus,
    ltjsrsTransformJobSummaries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListTransformJobs' smart constructor.
data ListTransformJobs = ListTransformJobs'
  { nameContains ::
      Lude.Maybe Lude.Text,
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    statusEquals :: Lude.Maybe TransformJobStatus,
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

-- | Creates a value of 'ListTransformJobs' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only transform jobs created after the specified time.
-- * 'creationTimeBefore' - A filter that returns only transform jobs created before the specified time.
-- * 'lastModifiedTimeAfter' - A filter that returns only transform jobs modified after the specified time.
-- * 'lastModifiedTimeBefore' - A filter that returns only transform jobs modified before the specified time.
-- * 'maxResults' - The maximum number of transform jobs to return in the response. The default value is @10@ .
-- * 'nameContains' - A string in the transform job name. This filter returns only transform jobs whose name contains the specified string.
-- * 'nextToken' - If the result of the previous @ListTransformJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of transform jobs, use the token in the next request.
-- * 'sortBy' - The field to sort results by. The default is @CreationTime@ .
-- * 'sortOrder' - The sort order for results. The default is @Descending@ .
-- * 'statusEquals' - A filter that retrieves only transform jobs with a specific status.
mkListTransformJobs ::
  ListTransformJobs
mkListTransformJobs =
  ListTransformJobs'
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

-- | A string in the transform job name. This filter returns only transform jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjNameContains :: Lens.Lens' ListTransformJobs (Lude.Maybe Lude.Text)
ltjNameContains = Lens.lens (nameContains :: ListTransformJobs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListTransformJobs)
{-# DEPRECATED ltjNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only transform jobs modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjLastModifiedTimeBefore :: Lens.Lens' ListTransformJobs (Lude.Maybe Lude.Timestamp)
ltjLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListTransformJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListTransformJobs)
{-# DEPRECATED ltjLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only transform jobs created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjCreationTimeAfter :: Lens.Lens' ListTransformJobs (Lude.Maybe Lude.Timestamp)
ltjCreationTimeAfter = Lens.lens (creationTimeAfter :: ListTransformJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListTransformJobs)
{-# DEPRECATED ltjCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of the previous @ListTransformJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of transform jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjNextToken :: Lens.Lens' ListTransformJobs (Lude.Maybe Lude.Text)
ltjNextToken = Lens.lens (nextToken :: ListTransformJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTransformJobs)
{-# DEPRECATED ltjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjSortOrder :: Lens.Lens' ListTransformJobs (Lude.Maybe SortOrder)
ltjSortOrder = Lens.lens (sortOrder :: ListTransformJobs -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListTransformJobs)
{-# DEPRECATED ltjSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only transform jobs modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjLastModifiedTimeAfter :: Lens.Lens' ListTransformJobs (Lude.Maybe Lude.Timestamp)
ltjLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListTransformJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListTransformJobs)
{-# DEPRECATED ltjLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only transform jobs created before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjCreationTimeBefore :: Lens.Lens' ListTransformJobs (Lude.Maybe Lude.Timestamp)
ltjCreationTimeBefore = Lens.lens (creationTimeBefore :: ListTransformJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListTransformJobs)
{-# DEPRECATED ltjCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that retrieves only transform jobs with a specific status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjStatusEquals :: Lens.Lens' ListTransformJobs (Lude.Maybe TransformJobStatus)
ltjStatusEquals = Lens.lens (statusEquals :: ListTransformJobs -> Lude.Maybe TransformJobStatus) (\s a -> s {statusEquals = a} :: ListTransformJobs)
{-# DEPRECATED ltjStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of transform jobs to return in the response. The default value is @10@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjMaxResults :: Lens.Lens' ListTransformJobs (Lude.Maybe Lude.Natural)
ltjMaxResults = Lens.lens (maxResults :: ListTransformJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTransformJobs)
{-# DEPRECATED ltjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjSortBy :: Lens.Lens' ListTransformJobs (Lude.Maybe SortBy)
ltjSortBy = Lens.lens (sortBy :: ListTransformJobs -> Lude.Maybe SortBy) (\s a -> s {sortBy = a} :: ListTransformJobs)
{-# DEPRECATED ltjSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListTransformJobs where
  page rq rs
    | Page.stop (rs Lens.^. ltjsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltjsrsTransformJobSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltjNextToken Lens..~ rs Lens.^. ltjsrsNextToken

instance Lude.AWSRequest ListTransformJobs where
  type Rs ListTransformJobs = ListTransformJobsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTransformJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "TransformJobSummaries" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListTransformJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListTransformJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTransformJobs where
  toJSON ListTransformJobs' {..} =
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

instance Lude.ToPath ListTransformJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTransformJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTransformJobsResponse' smart constructor.
data ListTransformJobsResponse = ListTransformJobsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    transformJobSummaries ::
      [TransformJobSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTransformJobsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of transform jobs, use it in the next request.
-- * 'responseStatus' - The response status code.
-- * 'transformJobSummaries' - An array of @TransformJobSummary@ objects.
mkListTransformJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTransformJobsResponse
mkListTransformJobsResponse pResponseStatus_ =
  ListTransformJobsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      transformJobSummaries = Lude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of transform jobs, use it in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsrsNextToken :: Lens.Lens' ListTransformJobsResponse (Lude.Maybe Lude.Text)
ltjsrsNextToken = Lens.lens (nextToken :: ListTransformJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTransformJobsResponse)
{-# DEPRECATED ltjsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsrsResponseStatus :: Lens.Lens' ListTransformJobsResponse Lude.Int
ltjsrsResponseStatus = Lens.lens (responseStatus :: ListTransformJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTransformJobsResponse)
{-# DEPRECATED ltjsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An array of @TransformJobSummary@ objects.
--
-- /Note:/ Consider using 'transformJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjsrsTransformJobSummaries :: Lens.Lens' ListTransformJobsResponse [TransformJobSummary]
ltjsrsTransformJobSummaries = Lens.lens (transformJobSummaries :: ListTransformJobsResponse -> [TransformJobSummary]) (\s a -> s {transformJobSummaries = a} :: ListTransformJobsResponse)
{-# DEPRECATED ltjsrsTransformJobSummaries "Use generic-lens or generic-optics with 'transformJobSummaries' instead." #-}
