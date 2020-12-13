{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListCompilationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists model compilation jobs that satisfy various filters.
--
-- To create a model compilation job, use 'CreateCompilationJob' . To get information about a particular model compilation job you have created, use 'DescribeCompilationJob' .
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCompilationJobs
  ( -- * Creating a request
    ListCompilationJobs (..),
    mkListCompilationJobs,

    -- ** Request lenses
    lcjNameContains,
    lcjLastModifiedTimeBefore,
    lcjCreationTimeAfter,
    lcjNextToken,
    lcjSortOrder,
    lcjLastModifiedTimeAfter,
    lcjCreationTimeBefore,
    lcjStatusEquals,
    lcjMaxResults,
    lcjSortBy,

    -- * Destructuring the response
    ListCompilationJobsResponse (..),
    mkListCompilationJobsResponse,

    -- ** Response lenses
    lcjrsCompilationJobSummaries,
    lcjrsNextToken,
    lcjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListCompilationJobs' smart constructor.
data ListCompilationJobs = ListCompilationJobs'
  { -- | A filter that returns the model compilation jobs whose name contains a specified string.
    nameContains :: Lude.Maybe Lude.Text,
    -- | A filter that returns the model compilation jobs that were modified before a specified time.
    lastModifiedTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns the model compilation jobs that were created after a specified time.
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | If the result of the previous @ListCompilationJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model compilation jobs, use the token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Lude.Maybe SortOrder,
    -- | A filter that returns the model compilation jobs that were modified after a specified time.
    lastModifiedTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | A filter that returns the model compilation jobs that were created before a specified time.
    creationTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | A filter that retrieves model compilation jobs with a specific 'DescribeCompilationJobResponse$CompilationJobStatus' status.
    statusEquals :: Lude.Maybe CompilationJobStatus,
    -- | The maximum number of model compilation jobs to return in the response.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The field by which to sort results. The default is @CreationTime@ .
    sortBy :: Lude.Maybe ListCompilationJobsSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCompilationJobs' with the minimum fields required to make a request.
--
-- * 'nameContains' - A filter that returns the model compilation jobs whose name contains a specified string.
-- * 'lastModifiedTimeBefore' - A filter that returns the model compilation jobs that were modified before a specified time.
-- * 'creationTimeAfter' - A filter that returns the model compilation jobs that were created after a specified time.
-- * 'nextToken' - If the result of the previous @ListCompilationJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model compilation jobs, use the token in the next request.
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'lastModifiedTimeAfter' - A filter that returns the model compilation jobs that were modified after a specified time.
-- * 'creationTimeBefore' - A filter that returns the model compilation jobs that were created before a specified time.
-- * 'statusEquals' - A filter that retrieves model compilation jobs with a specific 'DescribeCompilationJobResponse$CompilationJobStatus' status.
-- * 'maxResults' - The maximum number of model compilation jobs to return in the response.
-- * 'sortBy' - The field by which to sort results. The default is @CreationTime@ .
mkListCompilationJobs ::
  ListCompilationJobs
mkListCompilationJobs =
  ListCompilationJobs'
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

-- | A filter that returns the model compilation jobs whose name contains a specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjNameContains :: Lens.Lens' ListCompilationJobs (Lude.Maybe Lude.Text)
lcjNameContains = Lens.lens (nameContains :: ListCompilationJobs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListCompilationJobs)
{-# DEPRECATED lcjNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns the model compilation jobs that were modified before a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjLastModifiedTimeBefore :: Lens.Lens' ListCompilationJobs (Lude.Maybe Lude.Timestamp)
lcjLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListCompilationJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListCompilationJobs)
{-# DEPRECATED lcjLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns the model compilation jobs that were created after a specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjCreationTimeAfter :: Lens.Lens' ListCompilationJobs (Lude.Maybe Lude.Timestamp)
lcjCreationTimeAfter = Lens.lens (creationTimeAfter :: ListCompilationJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListCompilationJobs)
{-# DEPRECATED lcjCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of the previous @ListCompilationJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model compilation jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjNextToken :: Lens.Lens' ListCompilationJobs (Lude.Maybe Lude.Text)
lcjNextToken = Lens.lens (nextToken :: ListCompilationJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCompilationJobs)
{-# DEPRECATED lcjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjSortOrder :: Lens.Lens' ListCompilationJobs (Lude.Maybe SortOrder)
lcjSortOrder = Lens.lens (sortOrder :: ListCompilationJobs -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListCompilationJobs)
{-# DEPRECATED lcjSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns the model compilation jobs that were modified after a specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjLastModifiedTimeAfter :: Lens.Lens' ListCompilationJobs (Lude.Maybe Lude.Timestamp)
lcjLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListCompilationJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListCompilationJobs)
{-# DEPRECATED lcjLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns the model compilation jobs that were created before a specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjCreationTimeBefore :: Lens.Lens' ListCompilationJobs (Lude.Maybe Lude.Timestamp)
lcjCreationTimeBefore = Lens.lens (creationTimeBefore :: ListCompilationJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListCompilationJobs)
{-# DEPRECATED lcjCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that retrieves model compilation jobs with a specific 'DescribeCompilationJobResponse$CompilationJobStatus' status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjStatusEquals :: Lens.Lens' ListCompilationJobs (Lude.Maybe CompilationJobStatus)
lcjStatusEquals = Lens.lens (statusEquals :: ListCompilationJobs -> Lude.Maybe CompilationJobStatus) (\s a -> s {statusEquals = a} :: ListCompilationJobs)
{-# DEPRECATED lcjStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of model compilation jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjMaxResults :: Lens.Lens' ListCompilationJobs (Lude.Maybe Lude.Natural)
lcjMaxResults = Lens.lens (maxResults :: ListCompilationJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCompilationJobs)
{-# DEPRECATED lcjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field by which to sort results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjSortBy :: Lens.Lens' ListCompilationJobs (Lude.Maybe ListCompilationJobsSortBy)
lcjSortBy = Lens.lens (sortBy :: ListCompilationJobs -> Lude.Maybe ListCompilationJobsSortBy) (\s a -> s {sortBy = a} :: ListCompilationJobs)
{-# DEPRECATED lcjSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListCompilationJobs where
  page rq rs
    | Page.stop (rs Lens.^. lcjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcjrsCompilationJobSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcjNextToken Lens..~ rs Lens.^. lcjrsNextToken

instance Lude.AWSRequest ListCompilationJobs where
  type Rs ListCompilationJobs = ListCompilationJobsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCompilationJobsResponse'
            Lude.<$> (x Lude..?> "CompilationJobSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCompilationJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListCompilationJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCompilationJobs where
  toJSON ListCompilationJobs' {..} =
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

instance Lude.ToPath ListCompilationJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCompilationJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCompilationJobsResponse' smart constructor.
data ListCompilationJobsResponse = ListCompilationJobsResponse'
  { -- | An array of 'CompilationJobSummary' objects, each describing a model compilation job.
    compilationJobSummaries :: [CompilationJobSummary],
    -- | If the response is truncated, Amazon SageMaker returns this @NextToken@ . To retrieve the next set of model compilation jobs, use this token in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCompilationJobsResponse' with the minimum fields required to make a request.
--
-- * 'compilationJobSummaries' - An array of 'CompilationJobSummary' objects, each describing a model compilation job.
-- * 'nextToken' - If the response is truncated, Amazon SageMaker returns this @NextToken@ . To retrieve the next set of model compilation jobs, use this token in the next request.
-- * 'responseStatus' - The response status code.
mkListCompilationJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCompilationJobsResponse
mkListCompilationJobsResponse pResponseStatus_ =
  ListCompilationJobsResponse'
    { compilationJobSummaries =
        Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of 'CompilationJobSummary' objects, each describing a model compilation job.
--
-- /Note:/ Consider using 'compilationJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrsCompilationJobSummaries :: Lens.Lens' ListCompilationJobsResponse [CompilationJobSummary]
lcjrsCompilationJobSummaries = Lens.lens (compilationJobSummaries :: ListCompilationJobsResponse -> [CompilationJobSummary]) (\s a -> s {compilationJobSummaries = a} :: ListCompilationJobsResponse)
{-# DEPRECATED lcjrsCompilationJobSummaries "Use generic-lens or generic-optics with 'compilationJobSummaries' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this @NextToken@ . To retrieve the next set of model compilation jobs, use this token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrsNextToken :: Lens.Lens' ListCompilationJobsResponse (Lude.Maybe Lude.Text)
lcjrsNextToken = Lens.lens (nextToken :: ListCompilationJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCompilationJobsResponse)
{-# DEPRECATED lcjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcjrsResponseStatus :: Lens.Lens' ListCompilationJobsResponse Lude.Int
lcjrsResponseStatus = Lens.lens (responseStatus :: ListCompilationJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCompilationJobsResponse)
{-# DEPRECATED lcjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
