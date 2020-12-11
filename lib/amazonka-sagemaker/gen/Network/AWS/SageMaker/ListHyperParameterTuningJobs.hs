{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListHyperParameterTuningJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of 'HyperParameterTuningJobSummary' objects that describe the hyperparameter tuning jobs launched in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListHyperParameterTuningJobs
  ( -- * Creating a request
    ListHyperParameterTuningJobs (..),
    mkListHyperParameterTuningJobs,

    -- ** Request lenses
    lhptjNameContains,
    lhptjLastModifiedTimeBefore,
    lhptjCreationTimeAfter,
    lhptjNextToken,
    lhptjSortOrder,
    lhptjLastModifiedTimeAfter,
    lhptjCreationTimeBefore,
    lhptjStatusEquals,
    lhptjMaxResults,
    lhptjSortBy,

    -- * Destructuring the response
    ListHyperParameterTuningJobsResponse (..),
    mkListHyperParameterTuningJobsResponse,

    -- ** Response lenses
    lhptjrsNextToken,
    lhptjrsResponseStatus,
    lhptjrsHyperParameterTuningJobSummaries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListHyperParameterTuningJobs' smart constructor.
data ListHyperParameterTuningJobs = ListHyperParameterTuningJobs'
  { nameContains ::
      Lude.Maybe Lude.Text,
    lastModifiedTimeBefore ::
      Lude.Maybe Lude.Timestamp,
    creationTimeAfter ::
      Lude.Maybe Lude.Timestamp,
    nextToken :: Lude.Maybe Lude.Text,
    sortOrder :: Lude.Maybe SortOrder,
    lastModifiedTimeAfter ::
      Lude.Maybe Lude.Timestamp,
    creationTimeBefore ::
      Lude.Maybe Lude.Timestamp,
    statusEquals ::
      Lude.Maybe
        HyperParameterTuningJobStatus,
    maxResults ::
      Lude.Maybe Lude.Natural,
    sortBy ::
      Lude.Maybe
        HyperParameterTuningJobSortByOptions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHyperParameterTuningJobs' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - A filter that returns only tuning jobs that were created after the specified time.
-- * 'creationTimeBefore' - A filter that returns only tuning jobs that were created before the specified time.
-- * 'lastModifiedTimeAfter' - A filter that returns only tuning jobs that were modified after the specified time.
-- * 'lastModifiedTimeBefore' - A filter that returns only tuning jobs that were modified before the specified time.
-- * 'maxResults' - The maximum number of tuning jobs to return. The default value is 10.
-- * 'nameContains' - A string in the tuning job name. This filter returns only tuning jobs whose name contains the specified string.
-- * 'nextToken' - If the result of the previous @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
-- * 'sortBy' - The field to sort results by. The default is @Name@ .
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'statusEquals' - A filter that returns only tuning jobs with the specified status.
mkListHyperParameterTuningJobs ::
  ListHyperParameterTuningJobs
mkListHyperParameterTuningJobs =
  ListHyperParameterTuningJobs'
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

-- | A string in the tuning job name. This filter returns only tuning jobs whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjNameContains :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe Lude.Text)
lhptjNameContains = Lens.lens (nameContains :: ListHyperParameterTuningJobs -> Lude.Maybe Lude.Text) (\s a -> s {nameContains = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A filter that returns only tuning jobs that were modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjLastModifiedTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe Lude.Timestamp)
lhptjLastModifiedTimeBefore = Lens.lens (lastModifiedTimeBefore :: ListHyperParameterTuningJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeBefore = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | A filter that returns only tuning jobs that were created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjCreationTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe Lude.Timestamp)
lhptjCreationTimeAfter = Lens.lens (creationTimeAfter :: ListHyperParameterTuningJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | If the result of the previous @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjNextToken :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe Lude.Text)
lhptjNextToken = Lens.lens (nextToken :: ListHyperParameterTuningJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjSortOrder :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe SortOrder)
lhptjSortOrder = Lens.lens (sortOrder :: ListHyperParameterTuningJobs -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only tuning jobs that were modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjLastModifiedTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe Lude.Timestamp)
lhptjLastModifiedTimeAfter = Lens.lens (lastModifiedTimeAfter :: ListHyperParameterTuningJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimeAfter = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only tuning jobs that were created before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjCreationTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe Lude.Timestamp)
lhptjCreationTimeBefore = Lens.lens (creationTimeBefore :: ListHyperParameterTuningJobs -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only tuning jobs with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjStatusEquals :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe HyperParameterTuningJobStatus)
lhptjStatusEquals = Lens.lens (statusEquals :: ListHyperParameterTuningJobs -> Lude.Maybe HyperParameterTuningJobStatus) (\s a -> s {statusEquals = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of tuning jobs to return. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjMaxResults :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe Lude.Natural)
lhptjMaxResults = Lens.lens (maxResults :: ListHyperParameterTuningJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @Name@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjSortBy :: Lens.Lens' ListHyperParameterTuningJobs (Lude.Maybe HyperParameterTuningJobSortByOptions)
lhptjSortBy = Lens.lens (sortBy :: ListHyperParameterTuningJobs -> Lude.Maybe HyperParameterTuningJobSortByOptions) (\s a -> s {sortBy = a} :: ListHyperParameterTuningJobs)
{-# DEPRECATED lhptjSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListHyperParameterTuningJobs where
  page rq rs
    | Page.stop (rs Lens.^. lhptjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lhptjrsHyperParameterTuningJobSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhptjNextToken Lens..~ rs Lens.^. lhptjrsNextToken

instance Lude.AWSRequest ListHyperParameterTuningJobs where
  type
    Rs ListHyperParameterTuningJobs =
      ListHyperParameterTuningJobsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListHyperParameterTuningJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..?> "HyperParameterTuningJobSummaries"
                         Lude..!@ Lude.mempty
                     )
      )

instance Lude.ToHeaders ListHyperParameterTuningJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListHyperParameterTuningJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListHyperParameterTuningJobs where
  toJSON ListHyperParameterTuningJobs' {..} =
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

instance Lude.ToPath ListHyperParameterTuningJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListHyperParameterTuningJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListHyperParameterTuningJobsResponse' smart constructor.
data ListHyperParameterTuningJobsResponse = ListHyperParameterTuningJobsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    hyperParameterTuningJobSummaries ::
      [HyperParameterTuningJobSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHyperParameterTuningJobsResponse' with the minimum fields required to make a request.
--
-- * 'hyperParameterTuningJobSummaries' - A list of 'HyperParameterTuningJobSummary' objects that describe the tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
-- * 'nextToken' - If the result of this @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
-- * 'responseStatus' - The response status code.
mkListHyperParameterTuningJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListHyperParameterTuningJobsResponse
mkListHyperParameterTuningJobsResponse pResponseStatus_ =
  ListHyperParameterTuningJobsResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      hyperParameterTuningJobSummaries = Lude.mempty
    }

-- | If the result of this @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrsNextToken :: Lens.Lens' ListHyperParameterTuningJobsResponse (Lude.Maybe Lude.Text)
lhptjrsNextToken = Lens.lens (nextToken :: ListHyperParameterTuningJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHyperParameterTuningJobsResponse)
{-# DEPRECATED lhptjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrsResponseStatus :: Lens.Lens' ListHyperParameterTuningJobsResponse Lude.Int
lhptjrsResponseStatus = Lens.lens (responseStatus :: ListHyperParameterTuningJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHyperParameterTuningJobsResponse)
{-# DEPRECATED lhptjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of 'HyperParameterTuningJobSummary' objects that describe the tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
--
-- /Note:/ Consider using 'hyperParameterTuningJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhptjrsHyperParameterTuningJobSummaries :: Lens.Lens' ListHyperParameterTuningJobsResponse [HyperParameterTuningJobSummary]
lhptjrsHyperParameterTuningJobSummaries = Lens.lens (hyperParameterTuningJobSummaries :: ListHyperParameterTuningJobsResponse -> [HyperParameterTuningJobSummary]) (\s a -> s {hyperParameterTuningJobSummaries = a} :: ListHyperParameterTuningJobsResponse)
{-# DEPRECATED lhptjrsHyperParameterTuningJobSummaries "Use generic-lens or generic-optics with 'hyperParameterTuningJobSummaries' instead." #-}
