{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of 'TrainingJobSummary' objects that describe the training jobs that a hyperparameter tuning job launched.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
  ( -- * Creating a request
    ListTrainingJobsForHyperParameterTuningJob (..),
    mkListTrainingJobsForHyperParameterTuningJob,

    -- ** Request lenses
    ltjfhptjNextToken,
    ltjfhptjSortOrder,
    ltjfhptjStatusEquals,
    ltjfhptjMaxResults,
    ltjfhptjSortBy,
    ltjfhptjHyperParameterTuningJobName,

    -- * Destructuring the response
    ListTrainingJobsForHyperParameterTuningJobResponse (..),
    mkListTrainingJobsForHyperParameterTuningJobResponse,

    -- ** Response lenses
    ltjfhptjrsNextToken,
    ltjfhptjrsResponseStatus,
    ltjfhptjrsTrainingJobSummaries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListTrainingJobsForHyperParameterTuningJob' smart constructor.
data ListTrainingJobsForHyperParameterTuningJob = ListTrainingJobsForHyperParameterTuningJob'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    sortOrder ::
      Lude.Maybe
        SortOrder,
    statusEquals ::
      Lude.Maybe
        TrainingJobStatus,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    sortBy ::
      Lude.Maybe
        TrainingJobSortByOptions,
    hyperParameterTuningJobName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrainingJobsForHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- * 'hyperParameterTuningJobName' - The name of the tuning job whose training jobs you want to list.
-- * 'maxResults' - The maximum number of training jobs to return. The default value is 10.
-- * 'nextToken' - If the result of the previous @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
-- * 'sortBy' - The field to sort results by. The default is @Name@ .
--
-- If the value of this field is @FinalObjectiveMetricValue@ , any training jobs that did not return an objective metric are not listed.
-- * 'sortOrder' - The sort order for results. The default is @Ascending@ .
-- * 'statusEquals' - A filter that returns only training jobs with the specified status.
mkListTrainingJobsForHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Lude.Text ->
  ListTrainingJobsForHyperParameterTuningJob
mkListTrainingJobsForHyperParameterTuningJob
  pHyperParameterTuningJobName_ =
    ListTrainingJobsForHyperParameterTuningJob'
      { nextToken =
          Lude.Nothing,
        sortOrder = Lude.Nothing,
        statusEquals = Lude.Nothing,
        maxResults = Lude.Nothing,
        sortBy = Lude.Nothing,
        hyperParameterTuningJobName =
          pHyperParameterTuningJobName_
      }

-- | If the result of the previous @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjNextToken :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Lude.Maybe Lude.Text)
ltjfhptjNextToken = Lens.lens (nextToken :: ListTrainingJobsForHyperParameterTuningJob -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrainingJobsForHyperParameterTuningJob)
{-# DEPRECATED ltjfhptjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjSortOrder :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Lude.Maybe SortOrder)
ltjfhptjSortOrder = Lens.lens (sortOrder :: ListTrainingJobsForHyperParameterTuningJob -> Lude.Maybe SortOrder) (\s a -> s {sortOrder = a} :: ListTrainingJobsForHyperParameterTuningJob)
{-# DEPRECATED ltjfhptjSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | A filter that returns only training jobs with the specified status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjStatusEquals :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Lude.Maybe TrainingJobStatus)
ltjfhptjStatusEquals = Lens.lens (statusEquals :: ListTrainingJobsForHyperParameterTuningJob -> Lude.Maybe TrainingJobStatus) (\s a -> s {statusEquals = a} :: ListTrainingJobsForHyperParameterTuningJob)
{-# DEPRECATED ltjfhptjStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | The maximum number of training jobs to return. The default value is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjMaxResults :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Lude.Maybe Lude.Natural)
ltjfhptjMaxResults = Lens.lens (maxResults :: ListTrainingJobsForHyperParameterTuningJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTrainingJobsForHyperParameterTuningJob)
{-# DEPRECATED ltjfhptjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The field to sort results by. The default is @Name@ .
--
-- If the value of this field is @FinalObjectiveMetricValue@ , any training jobs that did not return an objective metric are not listed.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjSortBy :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Lude.Maybe TrainingJobSortByOptions)
ltjfhptjSortBy = Lens.lens (sortBy :: ListTrainingJobsForHyperParameterTuningJob -> Lude.Maybe TrainingJobSortByOptions) (\s a -> s {sortBy = a} :: ListTrainingJobsForHyperParameterTuningJob)
{-# DEPRECATED ltjfhptjSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The name of the tuning job whose training jobs you want to list.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjHyperParameterTuningJobName :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob Lude.Text
ltjfhptjHyperParameterTuningJobName = Lens.lens (hyperParameterTuningJobName :: ListTrainingJobsForHyperParameterTuningJob -> Lude.Text) (\s a -> s {hyperParameterTuningJobName = a} :: ListTrainingJobsForHyperParameterTuningJob)
{-# DEPRECATED ltjfhptjHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

instance Page.AWSPager ListTrainingJobsForHyperParameterTuningJob where
  page rq rs
    | Page.stop (rs Lens.^. ltjfhptjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltjfhptjrsTrainingJobSummaries) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltjfhptjNextToken Lens..~ rs Lens.^. ltjfhptjrsNextToken

instance Lude.AWSRequest ListTrainingJobsForHyperParameterTuningJob where
  type
    Rs ListTrainingJobsForHyperParameterTuningJob =
      ListTrainingJobsForHyperParameterTuningJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTrainingJobsForHyperParameterTuningJobResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "TrainingJobSummaries" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListTrainingJobsForHyperParameterTuningJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SageMaker.ListTrainingJobsForHyperParameterTuningJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTrainingJobsForHyperParameterTuningJob where
  toJSON ListTrainingJobsForHyperParameterTuningJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("StatusEquals" Lude..=) Lude.<$> statusEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy,
            Lude.Just
              ( "HyperParameterTuningJobName"
                  Lude..= hyperParameterTuningJobName
              )
          ]
      )

instance Lude.ToPath ListTrainingJobsForHyperParameterTuningJob where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTrainingJobsForHyperParameterTuningJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTrainingJobsForHyperParameterTuningJobResponse' smart constructor.
data ListTrainingJobsForHyperParameterTuningJobResponse = ListTrainingJobsForHyperParameterTuningJobResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    trainingJobSummaries ::
      [HyperParameterTrainingJobSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ListTrainingJobsForHyperParameterTuningJobResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the result of this @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
-- * 'responseStatus' - The response status code.
-- * 'trainingJobSummaries' - A list of 'TrainingJobSummary' objects that describe the training jobs that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
mkListTrainingJobsForHyperParameterTuningJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTrainingJobsForHyperParameterTuningJobResponse
mkListTrainingJobsForHyperParameterTuningJobResponse
  pResponseStatus_ =
    ListTrainingJobsForHyperParameterTuningJobResponse'
      { nextToken =
          Lude.Nothing,
        responseStatus = pResponseStatus_,
        trainingJobSummaries = Lude.mempty
      }

-- | If the result of this @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjrsNextToken :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse (Lude.Maybe Lude.Text)
ltjfhptjrsNextToken = Lens.lens (nextToken :: ListTrainingJobsForHyperParameterTuningJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTrainingJobsForHyperParameterTuningJobResponse)
{-# DEPRECATED ltjfhptjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjrsResponseStatus :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse Lude.Int
ltjfhptjrsResponseStatus = Lens.lens (responseStatus :: ListTrainingJobsForHyperParameterTuningJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrainingJobsForHyperParameterTuningJobResponse)
{-# DEPRECATED ltjfhptjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of 'TrainingJobSummary' objects that describe the training jobs that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
--
-- /Note:/ Consider using 'trainingJobSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltjfhptjrsTrainingJobSummaries :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse [HyperParameterTrainingJobSummary]
ltjfhptjrsTrainingJobSummaries = Lens.lens (trainingJobSummaries :: ListTrainingJobsForHyperParameterTuningJobResponse -> [HyperParameterTrainingJobSummary]) (\s a -> s {trainingJobSummaries = a} :: ListTrainingJobsForHyperParameterTuningJobResponse)
{-# DEPRECATED ltjfhptjrsTrainingJobSummaries "Use generic-lens or generic-optics with 'trainingJobSummaries' instead." #-}
