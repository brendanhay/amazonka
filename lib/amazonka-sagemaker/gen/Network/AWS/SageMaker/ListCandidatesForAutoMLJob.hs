{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListCandidatesForAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the Candidates created for the job.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCandidatesForAutoMLJob
  ( -- * Creating a request
    ListCandidatesForAutoMLJob (..),
    mkListCandidatesForAutoMLJob,

    -- ** Request lenses
    lcfamljCandidateNameEquals,
    lcfamljAutoMLJobName,
    lcfamljNextToken,
    lcfamljSortOrder,
    lcfamljStatusEquals,
    lcfamljMaxResults,
    lcfamljSortBy,

    -- * Destructuring the response
    ListCandidatesForAutoMLJobResponse (..),
    mkListCandidatesForAutoMLJobResponse,

    -- ** Response lenses
    lcfamljrsNextToken,
    lcfamljrsCandidates,
    lcfamljrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListCandidatesForAutoMLJob' smart constructor.
data ListCandidatesForAutoMLJob = ListCandidatesForAutoMLJob'
  { -- | List the Candidates for the job and filter by candidate name.
    candidateNameEquals :: Lude.Maybe Lude.Text,
    -- | List the Candidates created for the job by providing the job's name.
    autoMLJobName :: Lude.Text,
    -- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The sort order for the results. The default is Ascending.
    sortOrder :: Lude.Maybe AutoMLSortOrder,
    -- | List the Candidates for the job and filter by status.
    statusEquals :: Lude.Maybe CandidateStatus,
    -- | List the job's Candidates up to a specified limit.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The parameter by which to sort the results. The default is Descending.
    sortBy :: Lude.Maybe CandidateSortBy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCandidatesForAutoMLJob' with the minimum fields required to make a request.
--
-- * 'candidateNameEquals' - List the Candidates for the job and filter by candidate name.
-- * 'autoMLJobName' - List the Candidates created for the job by providing the job's name.
-- * 'nextToken' - If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
-- * 'sortOrder' - The sort order for the results. The default is Ascending.
-- * 'statusEquals' - List the Candidates for the job and filter by status.
-- * 'maxResults' - List the job's Candidates up to a specified limit.
-- * 'sortBy' - The parameter by which to sort the results. The default is Descending.
mkListCandidatesForAutoMLJob ::
  -- | 'autoMLJobName'
  Lude.Text ->
  ListCandidatesForAutoMLJob
mkListCandidatesForAutoMLJob pAutoMLJobName_ =
  ListCandidatesForAutoMLJob'
    { candidateNameEquals = Lude.Nothing,
      autoMLJobName = pAutoMLJobName_,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      statusEquals = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing
    }

-- | List the Candidates for the job and filter by candidate name.
--
-- /Note:/ Consider using 'candidateNameEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljCandidateNameEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Lude.Maybe Lude.Text)
lcfamljCandidateNameEquals = Lens.lens (candidateNameEquals :: ListCandidatesForAutoMLJob -> Lude.Maybe Lude.Text) (\s a -> s {candidateNameEquals = a} :: ListCandidatesForAutoMLJob)
{-# DEPRECATED lcfamljCandidateNameEquals "Use generic-lens or generic-optics with 'candidateNameEquals' instead." #-}

-- | List the Candidates created for the job by providing the job's name.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljAutoMLJobName :: Lens.Lens' ListCandidatesForAutoMLJob Lude.Text
lcfamljAutoMLJobName = Lens.lens (autoMLJobName :: ListCandidatesForAutoMLJob -> Lude.Text) (\s a -> s {autoMLJobName = a} :: ListCandidatesForAutoMLJob)
{-# DEPRECATED lcfamljAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljNextToken :: Lens.Lens' ListCandidatesForAutoMLJob (Lude.Maybe Lude.Text)
lcfamljNextToken = Lens.lens (nextToken :: ListCandidatesForAutoMLJob -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCandidatesForAutoMLJob)
{-# DEPRECATED lcfamljNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order for the results. The default is Ascending.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljSortOrder :: Lens.Lens' ListCandidatesForAutoMLJob (Lude.Maybe AutoMLSortOrder)
lcfamljSortOrder = Lens.lens (sortOrder :: ListCandidatesForAutoMLJob -> Lude.Maybe AutoMLSortOrder) (\s a -> s {sortOrder = a} :: ListCandidatesForAutoMLJob)
{-# DEPRECATED lcfamljSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | List the Candidates for the job and filter by status.
--
-- /Note:/ Consider using 'statusEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljStatusEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Lude.Maybe CandidateStatus)
lcfamljStatusEquals = Lens.lens (statusEquals :: ListCandidatesForAutoMLJob -> Lude.Maybe CandidateStatus) (\s a -> s {statusEquals = a} :: ListCandidatesForAutoMLJob)
{-# DEPRECATED lcfamljStatusEquals "Use generic-lens or generic-optics with 'statusEquals' instead." #-}

-- | List the job's Candidates up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljMaxResults :: Lens.Lens' ListCandidatesForAutoMLJob (Lude.Maybe Lude.Natural)
lcfamljMaxResults = Lens.lens (maxResults :: ListCandidatesForAutoMLJob -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCandidatesForAutoMLJob)
{-# DEPRECATED lcfamljMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The parameter by which to sort the results. The default is Descending.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljSortBy :: Lens.Lens' ListCandidatesForAutoMLJob (Lude.Maybe CandidateSortBy)
lcfamljSortBy = Lens.lens (sortBy :: ListCandidatesForAutoMLJob -> Lude.Maybe CandidateSortBy) (\s a -> s {sortBy = a} :: ListCandidatesForAutoMLJob)
{-# DEPRECATED lcfamljSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Page.AWSPager ListCandidatesForAutoMLJob where
  page rq rs
    | Page.stop (rs Lens.^. lcfamljrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcfamljrsCandidates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcfamljNextToken Lens..~ rs Lens.^. lcfamljrsNextToken

instance Lude.AWSRequest ListCandidatesForAutoMLJob where
  type
    Rs ListCandidatesForAutoMLJob =
      ListCandidatesForAutoMLJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCandidatesForAutoMLJobResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Candidates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCandidatesForAutoMLJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListCandidatesForAutoMLJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCandidatesForAutoMLJob where
  toJSON ListCandidatesForAutoMLJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CandidateNameEquals" Lude..=) Lude.<$> candidateNameEquals,
            Lude.Just ("AutoMLJobName" Lude..= autoMLJobName),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("StatusEquals" Lude..=) Lude.<$> statusEquals,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("SortBy" Lude..=) Lude.<$> sortBy
          ]
      )

instance Lude.ToPath ListCandidatesForAutoMLJob where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCandidatesForAutoMLJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCandidatesForAutoMLJobResponse' smart constructor.
data ListCandidatesForAutoMLJobResponse = ListCandidatesForAutoMLJobResponse'
  { -- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Summaries about the Candidates.
    candidates :: [AutoMLCandidate],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCandidatesForAutoMLJobResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
-- * 'candidates' - Summaries about the Candidates.
-- * 'responseStatus' - The response status code.
mkListCandidatesForAutoMLJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCandidatesForAutoMLJobResponse
mkListCandidatesForAutoMLJobResponse pResponseStatus_ =
  ListCandidatesForAutoMLJobResponse'
    { nextToken = Lude.Nothing,
      candidates = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrsNextToken :: Lens.Lens' ListCandidatesForAutoMLJobResponse (Lude.Maybe Lude.Text)
lcfamljrsNextToken = Lens.lens (nextToken :: ListCandidatesForAutoMLJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCandidatesForAutoMLJobResponse)
{-# DEPRECATED lcfamljrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Summaries about the Candidates.
--
-- /Note:/ Consider using 'candidates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrsCandidates :: Lens.Lens' ListCandidatesForAutoMLJobResponse [AutoMLCandidate]
lcfamljrsCandidates = Lens.lens (candidates :: ListCandidatesForAutoMLJobResponse -> [AutoMLCandidate]) (\s a -> s {candidates = a} :: ListCandidatesForAutoMLJobResponse)
{-# DEPRECATED lcfamljrsCandidates "Use generic-lens or generic-optics with 'candidates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfamljrsResponseStatus :: Lens.Lens' ListCandidatesForAutoMLJobResponse Lude.Int
lcfamljrsResponseStatus = Lens.lens (responseStatus :: ListCandidatesForAutoMLJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCandidatesForAutoMLJobResponse)
{-# DEPRECATED lcfamljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
