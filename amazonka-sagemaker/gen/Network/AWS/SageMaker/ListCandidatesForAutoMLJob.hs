{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListCandidatesForAutoMLJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the Candidates created for the job.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCandidatesForAutoMLJob
  ( -- * Creating a Request
    ListCandidatesForAutoMLJob (..),
    newListCandidatesForAutoMLJob,

    -- * Request Lenses
    listCandidatesForAutoMLJob_sortOrder,
    listCandidatesForAutoMLJob_nextToken,
    listCandidatesForAutoMLJob_maxResults,
    listCandidatesForAutoMLJob_candidateNameEquals,
    listCandidatesForAutoMLJob_sortBy,
    listCandidatesForAutoMLJob_statusEquals,
    listCandidatesForAutoMLJob_autoMLJobName,

    -- * Destructuring the Response
    ListCandidatesForAutoMLJobResponse (..),
    newListCandidatesForAutoMLJobResponse,

    -- * Response Lenses
    listCandidatesForAutoMLJobResponse_nextToken,
    listCandidatesForAutoMLJobResponse_httpStatus,
    listCandidatesForAutoMLJobResponse_candidates,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListCandidatesForAutoMLJob' smart constructor.
data ListCandidatesForAutoMLJob = ListCandidatesForAutoMLJob'
  { -- | The sort order for the results. The default is Ascending.
    sortOrder :: Core.Maybe AutoMLSortOrder,
    -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | List the job\'s Candidates up to a specified limit.
    maxResults :: Core.Maybe Core.Natural,
    -- | List the Candidates for the job and filter by candidate name.
    candidateNameEquals :: Core.Maybe Core.Text,
    -- | The parameter by which to sort the results. The default is Descending.
    sortBy :: Core.Maybe CandidateSortBy,
    -- | List the Candidates for the job and filter by status.
    statusEquals :: Core.Maybe CandidateStatus,
    -- | List the Candidates created for the job by providing the job\'s name.
    autoMLJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCandidatesForAutoMLJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listCandidatesForAutoMLJob_sortOrder' - The sort order for the results. The default is Ascending.
--
-- 'nextToken', 'listCandidatesForAutoMLJob_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'maxResults', 'listCandidatesForAutoMLJob_maxResults' - List the job\'s Candidates up to a specified limit.
--
-- 'candidateNameEquals', 'listCandidatesForAutoMLJob_candidateNameEquals' - List the Candidates for the job and filter by candidate name.
--
-- 'sortBy', 'listCandidatesForAutoMLJob_sortBy' - The parameter by which to sort the results. The default is Descending.
--
-- 'statusEquals', 'listCandidatesForAutoMLJob_statusEquals' - List the Candidates for the job and filter by status.
--
-- 'autoMLJobName', 'listCandidatesForAutoMLJob_autoMLJobName' - List the Candidates created for the job by providing the job\'s name.
newListCandidatesForAutoMLJob ::
  -- | 'autoMLJobName'
  Core.Text ->
  ListCandidatesForAutoMLJob
newListCandidatesForAutoMLJob pAutoMLJobName_ =
  ListCandidatesForAutoMLJob'
    { sortOrder =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      candidateNameEquals = Core.Nothing,
      sortBy = Core.Nothing,
      statusEquals = Core.Nothing,
      autoMLJobName = pAutoMLJobName_
    }

-- | The sort order for the results. The default is Ascending.
listCandidatesForAutoMLJob_sortOrder :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe AutoMLSortOrder)
listCandidatesForAutoMLJob_sortOrder = Lens.lens (\ListCandidatesForAutoMLJob' {sortOrder} -> sortOrder) (\s@ListCandidatesForAutoMLJob' {} a -> s {sortOrder = a} :: ListCandidatesForAutoMLJob)

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listCandidatesForAutoMLJob_nextToken :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Core.Text)
listCandidatesForAutoMLJob_nextToken = Lens.lens (\ListCandidatesForAutoMLJob' {nextToken} -> nextToken) (\s@ListCandidatesForAutoMLJob' {} a -> s {nextToken = a} :: ListCandidatesForAutoMLJob)

-- | List the job\'s Candidates up to a specified limit.
listCandidatesForAutoMLJob_maxResults :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Core.Natural)
listCandidatesForAutoMLJob_maxResults = Lens.lens (\ListCandidatesForAutoMLJob' {maxResults} -> maxResults) (\s@ListCandidatesForAutoMLJob' {} a -> s {maxResults = a} :: ListCandidatesForAutoMLJob)

-- | List the Candidates for the job and filter by candidate name.
listCandidatesForAutoMLJob_candidateNameEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe Core.Text)
listCandidatesForAutoMLJob_candidateNameEquals = Lens.lens (\ListCandidatesForAutoMLJob' {candidateNameEquals} -> candidateNameEquals) (\s@ListCandidatesForAutoMLJob' {} a -> s {candidateNameEquals = a} :: ListCandidatesForAutoMLJob)

-- | The parameter by which to sort the results. The default is Descending.
listCandidatesForAutoMLJob_sortBy :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe CandidateSortBy)
listCandidatesForAutoMLJob_sortBy = Lens.lens (\ListCandidatesForAutoMLJob' {sortBy} -> sortBy) (\s@ListCandidatesForAutoMLJob' {} a -> s {sortBy = a} :: ListCandidatesForAutoMLJob)

-- | List the Candidates for the job and filter by status.
listCandidatesForAutoMLJob_statusEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Core.Maybe CandidateStatus)
listCandidatesForAutoMLJob_statusEquals = Lens.lens (\ListCandidatesForAutoMLJob' {statusEquals} -> statusEquals) (\s@ListCandidatesForAutoMLJob' {} a -> s {statusEquals = a} :: ListCandidatesForAutoMLJob)

-- | List the Candidates created for the job by providing the job\'s name.
listCandidatesForAutoMLJob_autoMLJobName :: Lens.Lens' ListCandidatesForAutoMLJob Core.Text
listCandidatesForAutoMLJob_autoMLJobName = Lens.lens (\ListCandidatesForAutoMLJob' {autoMLJobName} -> autoMLJobName) (\s@ListCandidatesForAutoMLJob' {} a -> s {autoMLJobName = a} :: ListCandidatesForAutoMLJob)

instance Core.AWSPager ListCandidatesForAutoMLJob where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCandidatesForAutoMLJobResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listCandidatesForAutoMLJobResponse_candidates
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCandidatesForAutoMLJob_nextToken
          Lens..~ rs
          Lens.^? listCandidatesForAutoMLJobResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListCandidatesForAutoMLJob where
  type
    AWSResponse ListCandidatesForAutoMLJob =
      ListCandidatesForAutoMLJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCandidatesForAutoMLJobResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Candidates" Core..!@ Core.mempty)
      )

instance Core.Hashable ListCandidatesForAutoMLJob

instance Core.NFData ListCandidatesForAutoMLJob

instance Core.ToHeaders ListCandidatesForAutoMLJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListCandidatesForAutoMLJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCandidatesForAutoMLJob where
  toJSON ListCandidatesForAutoMLJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CandidateNameEquals" Core..=)
              Core.<$> candidateNameEquals,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("StatusEquals" Core..=) Core.<$> statusEquals,
            Core.Just ("AutoMLJobName" Core..= autoMLJobName)
          ]
      )

instance Core.ToPath ListCandidatesForAutoMLJob where
  toPath = Core.const "/"

instance Core.ToQuery ListCandidatesForAutoMLJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCandidatesForAutoMLJobResponse' smart constructor.
data ListCandidatesForAutoMLJobResponse = ListCandidatesForAutoMLJobResponse'
  { -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Summaries about the Candidates.
    candidates :: [AutoMLCandidate]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCandidatesForAutoMLJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCandidatesForAutoMLJobResponse_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'httpStatus', 'listCandidatesForAutoMLJobResponse_httpStatus' - The response's http status code.
--
-- 'candidates', 'listCandidatesForAutoMLJobResponse_candidates' - Summaries about the Candidates.
newListCandidatesForAutoMLJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCandidatesForAutoMLJobResponse
newListCandidatesForAutoMLJobResponse pHttpStatus_ =
  ListCandidatesForAutoMLJobResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      candidates = Core.mempty
    }

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listCandidatesForAutoMLJobResponse_nextToken :: Lens.Lens' ListCandidatesForAutoMLJobResponse (Core.Maybe Core.Text)
listCandidatesForAutoMLJobResponse_nextToken = Lens.lens (\ListCandidatesForAutoMLJobResponse' {nextToken} -> nextToken) (\s@ListCandidatesForAutoMLJobResponse' {} a -> s {nextToken = a} :: ListCandidatesForAutoMLJobResponse)

-- | The response's http status code.
listCandidatesForAutoMLJobResponse_httpStatus :: Lens.Lens' ListCandidatesForAutoMLJobResponse Core.Int
listCandidatesForAutoMLJobResponse_httpStatus = Lens.lens (\ListCandidatesForAutoMLJobResponse' {httpStatus} -> httpStatus) (\s@ListCandidatesForAutoMLJobResponse' {} a -> s {httpStatus = a} :: ListCandidatesForAutoMLJobResponse)

-- | Summaries about the Candidates.
listCandidatesForAutoMLJobResponse_candidates :: Lens.Lens' ListCandidatesForAutoMLJobResponse [AutoMLCandidate]
listCandidatesForAutoMLJobResponse_candidates = Lens.lens (\ListCandidatesForAutoMLJobResponse' {candidates} -> candidates) (\s@ListCandidatesForAutoMLJobResponse' {} a -> s {candidates = a} :: ListCandidatesForAutoMLJobResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListCandidatesForAutoMLJobResponse
