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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListCandidatesForAutoMLJob' smart constructor.
data ListCandidatesForAutoMLJob = ListCandidatesForAutoMLJob'
  { -- | The sort order for the results. The default is Ascending.
    sortOrder :: Prelude.Maybe AutoMLSortOrder,
    -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List the job\'s Candidates up to a specified limit.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | List the Candidates for the job and filter by candidate name.
    candidateNameEquals :: Prelude.Maybe Prelude.Text,
    -- | The parameter by which to sort the results. The default is Descending.
    sortBy :: Prelude.Maybe CandidateSortBy,
    -- | List the Candidates for the job and filter by status.
    statusEquals :: Prelude.Maybe CandidateStatus,
    -- | List the Candidates created for the job by providing the job\'s name.
    autoMLJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListCandidatesForAutoMLJob
newListCandidatesForAutoMLJob pAutoMLJobName_ =
  ListCandidatesForAutoMLJob'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      candidateNameEquals = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      autoMLJobName = pAutoMLJobName_
    }

-- | The sort order for the results. The default is Ascending.
listCandidatesForAutoMLJob_sortOrder :: Lens.Lens' ListCandidatesForAutoMLJob (Prelude.Maybe AutoMLSortOrder)
listCandidatesForAutoMLJob_sortOrder = Lens.lens (\ListCandidatesForAutoMLJob' {sortOrder} -> sortOrder) (\s@ListCandidatesForAutoMLJob' {} a -> s {sortOrder = a} :: ListCandidatesForAutoMLJob)

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listCandidatesForAutoMLJob_nextToken :: Lens.Lens' ListCandidatesForAutoMLJob (Prelude.Maybe Prelude.Text)
listCandidatesForAutoMLJob_nextToken = Lens.lens (\ListCandidatesForAutoMLJob' {nextToken} -> nextToken) (\s@ListCandidatesForAutoMLJob' {} a -> s {nextToken = a} :: ListCandidatesForAutoMLJob)

-- | List the job\'s Candidates up to a specified limit.
listCandidatesForAutoMLJob_maxResults :: Lens.Lens' ListCandidatesForAutoMLJob (Prelude.Maybe Prelude.Natural)
listCandidatesForAutoMLJob_maxResults = Lens.lens (\ListCandidatesForAutoMLJob' {maxResults} -> maxResults) (\s@ListCandidatesForAutoMLJob' {} a -> s {maxResults = a} :: ListCandidatesForAutoMLJob)

-- | List the Candidates for the job and filter by candidate name.
listCandidatesForAutoMLJob_candidateNameEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Prelude.Maybe Prelude.Text)
listCandidatesForAutoMLJob_candidateNameEquals = Lens.lens (\ListCandidatesForAutoMLJob' {candidateNameEquals} -> candidateNameEquals) (\s@ListCandidatesForAutoMLJob' {} a -> s {candidateNameEquals = a} :: ListCandidatesForAutoMLJob)

-- | The parameter by which to sort the results. The default is Descending.
listCandidatesForAutoMLJob_sortBy :: Lens.Lens' ListCandidatesForAutoMLJob (Prelude.Maybe CandidateSortBy)
listCandidatesForAutoMLJob_sortBy = Lens.lens (\ListCandidatesForAutoMLJob' {sortBy} -> sortBy) (\s@ListCandidatesForAutoMLJob' {} a -> s {sortBy = a} :: ListCandidatesForAutoMLJob)

-- | List the Candidates for the job and filter by status.
listCandidatesForAutoMLJob_statusEquals :: Lens.Lens' ListCandidatesForAutoMLJob (Prelude.Maybe CandidateStatus)
listCandidatesForAutoMLJob_statusEquals = Lens.lens (\ListCandidatesForAutoMLJob' {statusEquals} -> statusEquals) (\s@ListCandidatesForAutoMLJob' {} a -> s {statusEquals = a} :: ListCandidatesForAutoMLJob)

-- | List the Candidates created for the job by providing the job\'s name.
listCandidatesForAutoMLJob_autoMLJobName :: Lens.Lens' ListCandidatesForAutoMLJob Prelude.Text
listCandidatesForAutoMLJob_autoMLJobName = Lens.lens (\ListCandidatesForAutoMLJob' {autoMLJobName} -> autoMLJobName) (\s@ListCandidatesForAutoMLJob' {} a -> s {autoMLJobName = a} :: ListCandidatesForAutoMLJob)

instance Core.AWSPager ListCandidatesForAutoMLJob where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCandidatesForAutoMLJobResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listCandidatesForAutoMLJobResponse_candidates
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCandidatesForAutoMLJob_nextToken
          Lens..~ rs
          Lens.^? listCandidatesForAutoMLJobResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCandidatesForAutoMLJob where
  type
    AWSResponse ListCandidatesForAutoMLJob =
      ListCandidatesForAutoMLJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCandidatesForAutoMLJobResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Candidates" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListCandidatesForAutoMLJob

instance Prelude.NFData ListCandidatesForAutoMLJob

instance Core.ToHeaders ListCandidatesForAutoMLJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListCandidatesForAutoMLJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCandidatesForAutoMLJob where
  toJSON ListCandidatesForAutoMLJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CandidateNameEquals" Core..=)
              Prelude.<$> candidateNameEquals,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            Prelude.Just
              ("AutoMLJobName" Core..= autoMLJobName)
          ]
      )

instance Core.ToPath ListCandidatesForAutoMLJob where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCandidatesForAutoMLJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCandidatesForAutoMLJobResponse' smart constructor.
data ListCandidatesForAutoMLJobResponse = ListCandidatesForAutoMLJobResponse'
  { -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summaries about the Candidates.
    candidates :: [AutoMLCandidate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListCandidatesForAutoMLJobResponse
newListCandidatesForAutoMLJobResponse pHttpStatus_ =
  ListCandidatesForAutoMLJobResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      candidates = Prelude.mempty
    }

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listCandidatesForAutoMLJobResponse_nextToken :: Lens.Lens' ListCandidatesForAutoMLJobResponse (Prelude.Maybe Prelude.Text)
listCandidatesForAutoMLJobResponse_nextToken = Lens.lens (\ListCandidatesForAutoMLJobResponse' {nextToken} -> nextToken) (\s@ListCandidatesForAutoMLJobResponse' {} a -> s {nextToken = a} :: ListCandidatesForAutoMLJobResponse)

-- | The response's http status code.
listCandidatesForAutoMLJobResponse_httpStatus :: Lens.Lens' ListCandidatesForAutoMLJobResponse Prelude.Int
listCandidatesForAutoMLJobResponse_httpStatus = Lens.lens (\ListCandidatesForAutoMLJobResponse' {httpStatus} -> httpStatus) (\s@ListCandidatesForAutoMLJobResponse' {} a -> s {httpStatus = a} :: ListCandidatesForAutoMLJobResponse)

-- | Summaries about the Candidates.
listCandidatesForAutoMLJobResponse_candidates :: Lens.Lens' ListCandidatesForAutoMLJobResponse [AutoMLCandidate]
listCandidatesForAutoMLJobResponse_candidates = Lens.lens (\ListCandidatesForAutoMLJobResponse' {candidates} -> candidates) (\s@ListCandidatesForAutoMLJobResponse' {} a -> s {candidates = a} :: ListCandidatesForAutoMLJobResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListCandidatesForAutoMLJobResponse
