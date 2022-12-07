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
-- Module      : Amazonka.SageMaker.ListInferenceRecommendationsJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists recommendation jobs that satisfy various filters.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListInferenceRecommendationsJobs
  ( -- * Creating a Request
    ListInferenceRecommendationsJobs (..),
    newListInferenceRecommendationsJobs,

    -- * Request Lenses
    listInferenceRecommendationsJobs_sortOrder,
    listInferenceRecommendationsJobs_nextToken,
    listInferenceRecommendationsJobs_lastModifiedTimeAfter,
    listInferenceRecommendationsJobs_nameContains,
    listInferenceRecommendationsJobs_lastModifiedTimeBefore,
    listInferenceRecommendationsJobs_creationTimeBefore,
    listInferenceRecommendationsJobs_sortBy,
    listInferenceRecommendationsJobs_maxResults,
    listInferenceRecommendationsJobs_statusEquals,
    listInferenceRecommendationsJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListInferenceRecommendationsJobsResponse (..),
    newListInferenceRecommendationsJobsResponse,

    -- * Response Lenses
    listInferenceRecommendationsJobsResponse_nextToken,
    listInferenceRecommendationsJobsResponse_httpStatus,
    listInferenceRecommendationsJobsResponse_inferenceRecommendationsJobs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListInferenceRecommendationsJobs' smart constructor.
data ListInferenceRecommendationsJobs = ListInferenceRecommendationsJobs'
  { -- | The sort order for the results.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the response to a previous @ListInferenceRecommendationsJobsRequest@
    -- request was truncated, the response includes a @NextToken@. To retrieve
    -- the next set of recommendations, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only jobs that were last modified after the
    -- specified time (timestamp).
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A string in the job name. This filter returns only recommendations whose
    -- name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only jobs that were last modified before the
    -- specified time (timestamp).
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only jobs created before the specified time
    -- (timestamp).
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The parameter by which to sort the results.
    sortBy :: Prelude.Maybe ListInferenceRecommendationsJobsSortBy,
    -- | The maximum number of recommendations to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that retrieves only inference recommendations jobs with a
    -- specific status.
    statusEquals :: Prelude.Maybe RecommendationJobStatus,
    -- | A filter that returns only jobs created after the specified time
    -- (timestamp).
    creationTimeAfter :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceRecommendationsJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listInferenceRecommendationsJobs_sortOrder' - The sort order for the results.
--
-- 'nextToken', 'listInferenceRecommendationsJobs_nextToken' - If the response to a previous @ListInferenceRecommendationsJobsRequest@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of recommendations, use the token in the next request.
--
-- 'lastModifiedTimeAfter', 'listInferenceRecommendationsJobs_lastModifiedTimeAfter' - A filter that returns only jobs that were last modified after the
-- specified time (timestamp).
--
-- 'nameContains', 'listInferenceRecommendationsJobs_nameContains' - A string in the job name. This filter returns only recommendations whose
-- name contains the specified string.
--
-- 'lastModifiedTimeBefore', 'listInferenceRecommendationsJobs_lastModifiedTimeBefore' - A filter that returns only jobs that were last modified before the
-- specified time (timestamp).
--
-- 'creationTimeBefore', 'listInferenceRecommendationsJobs_creationTimeBefore' - A filter that returns only jobs created before the specified time
-- (timestamp).
--
-- 'sortBy', 'listInferenceRecommendationsJobs_sortBy' - The parameter by which to sort the results.
--
-- 'maxResults', 'listInferenceRecommendationsJobs_maxResults' - The maximum number of recommendations to return in the response.
--
-- 'statusEquals', 'listInferenceRecommendationsJobs_statusEquals' - A filter that retrieves only inference recommendations jobs with a
-- specific status.
--
-- 'creationTimeAfter', 'listInferenceRecommendationsJobs_creationTimeAfter' - A filter that returns only jobs created after the specified time
-- (timestamp).
newListInferenceRecommendationsJobs ::
  ListInferenceRecommendationsJobs
newListInferenceRecommendationsJobs =
  ListInferenceRecommendationsJobs'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | The sort order for the results.
listInferenceRecommendationsJobs_sortOrder :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe SortOrder)
listInferenceRecommendationsJobs_sortOrder = Lens.lens (\ListInferenceRecommendationsJobs' {sortOrder} -> sortOrder) (\s@ListInferenceRecommendationsJobs' {} a -> s {sortOrder = a} :: ListInferenceRecommendationsJobs)

-- | If the response to a previous @ListInferenceRecommendationsJobsRequest@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of recommendations, use the token in the next request.
listInferenceRecommendationsJobs_nextToken :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.Text)
listInferenceRecommendationsJobs_nextToken = Lens.lens (\ListInferenceRecommendationsJobs' {nextToken} -> nextToken) (\s@ListInferenceRecommendationsJobs' {} a -> s {nextToken = a} :: ListInferenceRecommendationsJobs)

-- | A filter that returns only jobs that were last modified after the
-- specified time (timestamp).
listInferenceRecommendationsJobs_lastModifiedTimeAfter :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.UTCTime)
listInferenceRecommendationsJobs_lastModifiedTimeAfter = Lens.lens (\ListInferenceRecommendationsJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListInferenceRecommendationsJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListInferenceRecommendationsJobs) Prelude.. Lens.mapping Data._Time

-- | A string in the job name. This filter returns only recommendations whose
-- name contains the specified string.
listInferenceRecommendationsJobs_nameContains :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.Text)
listInferenceRecommendationsJobs_nameContains = Lens.lens (\ListInferenceRecommendationsJobs' {nameContains} -> nameContains) (\s@ListInferenceRecommendationsJobs' {} a -> s {nameContains = a} :: ListInferenceRecommendationsJobs)

-- | A filter that returns only jobs that were last modified before the
-- specified time (timestamp).
listInferenceRecommendationsJobs_lastModifiedTimeBefore :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.UTCTime)
listInferenceRecommendationsJobs_lastModifiedTimeBefore = Lens.lens (\ListInferenceRecommendationsJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListInferenceRecommendationsJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListInferenceRecommendationsJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only jobs created before the specified time
-- (timestamp).
listInferenceRecommendationsJobs_creationTimeBefore :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.UTCTime)
listInferenceRecommendationsJobs_creationTimeBefore = Lens.lens (\ListInferenceRecommendationsJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListInferenceRecommendationsJobs' {} a -> s {creationTimeBefore = a} :: ListInferenceRecommendationsJobs) Prelude.. Lens.mapping Data._Time

-- | The parameter by which to sort the results.
listInferenceRecommendationsJobs_sortBy :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe ListInferenceRecommendationsJobsSortBy)
listInferenceRecommendationsJobs_sortBy = Lens.lens (\ListInferenceRecommendationsJobs' {sortBy} -> sortBy) (\s@ListInferenceRecommendationsJobs' {} a -> s {sortBy = a} :: ListInferenceRecommendationsJobs)

-- | The maximum number of recommendations to return in the response.
listInferenceRecommendationsJobs_maxResults :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.Natural)
listInferenceRecommendationsJobs_maxResults = Lens.lens (\ListInferenceRecommendationsJobs' {maxResults} -> maxResults) (\s@ListInferenceRecommendationsJobs' {} a -> s {maxResults = a} :: ListInferenceRecommendationsJobs)

-- | A filter that retrieves only inference recommendations jobs with a
-- specific status.
listInferenceRecommendationsJobs_statusEquals :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe RecommendationJobStatus)
listInferenceRecommendationsJobs_statusEquals = Lens.lens (\ListInferenceRecommendationsJobs' {statusEquals} -> statusEquals) (\s@ListInferenceRecommendationsJobs' {} a -> s {statusEquals = a} :: ListInferenceRecommendationsJobs)

-- | A filter that returns only jobs created after the specified time
-- (timestamp).
listInferenceRecommendationsJobs_creationTimeAfter :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.UTCTime)
listInferenceRecommendationsJobs_creationTimeAfter = Lens.lens (\ListInferenceRecommendationsJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListInferenceRecommendationsJobs' {} a -> s {creationTimeAfter = a} :: ListInferenceRecommendationsJobs) Prelude.. Lens.mapping Data._Time

instance
  Core.AWSPager
    ListInferenceRecommendationsJobs
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInferenceRecommendationsJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listInferenceRecommendationsJobsResponse_inferenceRecommendationsJobs
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInferenceRecommendationsJobs_nextToken
          Lens..~ rs
          Lens.^? listInferenceRecommendationsJobsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListInferenceRecommendationsJobs
  where
  type
    AWSResponse ListInferenceRecommendationsJobs =
      ListInferenceRecommendationsJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInferenceRecommendationsJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "InferenceRecommendationsJobs"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListInferenceRecommendationsJobs
  where
  hashWithSalt
    _salt
    ListInferenceRecommendationsJobs' {..} =
      _salt `Prelude.hashWithSalt` sortOrder
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` lastModifiedTimeAfter
        `Prelude.hashWithSalt` nameContains
        `Prelude.hashWithSalt` lastModifiedTimeBefore
        `Prelude.hashWithSalt` creationTimeBefore
        `Prelude.hashWithSalt` sortBy
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` statusEquals
        `Prelude.hashWithSalt` creationTimeAfter

instance
  Prelude.NFData
    ListInferenceRecommendationsJobs
  where
  rnf ListInferenceRecommendationsJobs' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf creationTimeAfter

instance
  Data.ToHeaders
    ListInferenceRecommendationsJobs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListInferenceRecommendationsJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInferenceRecommendationsJobs where
  toJSON ListInferenceRecommendationsJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals,
            ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Data.ToPath ListInferenceRecommendationsJobs where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListInferenceRecommendationsJobs
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInferenceRecommendationsJobsResponse' smart constructor.
data ListInferenceRecommendationsJobsResponse = ListInferenceRecommendationsJobsResponse'
  { -- | A token for getting the next set of recommendations, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The recommendations created from the Amazon SageMaker Inference
    -- Recommender job.
    inferenceRecommendationsJobs :: [InferenceRecommendationsJob]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceRecommendationsJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInferenceRecommendationsJobsResponse_nextToken' - A token for getting the next set of recommendations, if there are any.
--
-- 'httpStatus', 'listInferenceRecommendationsJobsResponse_httpStatus' - The response's http status code.
--
-- 'inferenceRecommendationsJobs', 'listInferenceRecommendationsJobsResponse_inferenceRecommendationsJobs' - The recommendations created from the Amazon SageMaker Inference
-- Recommender job.
newListInferenceRecommendationsJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInferenceRecommendationsJobsResponse
newListInferenceRecommendationsJobsResponse
  pHttpStatus_ =
    ListInferenceRecommendationsJobsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        inferenceRecommendationsJobs =
          Prelude.mempty
      }

-- | A token for getting the next set of recommendations, if there are any.
listInferenceRecommendationsJobsResponse_nextToken :: Lens.Lens' ListInferenceRecommendationsJobsResponse (Prelude.Maybe Prelude.Text)
listInferenceRecommendationsJobsResponse_nextToken = Lens.lens (\ListInferenceRecommendationsJobsResponse' {nextToken} -> nextToken) (\s@ListInferenceRecommendationsJobsResponse' {} a -> s {nextToken = a} :: ListInferenceRecommendationsJobsResponse)

-- | The response's http status code.
listInferenceRecommendationsJobsResponse_httpStatus :: Lens.Lens' ListInferenceRecommendationsJobsResponse Prelude.Int
listInferenceRecommendationsJobsResponse_httpStatus = Lens.lens (\ListInferenceRecommendationsJobsResponse' {httpStatus} -> httpStatus) (\s@ListInferenceRecommendationsJobsResponse' {} a -> s {httpStatus = a} :: ListInferenceRecommendationsJobsResponse)

-- | The recommendations created from the Amazon SageMaker Inference
-- Recommender job.
listInferenceRecommendationsJobsResponse_inferenceRecommendationsJobs :: Lens.Lens' ListInferenceRecommendationsJobsResponse [InferenceRecommendationsJob]
listInferenceRecommendationsJobsResponse_inferenceRecommendationsJobs = Lens.lens (\ListInferenceRecommendationsJobsResponse' {inferenceRecommendationsJobs} -> inferenceRecommendationsJobs) (\s@ListInferenceRecommendationsJobsResponse' {} a -> s {inferenceRecommendationsJobs = a} :: ListInferenceRecommendationsJobsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListInferenceRecommendationsJobsResponse
  where
  rnf ListInferenceRecommendationsJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf inferenceRecommendationsJobs
