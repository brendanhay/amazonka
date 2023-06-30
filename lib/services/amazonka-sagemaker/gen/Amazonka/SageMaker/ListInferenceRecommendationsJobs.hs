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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listInferenceRecommendationsJobs_creationTimeAfter,
    listInferenceRecommendationsJobs_creationTimeBefore,
    listInferenceRecommendationsJobs_lastModifiedTimeAfter,
    listInferenceRecommendationsJobs_lastModifiedTimeBefore,
    listInferenceRecommendationsJobs_maxResults,
    listInferenceRecommendationsJobs_nameContains,
    listInferenceRecommendationsJobs_nextToken,
    listInferenceRecommendationsJobs_sortBy,
    listInferenceRecommendationsJobs_sortOrder,
    listInferenceRecommendationsJobs_statusEquals,

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
  { -- | A filter that returns only jobs created after the specified time
    -- (timestamp).
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only jobs created before the specified time
    -- (timestamp).
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only jobs that were last modified after the
    -- specified time (timestamp).
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only jobs that were last modified before the
    -- specified time (timestamp).
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of recommendations to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string in the job name. This filter returns only recommendations whose
    -- name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the response to a previous @ListInferenceRecommendationsJobsRequest@
    -- request was truncated, the response includes a @NextToken@. To retrieve
    -- the next set of recommendations, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parameter by which to sort the results.
    sortBy :: Prelude.Maybe ListInferenceRecommendationsJobsSortBy,
    -- | The sort order for the results.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that retrieves only inference recommendations jobs with a
    -- specific status.
    statusEquals :: Prelude.Maybe RecommendationJobStatus
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
-- 'creationTimeAfter', 'listInferenceRecommendationsJobs_creationTimeAfter' - A filter that returns only jobs created after the specified time
-- (timestamp).
--
-- 'creationTimeBefore', 'listInferenceRecommendationsJobs_creationTimeBefore' - A filter that returns only jobs created before the specified time
-- (timestamp).
--
-- 'lastModifiedTimeAfter', 'listInferenceRecommendationsJobs_lastModifiedTimeAfter' - A filter that returns only jobs that were last modified after the
-- specified time (timestamp).
--
-- 'lastModifiedTimeBefore', 'listInferenceRecommendationsJobs_lastModifiedTimeBefore' - A filter that returns only jobs that were last modified before the
-- specified time (timestamp).
--
-- 'maxResults', 'listInferenceRecommendationsJobs_maxResults' - The maximum number of recommendations to return in the response.
--
-- 'nameContains', 'listInferenceRecommendationsJobs_nameContains' - A string in the job name. This filter returns only recommendations whose
-- name contains the specified string.
--
-- 'nextToken', 'listInferenceRecommendationsJobs_nextToken' - If the response to a previous @ListInferenceRecommendationsJobsRequest@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of recommendations, use the token in the next request.
--
-- 'sortBy', 'listInferenceRecommendationsJobs_sortBy' - The parameter by which to sort the results.
--
-- 'sortOrder', 'listInferenceRecommendationsJobs_sortOrder' - The sort order for the results.
--
-- 'statusEquals', 'listInferenceRecommendationsJobs_statusEquals' - A filter that retrieves only inference recommendations jobs with a
-- specific status.
newListInferenceRecommendationsJobs ::
  ListInferenceRecommendationsJobs
newListInferenceRecommendationsJobs =
  ListInferenceRecommendationsJobs'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | A filter that returns only jobs created after the specified time
-- (timestamp).
listInferenceRecommendationsJobs_creationTimeAfter :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.UTCTime)
listInferenceRecommendationsJobs_creationTimeAfter = Lens.lens (\ListInferenceRecommendationsJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListInferenceRecommendationsJobs' {} a -> s {creationTimeAfter = a} :: ListInferenceRecommendationsJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only jobs created before the specified time
-- (timestamp).
listInferenceRecommendationsJobs_creationTimeBefore :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.UTCTime)
listInferenceRecommendationsJobs_creationTimeBefore = Lens.lens (\ListInferenceRecommendationsJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListInferenceRecommendationsJobs' {} a -> s {creationTimeBefore = a} :: ListInferenceRecommendationsJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only jobs that were last modified after the
-- specified time (timestamp).
listInferenceRecommendationsJobs_lastModifiedTimeAfter :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.UTCTime)
listInferenceRecommendationsJobs_lastModifiedTimeAfter = Lens.lens (\ListInferenceRecommendationsJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListInferenceRecommendationsJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListInferenceRecommendationsJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only jobs that were last modified before the
-- specified time (timestamp).
listInferenceRecommendationsJobs_lastModifiedTimeBefore :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.UTCTime)
listInferenceRecommendationsJobs_lastModifiedTimeBefore = Lens.lens (\ListInferenceRecommendationsJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListInferenceRecommendationsJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListInferenceRecommendationsJobs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of recommendations to return in the response.
listInferenceRecommendationsJobs_maxResults :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.Natural)
listInferenceRecommendationsJobs_maxResults = Lens.lens (\ListInferenceRecommendationsJobs' {maxResults} -> maxResults) (\s@ListInferenceRecommendationsJobs' {} a -> s {maxResults = a} :: ListInferenceRecommendationsJobs)

-- | A string in the job name. This filter returns only recommendations whose
-- name contains the specified string.
listInferenceRecommendationsJobs_nameContains :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.Text)
listInferenceRecommendationsJobs_nameContains = Lens.lens (\ListInferenceRecommendationsJobs' {nameContains} -> nameContains) (\s@ListInferenceRecommendationsJobs' {} a -> s {nameContains = a} :: ListInferenceRecommendationsJobs)

-- | If the response to a previous @ListInferenceRecommendationsJobsRequest@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of recommendations, use the token in the next request.
listInferenceRecommendationsJobs_nextToken :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe Prelude.Text)
listInferenceRecommendationsJobs_nextToken = Lens.lens (\ListInferenceRecommendationsJobs' {nextToken} -> nextToken) (\s@ListInferenceRecommendationsJobs' {} a -> s {nextToken = a} :: ListInferenceRecommendationsJobs)

-- | The parameter by which to sort the results.
listInferenceRecommendationsJobs_sortBy :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe ListInferenceRecommendationsJobsSortBy)
listInferenceRecommendationsJobs_sortBy = Lens.lens (\ListInferenceRecommendationsJobs' {sortBy} -> sortBy) (\s@ListInferenceRecommendationsJobs' {} a -> s {sortBy = a} :: ListInferenceRecommendationsJobs)

-- | The sort order for the results.
listInferenceRecommendationsJobs_sortOrder :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe SortOrder)
listInferenceRecommendationsJobs_sortOrder = Lens.lens (\ListInferenceRecommendationsJobs' {sortOrder} -> sortOrder) (\s@ListInferenceRecommendationsJobs' {} a -> s {sortOrder = a} :: ListInferenceRecommendationsJobs)

-- | A filter that retrieves only inference recommendations jobs with a
-- specific status.
listInferenceRecommendationsJobs_statusEquals :: Lens.Lens' ListInferenceRecommendationsJobs (Prelude.Maybe RecommendationJobStatus)
listInferenceRecommendationsJobs_statusEquals = Lens.lens (\ListInferenceRecommendationsJobs' {statusEquals} -> statusEquals) (\s@ListInferenceRecommendationsJobs' {} a -> s {statusEquals = a} :: ListInferenceRecommendationsJobs)

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<*> ( x
                            Data..?> "InferenceRecommendationsJobs"
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
      _salt
        `Prelude.hashWithSalt` creationTimeAfter
        `Prelude.hashWithSalt` creationTimeBefore
        `Prelude.hashWithSalt` lastModifiedTimeAfter
        `Prelude.hashWithSalt` lastModifiedTimeBefore
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nameContains
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` sortBy
        `Prelude.hashWithSalt` sortOrder
        `Prelude.hashWithSalt` statusEquals

instance
  Prelude.NFData
    ListInferenceRecommendationsJobs
  where
  rnf ListInferenceRecommendationsJobs' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals

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
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
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
