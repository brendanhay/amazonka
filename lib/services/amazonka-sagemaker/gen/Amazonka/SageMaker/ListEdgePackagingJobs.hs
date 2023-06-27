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
-- Module      : Amazonka.SageMaker.ListEdgePackagingJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of edge packaging jobs.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListEdgePackagingJobs
  ( -- * Creating a Request
    ListEdgePackagingJobs (..),
    newListEdgePackagingJobs,

    -- * Request Lenses
    listEdgePackagingJobs_creationTimeAfter,
    listEdgePackagingJobs_creationTimeBefore,
    listEdgePackagingJobs_lastModifiedTimeAfter,
    listEdgePackagingJobs_lastModifiedTimeBefore,
    listEdgePackagingJobs_maxResults,
    listEdgePackagingJobs_modelNameContains,
    listEdgePackagingJobs_nameContains,
    listEdgePackagingJobs_nextToken,
    listEdgePackagingJobs_sortBy,
    listEdgePackagingJobs_sortOrder,
    listEdgePackagingJobs_statusEquals,

    -- * Destructuring the Response
    ListEdgePackagingJobsResponse (..),
    newListEdgePackagingJobsResponse,

    -- * Response Lenses
    listEdgePackagingJobsResponse_nextToken,
    listEdgePackagingJobsResponse_httpStatus,
    listEdgePackagingJobsResponse_edgePackagingJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListEdgePackagingJobs' smart constructor.
data ListEdgePackagingJobs = ListEdgePackagingJobs'
  { -- | Select jobs where the job was created after specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Select jobs where the job was created before specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Select jobs where the job was updated after specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Select jobs where the job was updated before specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Maximum number of results to select.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Filter for jobs where the model name contains this string.
    modelNameContains :: Prelude.Maybe Prelude.Text,
    -- | Filter for jobs containing this name in their packaging job name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use to specify what column to sort by.
    sortBy :: Prelude.Maybe ListEdgePackagingJobsSortBy,
    -- | What direction to sort by.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The job status to filter for.
    statusEquals :: Prelude.Maybe EdgePackagingJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEdgePackagingJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listEdgePackagingJobs_creationTimeAfter' - Select jobs where the job was created after specified time.
--
-- 'creationTimeBefore', 'listEdgePackagingJobs_creationTimeBefore' - Select jobs where the job was created before specified time.
--
-- 'lastModifiedTimeAfter', 'listEdgePackagingJobs_lastModifiedTimeAfter' - Select jobs where the job was updated after specified time.
--
-- 'lastModifiedTimeBefore', 'listEdgePackagingJobs_lastModifiedTimeBefore' - Select jobs where the job was updated before specified time.
--
-- 'maxResults', 'listEdgePackagingJobs_maxResults' - Maximum number of results to select.
--
-- 'modelNameContains', 'listEdgePackagingJobs_modelNameContains' - Filter for jobs where the model name contains this string.
--
-- 'nameContains', 'listEdgePackagingJobs_nameContains' - Filter for jobs containing this name in their packaging job name.
--
-- 'nextToken', 'listEdgePackagingJobs_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'sortBy', 'listEdgePackagingJobs_sortBy' - Use to specify what column to sort by.
--
-- 'sortOrder', 'listEdgePackagingJobs_sortOrder' - What direction to sort by.
--
-- 'statusEquals', 'listEdgePackagingJobs_statusEquals' - The job status to filter for.
newListEdgePackagingJobs ::
  ListEdgePackagingJobs
newListEdgePackagingJobs =
  ListEdgePackagingJobs'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelNameContains = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | Select jobs where the job was created after specified time.
listEdgePackagingJobs_creationTimeAfter :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe Prelude.UTCTime)
listEdgePackagingJobs_creationTimeAfter = Lens.lens (\ListEdgePackagingJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListEdgePackagingJobs' {} a -> s {creationTimeAfter = a} :: ListEdgePackagingJobs) Prelude.. Lens.mapping Data._Time

-- | Select jobs where the job was created before specified time.
listEdgePackagingJobs_creationTimeBefore :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe Prelude.UTCTime)
listEdgePackagingJobs_creationTimeBefore = Lens.lens (\ListEdgePackagingJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListEdgePackagingJobs' {} a -> s {creationTimeBefore = a} :: ListEdgePackagingJobs) Prelude.. Lens.mapping Data._Time

-- | Select jobs where the job was updated after specified time.
listEdgePackagingJobs_lastModifiedTimeAfter :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe Prelude.UTCTime)
listEdgePackagingJobs_lastModifiedTimeAfter = Lens.lens (\ListEdgePackagingJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListEdgePackagingJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListEdgePackagingJobs) Prelude.. Lens.mapping Data._Time

-- | Select jobs where the job was updated before specified time.
listEdgePackagingJobs_lastModifiedTimeBefore :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe Prelude.UTCTime)
listEdgePackagingJobs_lastModifiedTimeBefore = Lens.lens (\ListEdgePackagingJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListEdgePackagingJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListEdgePackagingJobs) Prelude.. Lens.mapping Data._Time

-- | Maximum number of results to select.
listEdgePackagingJobs_maxResults :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe Prelude.Int)
listEdgePackagingJobs_maxResults = Lens.lens (\ListEdgePackagingJobs' {maxResults} -> maxResults) (\s@ListEdgePackagingJobs' {} a -> s {maxResults = a} :: ListEdgePackagingJobs)

-- | Filter for jobs where the model name contains this string.
listEdgePackagingJobs_modelNameContains :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe Prelude.Text)
listEdgePackagingJobs_modelNameContains = Lens.lens (\ListEdgePackagingJobs' {modelNameContains} -> modelNameContains) (\s@ListEdgePackagingJobs' {} a -> s {modelNameContains = a} :: ListEdgePackagingJobs)

-- | Filter for jobs containing this name in their packaging job name.
listEdgePackagingJobs_nameContains :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe Prelude.Text)
listEdgePackagingJobs_nameContains = Lens.lens (\ListEdgePackagingJobs' {nameContains} -> nameContains) (\s@ListEdgePackagingJobs' {} a -> s {nameContains = a} :: ListEdgePackagingJobs)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listEdgePackagingJobs_nextToken :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe Prelude.Text)
listEdgePackagingJobs_nextToken = Lens.lens (\ListEdgePackagingJobs' {nextToken} -> nextToken) (\s@ListEdgePackagingJobs' {} a -> s {nextToken = a} :: ListEdgePackagingJobs)

-- | Use to specify what column to sort by.
listEdgePackagingJobs_sortBy :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe ListEdgePackagingJobsSortBy)
listEdgePackagingJobs_sortBy = Lens.lens (\ListEdgePackagingJobs' {sortBy} -> sortBy) (\s@ListEdgePackagingJobs' {} a -> s {sortBy = a} :: ListEdgePackagingJobs)

-- | What direction to sort by.
listEdgePackagingJobs_sortOrder :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe SortOrder)
listEdgePackagingJobs_sortOrder = Lens.lens (\ListEdgePackagingJobs' {sortOrder} -> sortOrder) (\s@ListEdgePackagingJobs' {} a -> s {sortOrder = a} :: ListEdgePackagingJobs)

-- | The job status to filter for.
listEdgePackagingJobs_statusEquals :: Lens.Lens' ListEdgePackagingJobs (Prelude.Maybe EdgePackagingJobStatus)
listEdgePackagingJobs_statusEquals = Lens.lens (\ListEdgePackagingJobs' {statusEquals} -> statusEquals) (\s@ListEdgePackagingJobs' {} a -> s {statusEquals = a} :: ListEdgePackagingJobs)

instance Core.AWSPager ListEdgePackagingJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEdgePackagingJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEdgePackagingJobsResponse_edgePackagingJobSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEdgePackagingJobs_nextToken
          Lens..~ rs
          Lens.^? listEdgePackagingJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEdgePackagingJobs where
  type
    AWSResponse ListEdgePackagingJobs =
      ListEdgePackagingJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEdgePackagingJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "EdgePackagingJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEdgePackagingJobs where
  hashWithSalt _salt ListEdgePackagingJobs' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelNameContains
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListEdgePackagingJobs where
  rnf ListEdgePackagingJobs' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelNameContains
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals

instance Data.ToHeaders ListEdgePackagingJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListEdgePackagingJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEdgePackagingJobs where
  toJSON ListEdgePackagingJobs' {..} =
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
            ("ModelNameContains" Data..=)
              Prelude.<$> modelNameContains,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
          ]
      )

instance Data.ToPath ListEdgePackagingJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEdgePackagingJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEdgePackagingJobsResponse' smart constructor.
data ListEdgePackagingJobsResponse = ListEdgePackagingJobsResponse'
  { -- | Token to use when calling the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summaries of edge packaging jobs.
    edgePackagingJobSummaries :: [EdgePackagingJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEdgePackagingJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEdgePackagingJobsResponse_nextToken' - Token to use when calling the next page of results.
--
-- 'httpStatus', 'listEdgePackagingJobsResponse_httpStatus' - The response's http status code.
--
-- 'edgePackagingJobSummaries', 'listEdgePackagingJobsResponse_edgePackagingJobSummaries' - Summaries of edge packaging jobs.
newListEdgePackagingJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEdgePackagingJobsResponse
newListEdgePackagingJobsResponse pHttpStatus_ =
  ListEdgePackagingJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      edgePackagingJobSummaries = Prelude.mempty
    }

-- | Token to use when calling the next page of results.
listEdgePackagingJobsResponse_nextToken :: Lens.Lens' ListEdgePackagingJobsResponse (Prelude.Maybe Prelude.Text)
listEdgePackagingJobsResponse_nextToken = Lens.lens (\ListEdgePackagingJobsResponse' {nextToken} -> nextToken) (\s@ListEdgePackagingJobsResponse' {} a -> s {nextToken = a} :: ListEdgePackagingJobsResponse)

-- | The response's http status code.
listEdgePackagingJobsResponse_httpStatus :: Lens.Lens' ListEdgePackagingJobsResponse Prelude.Int
listEdgePackagingJobsResponse_httpStatus = Lens.lens (\ListEdgePackagingJobsResponse' {httpStatus} -> httpStatus) (\s@ListEdgePackagingJobsResponse' {} a -> s {httpStatus = a} :: ListEdgePackagingJobsResponse)

-- | Summaries of edge packaging jobs.
listEdgePackagingJobsResponse_edgePackagingJobSummaries :: Lens.Lens' ListEdgePackagingJobsResponse [EdgePackagingJobSummary]
listEdgePackagingJobsResponse_edgePackagingJobSummaries = Lens.lens (\ListEdgePackagingJobsResponse' {edgePackagingJobSummaries} -> edgePackagingJobSummaries) (\s@ListEdgePackagingJobsResponse' {} a -> s {edgePackagingJobSummaries = a} :: ListEdgePackagingJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEdgePackagingJobsResponse where
  rnf ListEdgePackagingJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf edgePackagingJobSummaries
