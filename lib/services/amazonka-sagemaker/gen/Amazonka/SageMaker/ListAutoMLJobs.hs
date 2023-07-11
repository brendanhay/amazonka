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
-- Module      : Amazonka.SageMaker.ListAutoMLJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of jobs.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListAutoMLJobs
  ( -- * Creating a Request
    ListAutoMLJobs (..),
    newListAutoMLJobs,

    -- * Request Lenses
    listAutoMLJobs_creationTimeAfter,
    listAutoMLJobs_creationTimeBefore,
    listAutoMLJobs_lastModifiedTimeAfter,
    listAutoMLJobs_lastModifiedTimeBefore,
    listAutoMLJobs_maxResults,
    listAutoMLJobs_nameContains,
    listAutoMLJobs_nextToken,
    listAutoMLJobs_sortBy,
    listAutoMLJobs_sortOrder,
    listAutoMLJobs_statusEquals,

    -- * Destructuring the Response
    ListAutoMLJobsResponse (..),
    newListAutoMLJobsResponse,

    -- * Response Lenses
    listAutoMLJobsResponse_nextToken,
    listAutoMLJobsResponse_httpStatus,
    listAutoMLJobsResponse_autoMLJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListAutoMLJobs' smart constructor.
data ListAutoMLJobs = ListAutoMLJobs'
  { -- | Request a list of jobs, using a filter for time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Request a list of jobs, using a filter for time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Request a list of jobs, using a filter for time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Request a list of jobs, using a filter for time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Request a list of jobs up to a specified limit.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Request a list of jobs, using a search filter for name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parameter by which to sort the results. The default is @Name@.
    sortBy :: Prelude.Maybe AutoMLSortBy,
    -- | The sort order for the results. The default is @Descending@.
    sortOrder :: Prelude.Maybe AutoMLSortOrder,
    -- | Request a list of jobs, using a filter for status.
    statusEquals :: Prelude.Maybe AutoMLJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAutoMLJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listAutoMLJobs_creationTimeAfter' - Request a list of jobs, using a filter for time.
--
-- 'creationTimeBefore', 'listAutoMLJobs_creationTimeBefore' - Request a list of jobs, using a filter for time.
--
-- 'lastModifiedTimeAfter', 'listAutoMLJobs_lastModifiedTimeAfter' - Request a list of jobs, using a filter for time.
--
-- 'lastModifiedTimeBefore', 'listAutoMLJobs_lastModifiedTimeBefore' - Request a list of jobs, using a filter for time.
--
-- 'maxResults', 'listAutoMLJobs_maxResults' - Request a list of jobs up to a specified limit.
--
-- 'nameContains', 'listAutoMLJobs_nameContains' - Request a list of jobs, using a search filter for name.
--
-- 'nextToken', 'listAutoMLJobs_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'sortBy', 'listAutoMLJobs_sortBy' - The parameter by which to sort the results. The default is @Name@.
--
-- 'sortOrder', 'listAutoMLJobs_sortOrder' - The sort order for the results. The default is @Descending@.
--
-- 'statusEquals', 'listAutoMLJobs_statusEquals' - Request a list of jobs, using a filter for status.
newListAutoMLJobs ::
  ListAutoMLJobs
newListAutoMLJobs =
  ListAutoMLJobs'
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

-- | Request a list of jobs, using a filter for time.
listAutoMLJobs_creationTimeAfter :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe Prelude.UTCTime)
listAutoMLJobs_creationTimeAfter = Lens.lens (\ListAutoMLJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListAutoMLJobs' {} a -> s {creationTimeAfter = a} :: ListAutoMLJobs) Prelude.. Lens.mapping Data._Time

-- | Request a list of jobs, using a filter for time.
listAutoMLJobs_creationTimeBefore :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe Prelude.UTCTime)
listAutoMLJobs_creationTimeBefore = Lens.lens (\ListAutoMLJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListAutoMLJobs' {} a -> s {creationTimeBefore = a} :: ListAutoMLJobs) Prelude.. Lens.mapping Data._Time

-- | Request a list of jobs, using a filter for time.
listAutoMLJobs_lastModifiedTimeAfter :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe Prelude.UTCTime)
listAutoMLJobs_lastModifiedTimeAfter = Lens.lens (\ListAutoMLJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListAutoMLJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListAutoMLJobs) Prelude.. Lens.mapping Data._Time

-- | Request a list of jobs, using a filter for time.
listAutoMLJobs_lastModifiedTimeBefore :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe Prelude.UTCTime)
listAutoMLJobs_lastModifiedTimeBefore = Lens.lens (\ListAutoMLJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListAutoMLJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListAutoMLJobs) Prelude.. Lens.mapping Data._Time

-- | Request a list of jobs up to a specified limit.
listAutoMLJobs_maxResults :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe Prelude.Natural)
listAutoMLJobs_maxResults = Lens.lens (\ListAutoMLJobs' {maxResults} -> maxResults) (\s@ListAutoMLJobs' {} a -> s {maxResults = a} :: ListAutoMLJobs)

-- | Request a list of jobs, using a search filter for name.
listAutoMLJobs_nameContains :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe Prelude.Text)
listAutoMLJobs_nameContains = Lens.lens (\ListAutoMLJobs' {nameContains} -> nameContains) (\s@ListAutoMLJobs' {} a -> s {nameContains = a} :: ListAutoMLJobs)

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listAutoMLJobs_nextToken :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe Prelude.Text)
listAutoMLJobs_nextToken = Lens.lens (\ListAutoMLJobs' {nextToken} -> nextToken) (\s@ListAutoMLJobs' {} a -> s {nextToken = a} :: ListAutoMLJobs)

-- | The parameter by which to sort the results. The default is @Name@.
listAutoMLJobs_sortBy :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe AutoMLSortBy)
listAutoMLJobs_sortBy = Lens.lens (\ListAutoMLJobs' {sortBy} -> sortBy) (\s@ListAutoMLJobs' {} a -> s {sortBy = a} :: ListAutoMLJobs)

-- | The sort order for the results. The default is @Descending@.
listAutoMLJobs_sortOrder :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe AutoMLSortOrder)
listAutoMLJobs_sortOrder = Lens.lens (\ListAutoMLJobs' {sortOrder} -> sortOrder) (\s@ListAutoMLJobs' {} a -> s {sortOrder = a} :: ListAutoMLJobs)

-- | Request a list of jobs, using a filter for status.
listAutoMLJobs_statusEquals :: Lens.Lens' ListAutoMLJobs (Prelude.Maybe AutoMLJobStatus)
listAutoMLJobs_statusEquals = Lens.lens (\ListAutoMLJobs' {statusEquals} -> statusEquals) (\s@ListAutoMLJobs' {} a -> s {statusEquals = a} :: ListAutoMLJobs)

instance Core.AWSPager ListAutoMLJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAutoMLJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAutoMLJobsResponse_autoMLJobSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAutoMLJobs_nextToken
          Lens..~ rs
          Lens.^? listAutoMLJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAutoMLJobs where
  type
    AWSResponse ListAutoMLJobs =
      ListAutoMLJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAutoMLJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "AutoMLJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAutoMLJobs where
  hashWithSalt _salt ListAutoMLJobs' {..} =
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

instance Prelude.NFData ListAutoMLJobs where
  rnf ListAutoMLJobs' {..} =
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

instance Data.ToHeaders ListAutoMLJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListAutoMLJobs" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAutoMLJobs where
  toJSON ListAutoMLJobs' {..} =
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

instance Data.ToPath ListAutoMLJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAutoMLJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAutoMLJobsResponse' smart constructor.
data ListAutoMLJobsResponse = ListAutoMLJobsResponse'
  { -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns a summary list of jobs.
    autoMLJobSummaries :: [AutoMLJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAutoMLJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAutoMLJobsResponse_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'httpStatus', 'listAutoMLJobsResponse_httpStatus' - The response's http status code.
--
-- 'autoMLJobSummaries', 'listAutoMLJobsResponse_autoMLJobSummaries' - Returns a summary list of jobs.
newListAutoMLJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAutoMLJobsResponse
newListAutoMLJobsResponse pHttpStatus_ =
  ListAutoMLJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      autoMLJobSummaries = Prelude.mempty
    }

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listAutoMLJobsResponse_nextToken :: Lens.Lens' ListAutoMLJobsResponse (Prelude.Maybe Prelude.Text)
listAutoMLJobsResponse_nextToken = Lens.lens (\ListAutoMLJobsResponse' {nextToken} -> nextToken) (\s@ListAutoMLJobsResponse' {} a -> s {nextToken = a} :: ListAutoMLJobsResponse)

-- | The response's http status code.
listAutoMLJobsResponse_httpStatus :: Lens.Lens' ListAutoMLJobsResponse Prelude.Int
listAutoMLJobsResponse_httpStatus = Lens.lens (\ListAutoMLJobsResponse' {httpStatus} -> httpStatus) (\s@ListAutoMLJobsResponse' {} a -> s {httpStatus = a} :: ListAutoMLJobsResponse)

-- | Returns a summary list of jobs.
listAutoMLJobsResponse_autoMLJobSummaries :: Lens.Lens' ListAutoMLJobsResponse [AutoMLJobSummary]
listAutoMLJobsResponse_autoMLJobSummaries = Lens.lens (\ListAutoMLJobsResponse' {autoMLJobSummaries} -> autoMLJobSummaries) (\s@ListAutoMLJobsResponse' {} a -> s {autoMLJobSummaries = a} :: ListAutoMLJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAutoMLJobsResponse where
  rnf ListAutoMLJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf autoMLJobSummaries
