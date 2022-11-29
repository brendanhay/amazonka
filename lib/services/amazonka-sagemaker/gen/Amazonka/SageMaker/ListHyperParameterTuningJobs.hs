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
-- Module      : Amazonka.SageMaker.ListHyperParameterTuningJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of HyperParameterTuningJobSummary objects that describe the
-- hyperparameter tuning jobs launched in your account.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListHyperParameterTuningJobs
  ( -- * Creating a Request
    ListHyperParameterTuningJobs (..),
    newListHyperParameterTuningJobs,

    -- * Request Lenses
    listHyperParameterTuningJobs_sortOrder,
    listHyperParameterTuningJobs_nextToken,
    listHyperParameterTuningJobs_lastModifiedTimeAfter,
    listHyperParameterTuningJobs_nameContains,
    listHyperParameterTuningJobs_lastModifiedTimeBefore,
    listHyperParameterTuningJobs_creationTimeBefore,
    listHyperParameterTuningJobs_sortBy,
    listHyperParameterTuningJobs_maxResults,
    listHyperParameterTuningJobs_statusEquals,
    listHyperParameterTuningJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListHyperParameterTuningJobsResponse (..),
    newListHyperParameterTuningJobsResponse,

    -- * Response Lenses
    listHyperParameterTuningJobsResponse_nextToken,
    listHyperParameterTuningJobsResponse_httpStatus,
    listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListHyperParameterTuningJobs' smart constructor.
data ListHyperParameterTuningJobs = ListHyperParameterTuningJobs'
  { -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the result of the previous @ListHyperParameterTuningJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of tuning jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only tuning jobs that were modified after the
    -- specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A string in the tuning job name. This filter returns only tuning jobs
    -- whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only tuning jobs that were modified before the
    -- specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only tuning jobs that were created before the
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @Name@.
    sortBy :: Prelude.Maybe HyperParameterTuningJobSortByOptions,
    -- | The maximum number of tuning jobs to return. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only tuning jobs with the specified status.
    statusEquals :: Prelude.Maybe HyperParameterTuningJobStatus,
    -- | A filter that returns only tuning jobs that were created after the
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHyperParameterTuningJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listHyperParameterTuningJobs_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listHyperParameterTuningJobs_nextToken' - If the result of the previous @ListHyperParameterTuningJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of tuning jobs, use the token in the next request.
--
-- 'lastModifiedTimeAfter', 'listHyperParameterTuningJobs_lastModifiedTimeAfter' - A filter that returns only tuning jobs that were modified after the
-- specified time.
--
-- 'nameContains', 'listHyperParameterTuningJobs_nameContains' - A string in the tuning job name. This filter returns only tuning jobs
-- whose name contains the specified string.
--
-- 'lastModifiedTimeBefore', 'listHyperParameterTuningJobs_lastModifiedTimeBefore' - A filter that returns only tuning jobs that were modified before the
-- specified time.
--
-- 'creationTimeBefore', 'listHyperParameterTuningJobs_creationTimeBefore' - A filter that returns only tuning jobs that were created before the
-- specified time.
--
-- 'sortBy', 'listHyperParameterTuningJobs_sortBy' - The field to sort results by. The default is @Name@.
--
-- 'maxResults', 'listHyperParameterTuningJobs_maxResults' - The maximum number of tuning jobs to return. The default value is 10.
--
-- 'statusEquals', 'listHyperParameterTuningJobs_statusEquals' - A filter that returns only tuning jobs with the specified status.
--
-- 'creationTimeAfter', 'listHyperParameterTuningJobs_creationTimeAfter' - A filter that returns only tuning jobs that were created after the
-- specified time.
newListHyperParameterTuningJobs ::
  ListHyperParameterTuningJobs
newListHyperParameterTuningJobs =
  ListHyperParameterTuningJobs'
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

-- | The sort order for results. The default is @Ascending@.
listHyperParameterTuningJobs_sortOrder :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe SortOrder)
listHyperParameterTuningJobs_sortOrder = Lens.lens (\ListHyperParameterTuningJobs' {sortOrder} -> sortOrder) (\s@ListHyperParameterTuningJobs' {} a -> s {sortOrder = a} :: ListHyperParameterTuningJobs)

-- | If the result of the previous @ListHyperParameterTuningJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of tuning jobs, use the token in the next request.
listHyperParameterTuningJobs_nextToken :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe Prelude.Text)
listHyperParameterTuningJobs_nextToken = Lens.lens (\ListHyperParameterTuningJobs' {nextToken} -> nextToken) (\s@ListHyperParameterTuningJobs' {} a -> s {nextToken = a} :: ListHyperParameterTuningJobs)

-- | A filter that returns only tuning jobs that were modified after the
-- specified time.
listHyperParameterTuningJobs_lastModifiedTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe Prelude.UTCTime)
listHyperParameterTuningJobs_lastModifiedTimeAfter = Lens.lens (\ListHyperParameterTuningJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListHyperParameterTuningJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListHyperParameterTuningJobs) Prelude.. Lens.mapping Core._Time

-- | A string in the tuning job name. This filter returns only tuning jobs
-- whose name contains the specified string.
listHyperParameterTuningJobs_nameContains :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe Prelude.Text)
listHyperParameterTuningJobs_nameContains = Lens.lens (\ListHyperParameterTuningJobs' {nameContains} -> nameContains) (\s@ListHyperParameterTuningJobs' {} a -> s {nameContains = a} :: ListHyperParameterTuningJobs)

-- | A filter that returns only tuning jobs that were modified before the
-- specified time.
listHyperParameterTuningJobs_lastModifiedTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe Prelude.UTCTime)
listHyperParameterTuningJobs_lastModifiedTimeBefore = Lens.lens (\ListHyperParameterTuningJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListHyperParameterTuningJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListHyperParameterTuningJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only tuning jobs that were created before the
-- specified time.
listHyperParameterTuningJobs_creationTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe Prelude.UTCTime)
listHyperParameterTuningJobs_creationTimeBefore = Lens.lens (\ListHyperParameterTuningJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListHyperParameterTuningJobs' {} a -> s {creationTimeBefore = a} :: ListHyperParameterTuningJobs) Prelude.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @Name@.
listHyperParameterTuningJobs_sortBy :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe HyperParameterTuningJobSortByOptions)
listHyperParameterTuningJobs_sortBy = Lens.lens (\ListHyperParameterTuningJobs' {sortBy} -> sortBy) (\s@ListHyperParameterTuningJobs' {} a -> s {sortBy = a} :: ListHyperParameterTuningJobs)

-- | The maximum number of tuning jobs to return. The default value is 10.
listHyperParameterTuningJobs_maxResults :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe Prelude.Natural)
listHyperParameterTuningJobs_maxResults = Lens.lens (\ListHyperParameterTuningJobs' {maxResults} -> maxResults) (\s@ListHyperParameterTuningJobs' {} a -> s {maxResults = a} :: ListHyperParameterTuningJobs)

-- | A filter that returns only tuning jobs with the specified status.
listHyperParameterTuningJobs_statusEquals :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe HyperParameterTuningJobStatus)
listHyperParameterTuningJobs_statusEquals = Lens.lens (\ListHyperParameterTuningJobs' {statusEquals} -> statusEquals) (\s@ListHyperParameterTuningJobs' {} a -> s {statusEquals = a} :: ListHyperParameterTuningJobs)

-- | A filter that returns only tuning jobs that were created after the
-- specified time.
listHyperParameterTuningJobs_creationTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Prelude.Maybe Prelude.UTCTime)
listHyperParameterTuningJobs_creationTimeAfter = Lens.lens (\ListHyperParameterTuningJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListHyperParameterTuningJobs' {} a -> s {creationTimeAfter = a} :: ListHyperParameterTuningJobs) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListHyperParameterTuningJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHyperParameterTuningJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listHyperParameterTuningJobs_nextToken
          Lens..~ rs
          Lens.^? listHyperParameterTuningJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListHyperParameterTuningJobs where
  type
    AWSResponse ListHyperParameterTuningJobs =
      ListHyperParameterTuningJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHyperParameterTuningJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "HyperParameterTuningJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListHyperParameterTuningJobs
  where
  hashWithSalt _salt ListHyperParameterTuningJobs' {..} =
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

instance Prelude.NFData ListHyperParameterTuningJobs where
  rnf ListHyperParameterTuningJobs' {..} =
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

instance Core.ToHeaders ListHyperParameterTuningJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListHyperParameterTuningJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListHyperParameterTuningJobs where
  toJSON ListHyperParameterTuningJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListHyperParameterTuningJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListHyperParameterTuningJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHyperParameterTuningJobsResponse' smart constructor.
data ListHyperParameterTuningJobsResponse = ListHyperParameterTuningJobsResponse'
  { -- | If the result of this @ListHyperParameterTuningJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of tuning jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of HyperParameterTuningJobSummary objects that describe the
    -- tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
    hyperParameterTuningJobSummaries :: [HyperParameterTuningJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHyperParameterTuningJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHyperParameterTuningJobsResponse_nextToken' - If the result of this @ListHyperParameterTuningJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of tuning jobs, use the token in the next request.
--
-- 'httpStatus', 'listHyperParameterTuningJobsResponse_httpStatus' - The response's http status code.
--
-- 'hyperParameterTuningJobSummaries', 'listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries' - A list of HyperParameterTuningJobSummary objects that describe the
-- tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
newListHyperParameterTuningJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHyperParameterTuningJobsResponse
newListHyperParameterTuningJobsResponse pHttpStatus_ =
  ListHyperParameterTuningJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      hyperParameterTuningJobSummaries =
        Prelude.mempty
    }

-- | If the result of this @ListHyperParameterTuningJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of tuning jobs, use the token in the next request.
listHyperParameterTuningJobsResponse_nextToken :: Lens.Lens' ListHyperParameterTuningJobsResponse (Prelude.Maybe Prelude.Text)
listHyperParameterTuningJobsResponse_nextToken = Lens.lens (\ListHyperParameterTuningJobsResponse' {nextToken} -> nextToken) (\s@ListHyperParameterTuningJobsResponse' {} a -> s {nextToken = a} :: ListHyperParameterTuningJobsResponse)

-- | The response's http status code.
listHyperParameterTuningJobsResponse_httpStatus :: Lens.Lens' ListHyperParameterTuningJobsResponse Prelude.Int
listHyperParameterTuningJobsResponse_httpStatus = Lens.lens (\ListHyperParameterTuningJobsResponse' {httpStatus} -> httpStatus) (\s@ListHyperParameterTuningJobsResponse' {} a -> s {httpStatus = a} :: ListHyperParameterTuningJobsResponse)

-- | A list of HyperParameterTuningJobSummary objects that describe the
-- tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries :: Lens.Lens' ListHyperParameterTuningJobsResponse [HyperParameterTuningJobSummary]
listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries = Lens.lens (\ListHyperParameterTuningJobsResponse' {hyperParameterTuningJobSummaries} -> hyperParameterTuningJobSummaries) (\s@ListHyperParameterTuningJobsResponse' {} a -> s {hyperParameterTuningJobSummaries = a} :: ListHyperParameterTuningJobsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListHyperParameterTuningJobsResponse
  where
  rnf ListHyperParameterTuningJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobSummaries
