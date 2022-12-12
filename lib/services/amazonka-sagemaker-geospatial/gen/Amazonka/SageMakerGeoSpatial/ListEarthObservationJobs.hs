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
-- Module      : Amazonka.SageMakerGeoSpatial.ListEarthObservationJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to get a list of the Earth Observation jobs
-- associated with the calling Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.SageMakerGeoSpatial.ListEarthObservationJobs
  ( -- * Creating a Request
    ListEarthObservationJobs (..),
    newListEarthObservationJobs,

    -- * Request Lenses
    listEarthObservationJobs_maxResults,
    listEarthObservationJobs_nextToken,
    listEarthObservationJobs_sortBy,
    listEarthObservationJobs_sortOrder,
    listEarthObservationJobs_statusEquals,

    -- * Destructuring the Response
    ListEarthObservationJobsResponse (..),
    newListEarthObservationJobsResponse,

    -- * Response Lenses
    listEarthObservationJobsResponse_nextToken,
    listEarthObservationJobsResponse_httpStatus,
    listEarthObservationJobsResponse_earthObservationJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newListEarthObservationJobs' smart constructor.
data ListEarthObservationJobs = ListEarthObservationJobs'
  { -- | The total number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The parameter by which to sort the results.
    sortBy :: Prelude.Maybe Prelude.Text,
    -- | An optional value that specifies whether you want the results sorted in
    -- @Ascending@ or @Descending@ order.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that retrieves only jobs with a specific status.
    statusEquals :: Prelude.Maybe EarthObservationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEarthObservationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEarthObservationJobs_maxResults' - The total number of items to return.
--
-- 'nextToken', 'listEarthObservationJobs_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'sortBy', 'listEarthObservationJobs_sortBy' - The parameter by which to sort the results.
--
-- 'sortOrder', 'listEarthObservationJobs_sortOrder' - An optional value that specifies whether you want the results sorted in
-- @Ascending@ or @Descending@ order.
--
-- 'statusEquals', 'listEarthObservationJobs_statusEquals' - A filter that retrieves only jobs with a specific status.
newListEarthObservationJobs ::
  ListEarthObservationJobs
newListEarthObservationJobs =
  ListEarthObservationJobs'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | The total number of items to return.
listEarthObservationJobs_maxResults :: Lens.Lens' ListEarthObservationJobs (Prelude.Maybe Prelude.Natural)
listEarthObservationJobs_maxResults = Lens.lens (\ListEarthObservationJobs' {maxResults} -> maxResults) (\s@ListEarthObservationJobs' {} a -> s {maxResults = a} :: ListEarthObservationJobs)

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listEarthObservationJobs_nextToken :: Lens.Lens' ListEarthObservationJobs (Prelude.Maybe Prelude.Text)
listEarthObservationJobs_nextToken = Lens.lens (\ListEarthObservationJobs' {nextToken} -> nextToken) (\s@ListEarthObservationJobs' {} a -> s {nextToken = a} :: ListEarthObservationJobs) Prelude.. Lens.mapping Data._Sensitive

-- | The parameter by which to sort the results.
listEarthObservationJobs_sortBy :: Lens.Lens' ListEarthObservationJobs (Prelude.Maybe Prelude.Text)
listEarthObservationJobs_sortBy = Lens.lens (\ListEarthObservationJobs' {sortBy} -> sortBy) (\s@ListEarthObservationJobs' {} a -> s {sortBy = a} :: ListEarthObservationJobs)

-- | An optional value that specifies whether you want the results sorted in
-- @Ascending@ or @Descending@ order.
listEarthObservationJobs_sortOrder :: Lens.Lens' ListEarthObservationJobs (Prelude.Maybe SortOrder)
listEarthObservationJobs_sortOrder = Lens.lens (\ListEarthObservationJobs' {sortOrder} -> sortOrder) (\s@ListEarthObservationJobs' {} a -> s {sortOrder = a} :: ListEarthObservationJobs)

-- | A filter that retrieves only jobs with a specific status.
listEarthObservationJobs_statusEquals :: Lens.Lens' ListEarthObservationJobs (Prelude.Maybe EarthObservationJobStatus)
listEarthObservationJobs_statusEquals = Lens.lens (\ListEarthObservationJobs' {statusEquals} -> statusEquals) (\s@ListEarthObservationJobs' {} a -> s {statusEquals = a} :: ListEarthObservationJobs)

instance Core.AWSPager ListEarthObservationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEarthObservationJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEarthObservationJobsResponse_earthObservationJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEarthObservationJobs_nextToken
          Lens..~ rs
          Lens.^? listEarthObservationJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEarthObservationJobs where
  type
    AWSResponse ListEarthObservationJobs =
      ListEarthObservationJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEarthObservationJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "EarthObservationJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEarthObservationJobs where
  hashWithSalt _salt ListEarthObservationJobs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListEarthObservationJobs where
  rnf ListEarthObservationJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals

instance Data.ToHeaders ListEarthObservationJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEarthObservationJobs where
  toJSON ListEarthObservationJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
          ]
      )

instance Data.ToPath ListEarthObservationJobs where
  toPath = Prelude.const "/list-earth-observation-jobs"

instance Data.ToQuery ListEarthObservationJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEarthObservationJobsResponse' smart constructor.
data ListEarthObservationJobsResponse = ListEarthObservationJobsResponse'
  { -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains summary information about the Earth Observation jobs.
    earthObservationJobSummaries :: [ListEarthObservationJobOutputConfig]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEarthObservationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEarthObservationJobsResponse_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'httpStatus', 'listEarthObservationJobsResponse_httpStatus' - The response's http status code.
--
-- 'earthObservationJobSummaries', 'listEarthObservationJobsResponse_earthObservationJobSummaries' - Contains summary information about the Earth Observation jobs.
newListEarthObservationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEarthObservationJobsResponse
newListEarthObservationJobsResponse pHttpStatus_ =
  ListEarthObservationJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      earthObservationJobSummaries =
        Prelude.mempty
    }

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listEarthObservationJobsResponse_nextToken :: Lens.Lens' ListEarthObservationJobsResponse (Prelude.Maybe Prelude.Text)
listEarthObservationJobsResponse_nextToken = Lens.lens (\ListEarthObservationJobsResponse' {nextToken} -> nextToken) (\s@ListEarthObservationJobsResponse' {} a -> s {nextToken = a} :: ListEarthObservationJobsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
listEarthObservationJobsResponse_httpStatus :: Lens.Lens' ListEarthObservationJobsResponse Prelude.Int
listEarthObservationJobsResponse_httpStatus = Lens.lens (\ListEarthObservationJobsResponse' {httpStatus} -> httpStatus) (\s@ListEarthObservationJobsResponse' {} a -> s {httpStatus = a} :: ListEarthObservationJobsResponse)

-- | Contains summary information about the Earth Observation jobs.
listEarthObservationJobsResponse_earthObservationJobSummaries :: Lens.Lens' ListEarthObservationJobsResponse [ListEarthObservationJobOutputConfig]
listEarthObservationJobsResponse_earthObservationJobSummaries = Lens.lens (\ListEarthObservationJobsResponse' {earthObservationJobSummaries} -> earthObservationJobSummaries) (\s@ListEarthObservationJobsResponse' {} a -> s {earthObservationJobSummaries = a} :: ListEarthObservationJobsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEarthObservationJobsResponse
  where
  rnf ListEarthObservationJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf earthObservationJobSummaries
