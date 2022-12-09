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
-- Module      : Amazonka.SageMakerGeoSpatial.ListVectorEnrichmentJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of vector enrichment jobs.
--
-- This operation returns paginated results.
module Amazonka.SageMakerGeoSpatial.ListVectorEnrichmentJobs
  ( -- * Creating a Request
    ListVectorEnrichmentJobs (..),
    newListVectorEnrichmentJobs,

    -- * Request Lenses
    listVectorEnrichmentJobs_maxResults,
    listVectorEnrichmentJobs_nextToken,
    listVectorEnrichmentJobs_sortBy,
    listVectorEnrichmentJobs_sortOrder,
    listVectorEnrichmentJobs_statusEquals,

    -- * Destructuring the Response
    ListVectorEnrichmentJobsResponse (..),
    newListVectorEnrichmentJobsResponse,

    -- * Response Lenses
    listVectorEnrichmentJobsResponse_nextToken,
    listVectorEnrichmentJobsResponse_httpStatus,
    listVectorEnrichmentJobsResponse_vectorEnrichmentJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newListVectorEnrichmentJobs' smart constructor.
data ListVectorEnrichmentJobs = ListVectorEnrichmentJobs'
  { -- | The maximum number of items to return.
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
    statusEquals :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVectorEnrichmentJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVectorEnrichmentJobs_maxResults' - The maximum number of items to return.
--
-- 'nextToken', 'listVectorEnrichmentJobs_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'sortBy', 'listVectorEnrichmentJobs_sortBy' - The parameter by which to sort the results.
--
-- 'sortOrder', 'listVectorEnrichmentJobs_sortOrder' - An optional value that specifies whether you want the results sorted in
-- @Ascending@ or @Descending@ order.
--
-- 'statusEquals', 'listVectorEnrichmentJobs_statusEquals' - A filter that retrieves only jobs with a specific status.
newListVectorEnrichmentJobs ::
  ListVectorEnrichmentJobs
newListVectorEnrichmentJobs =
  ListVectorEnrichmentJobs'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | The maximum number of items to return.
listVectorEnrichmentJobs_maxResults :: Lens.Lens' ListVectorEnrichmentJobs (Prelude.Maybe Prelude.Natural)
listVectorEnrichmentJobs_maxResults = Lens.lens (\ListVectorEnrichmentJobs' {maxResults} -> maxResults) (\s@ListVectorEnrichmentJobs' {} a -> s {maxResults = a} :: ListVectorEnrichmentJobs)

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listVectorEnrichmentJobs_nextToken :: Lens.Lens' ListVectorEnrichmentJobs (Prelude.Maybe Prelude.Text)
listVectorEnrichmentJobs_nextToken = Lens.lens (\ListVectorEnrichmentJobs' {nextToken} -> nextToken) (\s@ListVectorEnrichmentJobs' {} a -> s {nextToken = a} :: ListVectorEnrichmentJobs) Prelude.. Lens.mapping Data._Sensitive

-- | The parameter by which to sort the results.
listVectorEnrichmentJobs_sortBy :: Lens.Lens' ListVectorEnrichmentJobs (Prelude.Maybe Prelude.Text)
listVectorEnrichmentJobs_sortBy = Lens.lens (\ListVectorEnrichmentJobs' {sortBy} -> sortBy) (\s@ListVectorEnrichmentJobs' {} a -> s {sortBy = a} :: ListVectorEnrichmentJobs)

-- | An optional value that specifies whether you want the results sorted in
-- @Ascending@ or @Descending@ order.
listVectorEnrichmentJobs_sortOrder :: Lens.Lens' ListVectorEnrichmentJobs (Prelude.Maybe SortOrder)
listVectorEnrichmentJobs_sortOrder = Lens.lens (\ListVectorEnrichmentJobs' {sortOrder} -> sortOrder) (\s@ListVectorEnrichmentJobs' {} a -> s {sortOrder = a} :: ListVectorEnrichmentJobs)

-- | A filter that retrieves only jobs with a specific status.
listVectorEnrichmentJobs_statusEquals :: Lens.Lens' ListVectorEnrichmentJobs (Prelude.Maybe Prelude.Text)
listVectorEnrichmentJobs_statusEquals = Lens.lens (\ListVectorEnrichmentJobs' {statusEquals} -> statusEquals) (\s@ListVectorEnrichmentJobs' {} a -> s {statusEquals = a} :: ListVectorEnrichmentJobs)

instance Core.AWSPager ListVectorEnrichmentJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVectorEnrichmentJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listVectorEnrichmentJobsResponse_vectorEnrichmentJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVectorEnrichmentJobs_nextToken
          Lens..~ rs
          Lens.^? listVectorEnrichmentJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListVectorEnrichmentJobs where
  type
    AWSResponse ListVectorEnrichmentJobs =
      ListVectorEnrichmentJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVectorEnrichmentJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "VectorEnrichmentJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListVectorEnrichmentJobs where
  hashWithSalt _salt ListVectorEnrichmentJobs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListVectorEnrichmentJobs where
  rnf ListVectorEnrichmentJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals

instance Data.ToHeaders ListVectorEnrichmentJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVectorEnrichmentJobs where
  toJSON ListVectorEnrichmentJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
          ]
      )

instance Data.ToPath ListVectorEnrichmentJobs where
  toPath = Prelude.const "/list-vector-enrichment-jobs"

instance Data.ToQuery ListVectorEnrichmentJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVectorEnrichmentJobsResponse' smart constructor.
data ListVectorEnrichmentJobsResponse = ListVectorEnrichmentJobsResponse'
  { -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains summary information about the Vector Enrichment jobs.
    vectorEnrichmentJobSummaries :: [ListVectorEnrichmentJobOutputConfig]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVectorEnrichmentJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVectorEnrichmentJobsResponse_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'httpStatus', 'listVectorEnrichmentJobsResponse_httpStatus' - The response's http status code.
--
-- 'vectorEnrichmentJobSummaries', 'listVectorEnrichmentJobsResponse_vectorEnrichmentJobSummaries' - Contains summary information about the Vector Enrichment jobs.
newListVectorEnrichmentJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVectorEnrichmentJobsResponse
newListVectorEnrichmentJobsResponse pHttpStatus_ =
  ListVectorEnrichmentJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      vectorEnrichmentJobSummaries =
        Prelude.mempty
    }

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listVectorEnrichmentJobsResponse_nextToken :: Lens.Lens' ListVectorEnrichmentJobsResponse (Prelude.Maybe Prelude.Text)
listVectorEnrichmentJobsResponse_nextToken = Lens.lens (\ListVectorEnrichmentJobsResponse' {nextToken} -> nextToken) (\s@ListVectorEnrichmentJobsResponse' {} a -> s {nextToken = a} :: ListVectorEnrichmentJobsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
listVectorEnrichmentJobsResponse_httpStatus :: Lens.Lens' ListVectorEnrichmentJobsResponse Prelude.Int
listVectorEnrichmentJobsResponse_httpStatus = Lens.lens (\ListVectorEnrichmentJobsResponse' {httpStatus} -> httpStatus) (\s@ListVectorEnrichmentJobsResponse' {} a -> s {httpStatus = a} :: ListVectorEnrichmentJobsResponse)

-- | Contains summary information about the Vector Enrichment jobs.
listVectorEnrichmentJobsResponse_vectorEnrichmentJobSummaries :: Lens.Lens' ListVectorEnrichmentJobsResponse [ListVectorEnrichmentJobOutputConfig]
listVectorEnrichmentJobsResponse_vectorEnrichmentJobSummaries = Lens.lens (\ListVectorEnrichmentJobsResponse' {vectorEnrichmentJobSummaries} -> vectorEnrichmentJobSummaries) (\s@ListVectorEnrichmentJobsResponse' {} a -> s {vectorEnrichmentJobSummaries = a} :: ListVectorEnrichmentJobsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListVectorEnrichmentJobsResponse
  where
  rnf ListVectorEnrichmentJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vectorEnrichmentJobSummaries
