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
-- Module      : Amazonka.MacieV2.ListClassificationJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a subset of information about one or more classification jobs.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListClassificationJobs
  ( -- * Creating a Request
    ListClassificationJobs (..),
    newListClassificationJobs,

    -- * Request Lenses
    listClassificationJobs_sortCriteria,
    listClassificationJobs_nextToken,
    listClassificationJobs_filterCriteria,
    listClassificationJobs_maxResults,

    -- * Destructuring the Response
    ListClassificationJobsResponse (..),
    newListClassificationJobsResponse,

    -- * Response Lenses
    listClassificationJobsResponse_items,
    listClassificationJobsResponse_nextToken,
    listClassificationJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListClassificationJobs' smart constructor.
data ListClassificationJobs = ListClassificationJobs'
  { -- | The criteria to use to sort the results.
    sortCriteria :: Prelude.Maybe ListJobsSortCriteria,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The criteria to use to filter the results.
    filterCriteria :: Prelude.Maybe ListJobsFilterCriteria,
    -- | The maximum number of items to include in each page of the response.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClassificationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortCriteria', 'listClassificationJobs_sortCriteria' - The criteria to use to sort the results.
--
-- 'nextToken', 'listClassificationJobs_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'filterCriteria', 'listClassificationJobs_filterCriteria' - The criteria to use to filter the results.
--
-- 'maxResults', 'listClassificationJobs_maxResults' - The maximum number of items to include in each page of the response.
newListClassificationJobs ::
  ListClassificationJobs
newListClassificationJobs =
  ListClassificationJobs'
    { sortCriteria =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The criteria to use to sort the results.
listClassificationJobs_sortCriteria :: Lens.Lens' ListClassificationJobs (Prelude.Maybe ListJobsSortCriteria)
listClassificationJobs_sortCriteria = Lens.lens (\ListClassificationJobs' {sortCriteria} -> sortCriteria) (\s@ListClassificationJobs' {} a -> s {sortCriteria = a} :: ListClassificationJobs)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listClassificationJobs_nextToken :: Lens.Lens' ListClassificationJobs (Prelude.Maybe Prelude.Text)
listClassificationJobs_nextToken = Lens.lens (\ListClassificationJobs' {nextToken} -> nextToken) (\s@ListClassificationJobs' {} a -> s {nextToken = a} :: ListClassificationJobs)

-- | The criteria to use to filter the results.
listClassificationJobs_filterCriteria :: Lens.Lens' ListClassificationJobs (Prelude.Maybe ListJobsFilterCriteria)
listClassificationJobs_filterCriteria = Lens.lens (\ListClassificationJobs' {filterCriteria} -> filterCriteria) (\s@ListClassificationJobs' {} a -> s {filterCriteria = a} :: ListClassificationJobs)

-- | The maximum number of items to include in each page of the response.
listClassificationJobs_maxResults :: Lens.Lens' ListClassificationJobs (Prelude.Maybe Prelude.Int)
listClassificationJobs_maxResults = Lens.lens (\ListClassificationJobs' {maxResults} -> maxResults) (\s@ListClassificationJobs' {} a -> s {maxResults = a} :: ListClassificationJobs)

instance Core.AWSPager ListClassificationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClassificationJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClassificationJobsResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listClassificationJobs_nextToken
          Lens..~ rs
          Lens.^? listClassificationJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListClassificationJobs where
  type
    AWSResponse ListClassificationJobs =
      ListClassificationJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClassificationJobsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClassificationJobs where
  hashWithSalt _salt ListClassificationJobs' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListClassificationJobs where
  rnf ListClassificationJobs' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListClassificationJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListClassificationJobs where
  toJSON ListClassificationJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sortCriteria" Data..=) Prelude.<$> sortCriteria,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListClassificationJobs where
  toPath = Prelude.const "/jobs/list"

instance Data.ToQuery ListClassificationJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListClassificationJobsResponse' smart constructor.
data ListClassificationJobsResponse = ListClassificationJobsResponse'
  { -- | An array of objects, one for each job that meets the filter criteria
    -- specified in the request.
    items :: Prelude.Maybe [JobSummary],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClassificationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listClassificationJobsResponse_items' - An array of objects, one for each job that meets the filter criteria
-- specified in the request.
--
-- 'nextToken', 'listClassificationJobsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'listClassificationJobsResponse_httpStatus' - The response's http status code.
newListClassificationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClassificationJobsResponse
newListClassificationJobsResponse pHttpStatus_ =
  ListClassificationJobsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each job that meets the filter criteria
-- specified in the request.
listClassificationJobsResponse_items :: Lens.Lens' ListClassificationJobsResponse (Prelude.Maybe [JobSummary])
listClassificationJobsResponse_items = Lens.lens (\ListClassificationJobsResponse' {items} -> items) (\s@ListClassificationJobsResponse' {} a -> s {items = a} :: ListClassificationJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listClassificationJobsResponse_nextToken :: Lens.Lens' ListClassificationJobsResponse (Prelude.Maybe Prelude.Text)
listClassificationJobsResponse_nextToken = Lens.lens (\ListClassificationJobsResponse' {nextToken} -> nextToken) (\s@ListClassificationJobsResponse' {} a -> s {nextToken = a} :: ListClassificationJobsResponse)

-- | The response's http status code.
listClassificationJobsResponse_httpStatus :: Lens.Lens' ListClassificationJobsResponse Prelude.Int
listClassificationJobsResponse_httpStatus = Lens.lens (\ListClassificationJobsResponse' {httpStatus} -> httpStatus) (\s@ListClassificationJobsResponse' {} a -> s {httpStatus = a} :: ListClassificationJobsResponse)

instance
  Prelude.NFData
    ListClassificationJobsResponse
  where
  rnf ListClassificationJobsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
