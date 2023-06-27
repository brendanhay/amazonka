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
-- Module      : Amazonka.GuardDuty.ListCoverage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists coverage details for your GuardDuty account. If you\'re a
-- GuardDuty administrator, you can retrieve all resources associated with
-- the active member accounts in your organization.
--
-- Make sure the accounts have EKS Runtime Monitoring enabled and GuardDuty
-- agent running on their EKS nodes.
--
-- This operation returns paginated results.
module Amazonka.GuardDuty.ListCoverage
  ( -- * Creating a Request
    ListCoverage (..),
    newListCoverage,

    -- * Request Lenses
    listCoverage_filterCriteria,
    listCoverage_maxResults,
    listCoverage_nextToken,
    listCoverage_sortCriteria,
    listCoverage_detectorId,

    -- * Destructuring the Response
    ListCoverageResponse (..),
    newListCoverageResponse,

    -- * Response Lenses
    listCoverageResponse_nextToken,
    listCoverageResponse_httpStatus,
    listCoverageResponse_resources,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoverage' smart constructor.
data ListCoverage = ListCoverage'
  { -- | Represents the criteria used to filter the coverage details.
    filterCriteria :: Prelude.Maybe CoverageFilterCriteria,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the NextToken value returned from the
    -- previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the criteria used to sort the coverage details.
    sortCriteria :: Prelude.Maybe CoverageSortCriteria,
    -- | The unique ID of the detector whose coverage details you want to
    -- retrieve.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoverage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterCriteria', 'listCoverage_filterCriteria' - Represents the criteria used to filter the coverage details.
--
-- 'maxResults', 'listCoverage_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listCoverage_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the NextToken value returned from the
-- previous request to continue listing results after the first page.
--
-- 'sortCriteria', 'listCoverage_sortCriteria' - Represents the criteria used to sort the coverage details.
--
-- 'detectorId', 'listCoverage_detectorId' - The unique ID of the detector whose coverage details you want to
-- retrieve.
newListCoverage ::
  -- | 'detectorId'
  Prelude.Text ->
  ListCoverage
newListCoverage pDetectorId_ =
  ListCoverage'
    { filterCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | Represents the criteria used to filter the coverage details.
listCoverage_filterCriteria :: Lens.Lens' ListCoverage (Prelude.Maybe CoverageFilterCriteria)
listCoverage_filterCriteria = Lens.lens (\ListCoverage' {filterCriteria} -> filterCriteria) (\s@ListCoverage' {} a -> s {filterCriteria = a} :: ListCoverage)

-- | The maximum number of results to return in the response.
listCoverage_maxResults :: Lens.Lens' ListCoverage (Prelude.Maybe Prelude.Natural)
listCoverage_maxResults = Lens.lens (\ListCoverage' {maxResults} -> maxResults) (\s@ListCoverage' {} a -> s {maxResults = a} :: ListCoverage)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the NextToken value returned from the
-- previous request to continue listing results after the first page.
listCoverage_nextToken :: Lens.Lens' ListCoverage (Prelude.Maybe Prelude.Text)
listCoverage_nextToken = Lens.lens (\ListCoverage' {nextToken} -> nextToken) (\s@ListCoverage' {} a -> s {nextToken = a} :: ListCoverage)

-- | Represents the criteria used to sort the coverage details.
listCoverage_sortCriteria :: Lens.Lens' ListCoverage (Prelude.Maybe CoverageSortCriteria)
listCoverage_sortCriteria = Lens.lens (\ListCoverage' {sortCriteria} -> sortCriteria) (\s@ListCoverage' {} a -> s {sortCriteria = a} :: ListCoverage)

-- | The unique ID of the detector whose coverage details you want to
-- retrieve.
listCoverage_detectorId :: Lens.Lens' ListCoverage Prelude.Text
listCoverage_detectorId = Lens.lens (\ListCoverage' {detectorId} -> detectorId) (\s@ListCoverage' {} a -> s {detectorId = a} :: ListCoverage)

instance Core.AWSPager ListCoverage where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoverageResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listCoverageResponse_resources) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCoverage_nextToken
          Lens..~ rs
          Lens.^? listCoverageResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCoverage where
  type AWSResponse ListCoverage = ListCoverageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoverageResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "resources" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListCoverage where
  hashWithSalt _salt ListCoverage' {..} =
    _salt
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` detectorId

instance Prelude.NFData ListCoverage where
  rnf ListCoverage' {..} =
    Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf detectorId

instance Data.ToHeaders ListCoverage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCoverage where
  toJSON ListCoverage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortCriteria" Data..=) Prelude.<$> sortCriteria
          ]
      )

instance Data.ToPath ListCoverage where
  toPath ListCoverage' {..} =
    Prelude.mconcat
      ["/detector/", Data.toBS detectorId, "/coverage"]

instance Data.ToQuery ListCoverage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCoverageResponse' smart constructor.
data ListCoverageResponse = ListCoverageResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of resources and their attributes providing cluster details.
    resources :: [CoverageResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoverageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoverageResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'httpStatus', 'listCoverageResponse_httpStatus' - The response's http status code.
--
-- 'resources', 'listCoverageResponse_resources' - A list of resources and their attributes providing cluster details.
newListCoverageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoverageResponse
newListCoverageResponse pHttpStatus_ =
  ListCoverageResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      resources = Prelude.mempty
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listCoverageResponse_nextToken :: Lens.Lens' ListCoverageResponse (Prelude.Maybe Prelude.Text)
listCoverageResponse_nextToken = Lens.lens (\ListCoverageResponse' {nextToken} -> nextToken) (\s@ListCoverageResponse' {} a -> s {nextToken = a} :: ListCoverageResponse)

-- | The response's http status code.
listCoverageResponse_httpStatus :: Lens.Lens' ListCoverageResponse Prelude.Int
listCoverageResponse_httpStatus = Lens.lens (\ListCoverageResponse' {httpStatus} -> httpStatus) (\s@ListCoverageResponse' {} a -> s {httpStatus = a} :: ListCoverageResponse)

-- | A list of resources and their attributes providing cluster details.
listCoverageResponse_resources :: Lens.Lens' ListCoverageResponse [CoverageResource]
listCoverageResponse_resources = Lens.lens (\ListCoverageResponse' {resources} -> resources) (\s@ListCoverageResponse' {} a -> s {resources = a} :: ListCoverageResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListCoverageResponse where
  rnf ListCoverageResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resources
