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
-- Module      : Amazonka.Inspector2.ListCoverage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists coverage details for you environment.
--
-- This operation returns paginated results.
module Amazonka.Inspector2.ListCoverage
  ( -- * Creating a Request
    ListCoverage (..),
    newListCoverage,

    -- * Request Lenses
    listCoverage_filterCriteria,
    listCoverage_maxResults,
    listCoverage_nextToken,

    -- * Destructuring the Response
    ListCoverageResponse (..),
    newListCoverageResponse,

    -- * Response Lenses
    listCoverageResponse_coveredResources,
    listCoverageResponse_nextToken,
    listCoverageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoverage' smart constructor.
data ListCoverage = ListCoverage'
  { -- | An object that contains details on the filters to apply to the coverage
    -- data for your environment.
    filterCriteria :: Prelude.Maybe CoverageFilterCriteria,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filterCriteria', 'listCoverage_filterCriteria' - An object that contains details on the filters to apply to the coverage
-- data for your environment.
--
-- 'maxResults', 'listCoverage_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listCoverage_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
newListCoverage ::
  ListCoverage
newListCoverage =
  ListCoverage'
    { filterCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An object that contains details on the filters to apply to the coverage
-- data for your environment.
listCoverage_filterCriteria :: Lens.Lens' ListCoverage (Prelude.Maybe CoverageFilterCriteria)
listCoverage_filterCriteria = Lens.lens (\ListCoverage' {filterCriteria} -> filterCriteria) (\s@ListCoverage' {} a -> s {filterCriteria = a} :: ListCoverage)

-- | The maximum number of results to return in the response.
listCoverage_maxResults :: Lens.Lens' ListCoverage (Prelude.Maybe Prelude.Natural)
listCoverage_maxResults = Lens.lens (\ListCoverage' {maxResults} -> maxResults) (\s@ListCoverage' {} a -> s {maxResults = a} :: ListCoverage)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listCoverage_nextToken :: Lens.Lens' ListCoverage (Prelude.Maybe Prelude.Text)
listCoverage_nextToken = Lens.lens (\ListCoverage' {nextToken} -> nextToken) (\s@ListCoverage' {} a -> s {nextToken = a} :: ListCoverage)

instance Core.AWSPager ListCoverage where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoverageResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoverageResponse_coveredResources
            Prelude.. Lens._Just
        ) =
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
            Prelude.<$> ( x
                            Data..?> "coveredResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCoverage where
  hashWithSalt _salt ListCoverage' {..} =
    _salt
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCoverage where
  rnf ListCoverage' {..} =
    Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

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
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCoverage where
  toPath = Prelude.const "/coverage/list"

instance Data.ToQuery ListCoverage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCoverageResponse' smart constructor.
data ListCoverageResponse = ListCoverageResponse'
  { -- | An object that contains details on the covered resources in your
    -- environment.
    coveredResources :: Prelude.Maybe [CoveredResource],
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'coveredResources', 'listCoverageResponse_coveredResources' - An object that contains details on the covered resources in your
-- environment.
--
-- 'nextToken', 'listCoverageResponse_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'httpStatus', 'listCoverageResponse_httpStatus' - The response's http status code.
newListCoverageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoverageResponse
newListCoverageResponse pHttpStatus_ =
  ListCoverageResponse'
    { coveredResources =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains details on the covered resources in your
-- environment.
listCoverageResponse_coveredResources :: Lens.Lens' ListCoverageResponse (Prelude.Maybe [CoveredResource])
listCoverageResponse_coveredResources = Lens.lens (\ListCoverageResponse' {coveredResources} -> coveredResources) (\s@ListCoverageResponse' {} a -> s {coveredResources = a} :: ListCoverageResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listCoverageResponse_nextToken :: Lens.Lens' ListCoverageResponse (Prelude.Maybe Prelude.Text)
listCoverageResponse_nextToken = Lens.lens (\ListCoverageResponse' {nextToken} -> nextToken) (\s@ListCoverageResponse' {} a -> s {nextToken = a} :: ListCoverageResponse)

-- | The response's http status code.
listCoverageResponse_httpStatus :: Lens.Lens' ListCoverageResponse Prelude.Int
listCoverageResponse_httpStatus = Lens.lens (\ListCoverageResponse' {httpStatus} -> httpStatus) (\s@ListCoverageResponse' {} a -> s {httpStatus = a} :: ListCoverageResponse)

instance Prelude.NFData ListCoverageResponse where
  rnf ListCoverageResponse' {..} =
    Prelude.rnf coveredResources
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
