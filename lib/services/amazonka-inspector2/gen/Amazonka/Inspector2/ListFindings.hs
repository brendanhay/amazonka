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
-- Module      : Amazonka.Inspector2.ListFindings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists findings for your environment.
--
-- This operation returns paginated results.
module Amazonka.Inspector2.ListFindings
  ( -- * Creating a Request
    ListFindings (..),
    newListFindings,

    -- * Request Lenses
    listFindings_sortCriteria,
    listFindings_nextToken,
    listFindings_filterCriteria,
    listFindings_maxResults,

    -- * Destructuring the Response
    ListFindingsResponse (..),
    newListFindingsResponse,

    -- * Response Lenses
    listFindingsResponse_findings,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFindings' smart constructor.
data ListFindings = ListFindings'
  { -- | Details on the sort criteria to apply to your finding results.
    sortCriteria :: Prelude.Maybe SortCriteria,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Details on the filters to apply to your finding results.
    filterCriteria :: Prelude.Maybe FilterCriteria,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortCriteria', 'listFindings_sortCriteria' - Details on the sort criteria to apply to your finding results.
--
-- 'nextToken', 'listFindings_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'filterCriteria', 'listFindings_filterCriteria' - Details on the filters to apply to your finding results.
--
-- 'maxResults', 'listFindings_maxResults' - The maximum number of results to return in the response.
newListFindings ::
  ListFindings
newListFindings =
  ListFindings'
    { sortCriteria = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Details on the sort criteria to apply to your finding results.
listFindings_sortCriteria :: Lens.Lens' ListFindings (Prelude.Maybe SortCriteria)
listFindings_sortCriteria = Lens.lens (\ListFindings' {sortCriteria} -> sortCriteria) (\s@ListFindings' {} a -> s {sortCriteria = a} :: ListFindings)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listFindings_nextToken :: Lens.Lens' ListFindings (Prelude.Maybe Prelude.Text)
listFindings_nextToken = Lens.lens (\ListFindings' {nextToken} -> nextToken) (\s@ListFindings' {} a -> s {nextToken = a} :: ListFindings)

-- | Details on the filters to apply to your finding results.
listFindings_filterCriteria :: Lens.Lens' ListFindings (Prelude.Maybe FilterCriteria)
listFindings_filterCriteria = Lens.lens (\ListFindings' {filterCriteria} -> filterCriteria) (\s@ListFindings' {} a -> s {filterCriteria = a} :: ListFindings)

-- | The maximum number of results to return in the response.
listFindings_maxResults :: Lens.Lens' ListFindings (Prelude.Maybe Prelude.Natural)
listFindings_maxResults = Lens.lens (\ListFindings' {maxResults} -> maxResults) (\s@ListFindings' {} a -> s {maxResults = a} :: ListFindings)

instance Core.AWSPager ListFindings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFindingsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFindingsResponse_findings Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFindings_nextToken
          Lens..~ rs
          Lens.^? listFindingsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFindings where
  type AWSResponse ListFindings = ListFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFindingsResponse'
            Prelude.<$> (x Core..?> "findings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFindings where
  hashWithSalt _salt ListFindings' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFindings where
  rnf ListFindings' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFindings where
  toJSON ListFindings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sortCriteria" Core..=) Prelude.<$> sortCriteria,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filterCriteria" Core..=)
              Prelude.<$> filterCriteria,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListFindings where
  toPath = Prelude.const "/findings/list"

instance Core.ToQuery ListFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFindingsResponse' smart constructor.
data ListFindingsResponse = ListFindingsResponse'
  { -- | Contains details on the findings in your environment.
    findings :: Prelude.Maybe [Finding],
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
-- Create a value of 'ListFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findings', 'listFindingsResponse_findings' - Contains details on the findings in your environment.
--
-- 'nextToken', 'listFindingsResponse_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'httpStatus', 'listFindingsResponse_httpStatus' - The response's http status code.
newListFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFindingsResponse
newListFindingsResponse pHttpStatus_ =
  ListFindingsResponse'
    { findings = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains details on the findings in your environment.
listFindingsResponse_findings :: Lens.Lens' ListFindingsResponse (Prelude.Maybe [Finding])
listFindingsResponse_findings = Lens.lens (\ListFindingsResponse' {findings} -> findings) (\s@ListFindingsResponse' {} a -> s {findings = a} :: ListFindingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listFindingsResponse_nextToken :: Lens.Lens' ListFindingsResponse (Prelude.Maybe Prelude.Text)
listFindingsResponse_nextToken = Lens.lens (\ListFindingsResponse' {nextToken} -> nextToken) (\s@ListFindingsResponse' {} a -> s {nextToken = a} :: ListFindingsResponse)

-- | The response's http status code.
listFindingsResponse_httpStatus :: Lens.Lens' ListFindingsResponse Prelude.Int
listFindingsResponse_httpStatus = Lens.lens (\ListFindingsResponse' {httpStatus} -> httpStatus) (\s@ListFindingsResponse' {} a -> s {httpStatus = a} :: ListFindingsResponse)

instance Prelude.NFData ListFindingsResponse where
  rnf ListFindingsResponse' {..} =
    Prelude.rnf findings
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
