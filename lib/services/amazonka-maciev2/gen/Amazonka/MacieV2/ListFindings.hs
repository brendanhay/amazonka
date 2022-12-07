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
-- Module      : Amazonka.MacieV2.ListFindings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a subset of information about one or more findings.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListFindings
  ( -- * Creating a Request
    ListFindings (..),
    newListFindings,

    -- * Request Lenses
    listFindings_sortCriteria,
    listFindings_nextToken,
    listFindings_findingCriteria,
    listFindings_maxResults,

    -- * Destructuring the Response
    ListFindingsResponse (..),
    newListFindingsResponse,

    -- * Response Lenses
    listFindingsResponse_nextToken,
    listFindingsResponse_findingIds,
    listFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFindings' smart constructor.
data ListFindings = ListFindings'
  { -- | The criteria to use to sort the results.
    sortCriteria :: Prelude.Maybe SortCriteria,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The criteria to use to filter the results.
    findingCriteria :: Prelude.Maybe FindingCriteria,
    -- | The maximum number of items to include in each page of the response.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'sortCriteria', 'listFindings_sortCriteria' - The criteria to use to sort the results.
--
-- 'nextToken', 'listFindings_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'findingCriteria', 'listFindings_findingCriteria' - The criteria to use to filter the results.
--
-- 'maxResults', 'listFindings_maxResults' - The maximum number of items to include in each page of the response.
newListFindings ::
  ListFindings
newListFindings =
  ListFindings'
    { sortCriteria = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      findingCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The criteria to use to sort the results.
listFindings_sortCriteria :: Lens.Lens' ListFindings (Prelude.Maybe SortCriteria)
listFindings_sortCriteria = Lens.lens (\ListFindings' {sortCriteria} -> sortCriteria) (\s@ListFindings' {} a -> s {sortCriteria = a} :: ListFindings)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listFindings_nextToken :: Lens.Lens' ListFindings (Prelude.Maybe Prelude.Text)
listFindings_nextToken = Lens.lens (\ListFindings' {nextToken} -> nextToken) (\s@ListFindings' {} a -> s {nextToken = a} :: ListFindings)

-- | The criteria to use to filter the results.
listFindings_findingCriteria :: Lens.Lens' ListFindings (Prelude.Maybe FindingCriteria)
listFindings_findingCriteria = Lens.lens (\ListFindings' {findingCriteria} -> findingCriteria) (\s@ListFindings' {} a -> s {findingCriteria = a} :: ListFindings)

-- | The maximum number of items to include in each page of the response.
listFindings_maxResults :: Lens.Lens' ListFindings (Prelude.Maybe Prelude.Int)
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
            Lens.^? listFindingsResponse_findingIds Prelude.. Lens._Just
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
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "findingIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFindings where
  hashWithSalt _salt ListFindings' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` findingCriteria
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFindings where
  rnf ListFindings' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf findingCriteria
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFindings where
  toJSON ListFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sortCriteria" Data..=) Prelude.<$> sortCriteria,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("findingCriteria" Data..=)
              Prelude.<$> findingCriteria,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListFindings where
  toPath = Prelude.const "/findings"

instance Data.ToQuery ListFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFindingsResponse' smart constructor.
data ListFindingsResponse = ListFindingsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of strings, where each string is the unique identifier for a
    -- finding that meets the filter criteria specified in the request.
    findingIds :: Prelude.Maybe [Prelude.Text],
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
-- 'nextToken', 'listFindingsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'findingIds', 'listFindingsResponse_findingIds' - An array of strings, where each string is the unique identifier for a
-- finding that meets the filter criteria specified in the request.
--
-- 'httpStatus', 'listFindingsResponse_httpStatus' - The response's http status code.
newListFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFindingsResponse
newListFindingsResponse pHttpStatus_ =
  ListFindingsResponse'
    { nextToken = Prelude.Nothing,
      findingIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listFindingsResponse_nextToken :: Lens.Lens' ListFindingsResponse (Prelude.Maybe Prelude.Text)
listFindingsResponse_nextToken = Lens.lens (\ListFindingsResponse' {nextToken} -> nextToken) (\s@ListFindingsResponse' {} a -> s {nextToken = a} :: ListFindingsResponse)

-- | An array of strings, where each string is the unique identifier for a
-- finding that meets the filter criteria specified in the request.
listFindingsResponse_findingIds :: Lens.Lens' ListFindingsResponse (Prelude.Maybe [Prelude.Text])
listFindingsResponse_findingIds = Lens.lens (\ListFindingsResponse' {findingIds} -> findingIds) (\s@ListFindingsResponse' {} a -> s {findingIds = a} :: ListFindingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFindingsResponse_httpStatus :: Lens.Lens' ListFindingsResponse Prelude.Int
listFindingsResponse_httpStatus = Lens.lens (\ListFindingsResponse' {httpStatus} -> httpStatus) (\s@ListFindingsResponse' {} a -> s {httpStatus = a} :: ListFindingsResponse)

instance Prelude.NFData ListFindingsResponse where
  rnf ListFindingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf findingIds
      `Prelude.seq` Prelude.rnf httpStatus
