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
-- Module      : Amazonka.LexV2Models.ListTestExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The list of test set executions.
module Amazonka.LexV2Models.ListTestExecutions
  ( -- * Creating a Request
    ListTestExecutions (..),
    newListTestExecutions,

    -- * Request Lenses
    listTestExecutions_maxResults,
    listTestExecutions_nextToken,
    listTestExecutions_sortBy,

    -- * Destructuring the Response
    ListTestExecutionsResponse (..),
    newListTestExecutionsResponse,

    -- * Response Lenses
    listTestExecutionsResponse_nextToken,
    listTestExecutionsResponse_testExecutions,
    listTestExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTestExecutions' smart constructor.
data ListTestExecutions = ListTestExecutions'
  { -- | The maximum number of test executions to return in each page. If there
    -- are fewer results than the max page size, only the actual number of
    -- results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the ListTestExecutions operation contains more
    -- results than specified in the maxResults parameter, a token is returned
    -- in the response. Use that token in the nextToken parameter to return the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order of the test set executions.
    sortBy :: Prelude.Maybe TestExecutionSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTestExecutions_maxResults' - The maximum number of test executions to return in each page. If there
-- are fewer results than the max page size, only the actual number of
-- results are returned.
--
-- 'nextToken', 'listTestExecutions_nextToken' - If the response from the ListTestExecutions operation contains more
-- results than specified in the maxResults parameter, a token is returned
-- in the response. Use that token in the nextToken parameter to return the
-- next page of results.
--
-- 'sortBy', 'listTestExecutions_sortBy' - The sort order of the test set executions.
newListTestExecutions ::
  ListTestExecutions
newListTestExecutions =
  ListTestExecutions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | The maximum number of test executions to return in each page. If there
-- are fewer results than the max page size, only the actual number of
-- results are returned.
listTestExecutions_maxResults :: Lens.Lens' ListTestExecutions (Prelude.Maybe Prelude.Natural)
listTestExecutions_maxResults = Lens.lens (\ListTestExecutions' {maxResults} -> maxResults) (\s@ListTestExecutions' {} a -> s {maxResults = a} :: ListTestExecutions)

-- | If the response from the ListTestExecutions operation contains more
-- results than specified in the maxResults parameter, a token is returned
-- in the response. Use that token in the nextToken parameter to return the
-- next page of results.
listTestExecutions_nextToken :: Lens.Lens' ListTestExecutions (Prelude.Maybe Prelude.Text)
listTestExecutions_nextToken = Lens.lens (\ListTestExecutions' {nextToken} -> nextToken) (\s@ListTestExecutions' {} a -> s {nextToken = a} :: ListTestExecutions)

-- | The sort order of the test set executions.
listTestExecutions_sortBy :: Lens.Lens' ListTestExecutions (Prelude.Maybe TestExecutionSortBy)
listTestExecutions_sortBy = Lens.lens (\ListTestExecutions' {sortBy} -> sortBy) (\s@ListTestExecutions' {} a -> s {sortBy = a} :: ListTestExecutions)

instance Core.AWSRequest ListTestExecutions where
  type
    AWSResponse ListTestExecutions =
      ListTestExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestExecutionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "testExecutions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTestExecutions where
  hashWithSalt _salt ListTestExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy

instance Prelude.NFData ListTestExecutions where
  rnf ListTestExecutions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy

instance Data.ToHeaders ListTestExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTestExecutions where
  toJSON ListTestExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy
          ]
      )

instance Data.ToPath ListTestExecutions where
  toPath = Prelude.const "/testexecutions"

instance Data.ToQuery ListTestExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestExecutionsResponse' smart constructor.
data ListTestExecutionsResponse = ListTestExecutionsResponse'
  { -- | A token that indicates whether there are more results to return in a
    -- response to the ListTestExecutions operation. If the nextToken field is
    -- present, you send the contents as the nextToken parameter of a
    -- ListTestExecutions operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of test executions.
    testExecutions :: Prelude.Maybe [TestExecutionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestExecutionsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the ListTestExecutions operation. If the nextToken field is
-- present, you send the contents as the nextToken parameter of a
-- ListTestExecutions operation request to get the next page of results.
--
-- 'testExecutions', 'listTestExecutionsResponse_testExecutions' - The list of test executions.
--
-- 'httpStatus', 'listTestExecutionsResponse_httpStatus' - The response's http status code.
newListTestExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestExecutionsResponse
newListTestExecutionsResponse pHttpStatus_ =
  ListTestExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      testExecutions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates whether there are more results to return in a
-- response to the ListTestExecutions operation. If the nextToken field is
-- present, you send the contents as the nextToken parameter of a
-- ListTestExecutions operation request to get the next page of results.
listTestExecutionsResponse_nextToken :: Lens.Lens' ListTestExecutionsResponse (Prelude.Maybe Prelude.Text)
listTestExecutionsResponse_nextToken = Lens.lens (\ListTestExecutionsResponse' {nextToken} -> nextToken) (\s@ListTestExecutionsResponse' {} a -> s {nextToken = a} :: ListTestExecutionsResponse)

-- | The list of test executions.
listTestExecutionsResponse_testExecutions :: Lens.Lens' ListTestExecutionsResponse (Prelude.Maybe [TestExecutionSummary])
listTestExecutionsResponse_testExecutions = Lens.lens (\ListTestExecutionsResponse' {testExecutions} -> testExecutions) (\s@ListTestExecutionsResponse' {} a -> s {testExecutions = a} :: ListTestExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTestExecutionsResponse_httpStatus :: Lens.Lens' ListTestExecutionsResponse Prelude.Int
listTestExecutionsResponse_httpStatus = Lens.lens (\ListTestExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListTestExecutionsResponse' {} a -> s {httpStatus = a} :: ListTestExecutionsResponse)

instance Prelude.NFData ListTestExecutionsResponse where
  rnf ListTestExecutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf testExecutions
      `Prelude.seq` Prelude.rnf httpStatus
