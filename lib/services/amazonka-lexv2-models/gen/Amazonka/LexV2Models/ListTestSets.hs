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
-- Module      : Amazonka.LexV2Models.ListTestSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The list of the test sets
module Amazonka.LexV2Models.ListTestSets
  ( -- * Creating a Request
    ListTestSets (..),
    newListTestSets,

    -- * Request Lenses
    listTestSets_maxResults,
    listTestSets_nextToken,
    listTestSets_sortBy,

    -- * Destructuring the Response
    ListTestSetsResponse (..),
    newListTestSetsResponse,

    -- * Response Lenses
    listTestSetsResponse_nextToken,
    listTestSetsResponse_testSets,
    listTestSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTestSets' smart constructor.
data ListTestSets = ListTestSets'
  { -- | The maximum number of test sets to return in each page. If there are
    -- fewer results than the max page size, only the actual number of results
    -- are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the ListTestSets operation contains more results
    -- than specified in the maxResults parameter, a token is returned in the
    -- response. Use that token in the nextToken parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for the list of test sets.
    sortBy :: Prelude.Maybe TestSetSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTestSets_maxResults' - The maximum number of test sets to return in each page. If there are
-- fewer results than the max page size, only the actual number of results
-- are returned.
--
-- 'nextToken', 'listTestSets_nextToken' - If the response from the ListTestSets operation contains more results
-- than specified in the maxResults parameter, a token is returned in the
-- response. Use that token in the nextToken parameter to return the next
-- page of results.
--
-- 'sortBy', 'listTestSets_sortBy' - The sort order for the list of test sets.
newListTestSets ::
  ListTestSets
newListTestSets =
  ListTestSets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | The maximum number of test sets to return in each page. If there are
-- fewer results than the max page size, only the actual number of results
-- are returned.
listTestSets_maxResults :: Lens.Lens' ListTestSets (Prelude.Maybe Prelude.Natural)
listTestSets_maxResults = Lens.lens (\ListTestSets' {maxResults} -> maxResults) (\s@ListTestSets' {} a -> s {maxResults = a} :: ListTestSets)

-- | If the response from the ListTestSets operation contains more results
-- than specified in the maxResults parameter, a token is returned in the
-- response. Use that token in the nextToken parameter to return the next
-- page of results.
listTestSets_nextToken :: Lens.Lens' ListTestSets (Prelude.Maybe Prelude.Text)
listTestSets_nextToken = Lens.lens (\ListTestSets' {nextToken} -> nextToken) (\s@ListTestSets' {} a -> s {nextToken = a} :: ListTestSets)

-- | The sort order for the list of test sets.
listTestSets_sortBy :: Lens.Lens' ListTestSets (Prelude.Maybe TestSetSortBy)
listTestSets_sortBy = Lens.lens (\ListTestSets' {sortBy} -> sortBy) (\s@ListTestSets' {} a -> s {sortBy = a} :: ListTestSets)

instance Core.AWSRequest ListTestSets where
  type AWSResponse ListTestSets = ListTestSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestSetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "testSets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTestSets where
  hashWithSalt _salt ListTestSets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy

instance Prelude.NFData ListTestSets where
  rnf ListTestSets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy

instance Data.ToHeaders ListTestSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTestSets where
  toJSON ListTestSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy
          ]
      )

instance Data.ToPath ListTestSets where
  toPath = Prelude.const "/testsets"

instance Data.ToQuery ListTestSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestSetsResponse' smart constructor.
data ListTestSetsResponse = ListTestSetsResponse'
  { -- | A token that indicates whether there are more results to return in a
    -- response to the ListTestSets operation. If the nextToken field is
    -- present, you send the contents as the nextToken parameter of a
    -- ListTestSets operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The selected test sets in a list of test sets.
    testSets :: Prelude.Maybe [TestSetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestSetsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the ListTestSets operation. If the nextToken field is
-- present, you send the contents as the nextToken parameter of a
-- ListTestSets operation request to get the next page of results.
--
-- 'testSets', 'listTestSetsResponse_testSets' - The selected test sets in a list of test sets.
--
-- 'httpStatus', 'listTestSetsResponse_httpStatus' - The response's http status code.
newListTestSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestSetsResponse
newListTestSetsResponse pHttpStatus_ =
  ListTestSetsResponse'
    { nextToken = Prelude.Nothing,
      testSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates whether there are more results to return in a
-- response to the ListTestSets operation. If the nextToken field is
-- present, you send the contents as the nextToken parameter of a
-- ListTestSets operation request to get the next page of results.
listTestSetsResponse_nextToken :: Lens.Lens' ListTestSetsResponse (Prelude.Maybe Prelude.Text)
listTestSetsResponse_nextToken = Lens.lens (\ListTestSetsResponse' {nextToken} -> nextToken) (\s@ListTestSetsResponse' {} a -> s {nextToken = a} :: ListTestSetsResponse)

-- | The selected test sets in a list of test sets.
listTestSetsResponse_testSets :: Lens.Lens' ListTestSetsResponse (Prelude.Maybe [TestSetSummary])
listTestSetsResponse_testSets = Lens.lens (\ListTestSetsResponse' {testSets} -> testSets) (\s@ListTestSetsResponse' {} a -> s {testSets = a} :: ListTestSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTestSetsResponse_httpStatus :: Lens.Lens' ListTestSetsResponse Prelude.Int
listTestSetsResponse_httpStatus = Lens.lens (\ListTestSetsResponse' {httpStatus} -> httpStatus) (\s@ListTestSetsResponse' {} a -> s {httpStatus = a} :: ListTestSetsResponse)

instance Prelude.NFData ListTestSetsResponse where
  rnf ListTestSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf testSets
      `Prelude.seq` Prelude.rnf httpStatus
