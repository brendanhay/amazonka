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
-- Module      : Amazonka.LexV2Models.ListTestExecutionResultItems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of test execution result items.
module Amazonka.LexV2Models.ListTestExecutionResultItems
  ( -- * Creating a Request
    ListTestExecutionResultItems (..),
    newListTestExecutionResultItems,

    -- * Request Lenses
    listTestExecutionResultItems_maxResults,
    listTestExecutionResultItems_nextToken,
    listTestExecutionResultItems_testExecutionId,
    listTestExecutionResultItems_resultFilterBy,

    -- * Destructuring the Response
    ListTestExecutionResultItemsResponse (..),
    newListTestExecutionResultItemsResponse,

    -- * Response Lenses
    listTestExecutionResultItemsResponse_nextToken,
    listTestExecutionResultItemsResponse_testExecutionResults,
    listTestExecutionResultItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTestExecutionResultItems' smart constructor.
data ListTestExecutionResultItems = ListTestExecutionResultItems'
  { -- | The maximum number of test execution result items to return in each
    -- page. If there are fewer results than the max page size, only the actual
    -- number of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the @ListTestExecutionResultItems@ operation
    -- contains more results than specified in the @maxResults@ parameter, a
    -- token is returned in the response. Use that token in the @nextToken@
    -- parameter to return the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the test execution to list the result items.
    testExecutionId :: Prelude.Text,
    -- | The filter for the list of results from the test set execution.
    resultFilterBy :: TestExecutionResultFilterBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestExecutionResultItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTestExecutionResultItems_maxResults' - The maximum number of test execution result items to return in each
-- page. If there are fewer results than the max page size, only the actual
-- number of results are returned.
--
-- 'nextToken', 'listTestExecutionResultItems_nextToken' - If the response from the @ListTestExecutionResultItems@ operation
-- contains more results than specified in the @maxResults@ parameter, a
-- token is returned in the response. Use that token in the @nextToken@
-- parameter to return the next page of results.
--
-- 'testExecutionId', 'listTestExecutionResultItems_testExecutionId' - The unique identifier of the test execution to list the result items.
--
-- 'resultFilterBy', 'listTestExecutionResultItems_resultFilterBy' - The filter for the list of results from the test set execution.
newListTestExecutionResultItems ::
  -- | 'testExecutionId'
  Prelude.Text ->
  -- | 'resultFilterBy'
  TestExecutionResultFilterBy ->
  ListTestExecutionResultItems
newListTestExecutionResultItems
  pTestExecutionId_
  pResultFilterBy_ =
    ListTestExecutionResultItems'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        testExecutionId = pTestExecutionId_,
        resultFilterBy = pResultFilterBy_
      }

-- | The maximum number of test execution result items to return in each
-- page. If there are fewer results than the max page size, only the actual
-- number of results are returned.
listTestExecutionResultItems_maxResults :: Lens.Lens' ListTestExecutionResultItems (Prelude.Maybe Prelude.Natural)
listTestExecutionResultItems_maxResults = Lens.lens (\ListTestExecutionResultItems' {maxResults} -> maxResults) (\s@ListTestExecutionResultItems' {} a -> s {maxResults = a} :: ListTestExecutionResultItems)

-- | If the response from the @ListTestExecutionResultItems@ operation
-- contains more results than specified in the @maxResults@ parameter, a
-- token is returned in the response. Use that token in the @nextToken@
-- parameter to return the next page of results.
listTestExecutionResultItems_nextToken :: Lens.Lens' ListTestExecutionResultItems (Prelude.Maybe Prelude.Text)
listTestExecutionResultItems_nextToken = Lens.lens (\ListTestExecutionResultItems' {nextToken} -> nextToken) (\s@ListTestExecutionResultItems' {} a -> s {nextToken = a} :: ListTestExecutionResultItems)

-- | The unique identifier of the test execution to list the result items.
listTestExecutionResultItems_testExecutionId :: Lens.Lens' ListTestExecutionResultItems Prelude.Text
listTestExecutionResultItems_testExecutionId = Lens.lens (\ListTestExecutionResultItems' {testExecutionId} -> testExecutionId) (\s@ListTestExecutionResultItems' {} a -> s {testExecutionId = a} :: ListTestExecutionResultItems)

-- | The filter for the list of results from the test set execution.
listTestExecutionResultItems_resultFilterBy :: Lens.Lens' ListTestExecutionResultItems TestExecutionResultFilterBy
listTestExecutionResultItems_resultFilterBy = Lens.lens (\ListTestExecutionResultItems' {resultFilterBy} -> resultFilterBy) (\s@ListTestExecutionResultItems' {} a -> s {resultFilterBy = a} :: ListTestExecutionResultItems)

instance Core.AWSRequest ListTestExecutionResultItems where
  type
    AWSResponse ListTestExecutionResultItems =
      ListTestExecutionResultItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestExecutionResultItemsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "testExecutionResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListTestExecutionResultItems
  where
  hashWithSalt _salt ListTestExecutionResultItems' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` testExecutionId
      `Prelude.hashWithSalt` resultFilterBy

instance Prelude.NFData ListTestExecutionResultItems where
  rnf ListTestExecutionResultItems' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf testExecutionId
      `Prelude.seq` Prelude.rnf resultFilterBy

instance Data.ToHeaders ListTestExecutionResultItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTestExecutionResultItems where
  toJSON ListTestExecutionResultItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("resultFilterBy" Data..= resultFilterBy)
          ]
      )

instance Data.ToPath ListTestExecutionResultItems where
  toPath ListTestExecutionResultItems' {..} =
    Prelude.mconcat
      [ "/testexecutions/",
        Data.toBS testExecutionId,
        "/results"
      ]

instance Data.ToQuery ListTestExecutionResultItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestExecutionResultItemsResponse' smart constructor.
data ListTestExecutionResultItemsResponse = ListTestExecutionResultItemsResponse'
  { -- | A token that indicates whether there are more results to return in a
    -- response to the @ListTestExecutionResultItems@ operation. If the
    -- @nextToken@ field is present, you send the contents as the @nextToken@
    -- parameter of a @ListTestExecutionResultItems@ operation request to get
    -- the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of results from the test execution.
    testExecutionResults :: Prelude.Maybe TestExecutionResultItems,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestExecutionResultItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestExecutionResultItemsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListTestExecutionResultItems@ operation. If the
-- @nextToken@ field is present, you send the contents as the @nextToken@
-- parameter of a @ListTestExecutionResultItems@ operation request to get
-- the next page of results.
--
-- 'testExecutionResults', 'listTestExecutionResultItemsResponse_testExecutionResults' - The list of results from the test execution.
--
-- 'httpStatus', 'listTestExecutionResultItemsResponse_httpStatus' - The response's http status code.
newListTestExecutionResultItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestExecutionResultItemsResponse
newListTestExecutionResultItemsResponse pHttpStatus_ =
  ListTestExecutionResultItemsResponse'
    { nextToken =
        Prelude.Nothing,
      testExecutionResults =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates whether there are more results to return in a
-- response to the @ListTestExecutionResultItems@ operation. If the
-- @nextToken@ field is present, you send the contents as the @nextToken@
-- parameter of a @ListTestExecutionResultItems@ operation request to get
-- the next page of results.
listTestExecutionResultItemsResponse_nextToken :: Lens.Lens' ListTestExecutionResultItemsResponse (Prelude.Maybe Prelude.Text)
listTestExecutionResultItemsResponse_nextToken = Lens.lens (\ListTestExecutionResultItemsResponse' {nextToken} -> nextToken) (\s@ListTestExecutionResultItemsResponse' {} a -> s {nextToken = a} :: ListTestExecutionResultItemsResponse)

-- | The list of results from the test execution.
listTestExecutionResultItemsResponse_testExecutionResults :: Lens.Lens' ListTestExecutionResultItemsResponse (Prelude.Maybe TestExecutionResultItems)
listTestExecutionResultItemsResponse_testExecutionResults = Lens.lens (\ListTestExecutionResultItemsResponse' {testExecutionResults} -> testExecutionResults) (\s@ListTestExecutionResultItemsResponse' {} a -> s {testExecutionResults = a} :: ListTestExecutionResultItemsResponse)

-- | The response's http status code.
listTestExecutionResultItemsResponse_httpStatus :: Lens.Lens' ListTestExecutionResultItemsResponse Prelude.Int
listTestExecutionResultItemsResponse_httpStatus = Lens.lens (\ListTestExecutionResultItemsResponse' {httpStatus} -> httpStatus) (\s@ListTestExecutionResultItemsResponse' {} a -> s {httpStatus = a} :: ListTestExecutionResultItemsResponse)

instance
  Prelude.NFData
    ListTestExecutionResultItemsResponse
  where
  rnf ListTestExecutionResultItemsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf testExecutionResults
      `Prelude.seq` Prelude.rnf httpStatus
