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
-- Module      : Amazonka.SSMSAP.ListOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the operations performed by AWS Systems Manager for SAP.
--
-- This operation returns paginated results.
module Amazonka.SSMSAP.ListOperations
  ( -- * Creating a Request
    ListOperations (..),
    newListOperations,

    -- * Request Lenses
    listOperations_filters,
    listOperations_maxResults,
    listOperations_nextToken,
    listOperations_applicationId,

    -- * Destructuring the Response
    ListOperationsResponse (..),
    newListOperationsResponse,

    -- * Response Lenses
    listOperationsResponse_nextToken,
    listOperationsResponse_operations,
    listOperationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | The filters of an operation.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned nextToken
    -- value. If you do not specify a value for MaxResults, the request returns
    -- 50 items per page by default.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listOperations_filters' - The filters of an operation.
--
-- 'maxResults', 'listOperations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned nextToken
-- value. If you do not specify a value for MaxResults, the request returns
-- 50 items per page by default.
--
-- 'nextToken', 'listOperations_nextToken' - The token for the next page of results.
--
-- 'applicationId', 'listOperations_applicationId' - The ID of the application.
newListOperations ::
  -- | 'applicationId'
  Prelude.Text ->
  ListOperations
newListOperations pApplicationId_ =
  ListOperations'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The filters of an operation.
listOperations_filters :: Lens.Lens' ListOperations (Prelude.Maybe (Prelude.NonEmpty Filter))
listOperations_filters = Lens.lens (\ListOperations' {filters} -> filters) (\s@ListOperations' {} a -> s {filters = a} :: ListOperations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned nextToken
-- value. If you do not specify a value for MaxResults, the request returns
-- 50 items per page by default.
listOperations_maxResults :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Natural)
listOperations_maxResults = Lens.lens (\ListOperations' {maxResults} -> maxResults) (\s@ListOperations' {} a -> s {maxResults = a} :: ListOperations)

-- | The token for the next page of results.
listOperations_nextToken :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Text)
listOperations_nextToken = Lens.lens (\ListOperations' {nextToken} -> nextToken) (\s@ListOperations' {} a -> s {nextToken = a} :: ListOperations)

-- | The ID of the application.
listOperations_applicationId :: Lens.Lens' ListOperations Prelude.Text
listOperations_applicationId = Lens.lens (\ListOperations' {applicationId} -> applicationId) (\s@ListOperations' {} a -> s {applicationId = a} :: ListOperations)

instance Core.AWSPager ListOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOperationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOperationsResponse_operations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listOperations_nextToken
          Lens..~ rs
          Lens.^? listOperationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListOperations where
  type
    AWSResponse ListOperations =
      ListOperationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOperations where
  hashWithSalt _salt ListOperations' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListOperations where
  rnf ListOperations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders ListOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListOperations where
  toJSON ListOperations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ApplicationId" Data..= applicationId)
          ]
      )

instance Data.ToPath ListOperations where
  toPath = Prelude.const "/list-operations"

instance Data.ToQuery ListOperations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of operations performed by AWS Systems Manager for SAP.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOperationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'operations', 'listOperationsResponse_operations' - List of operations performed by AWS Systems Manager for SAP.
--
-- 'httpStatus', 'listOperationsResponse_httpStatus' - The response's http status code.
newListOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOperationsResponse
newListOperationsResponse pHttpStatus_ =
  ListOperationsResponse'
    { nextToken =
        Prelude.Nothing,
      operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listOperationsResponse_nextToken :: Lens.Lens' ListOperationsResponse (Prelude.Maybe Prelude.Text)
listOperationsResponse_nextToken = Lens.lens (\ListOperationsResponse' {nextToken} -> nextToken) (\s@ListOperationsResponse' {} a -> s {nextToken = a} :: ListOperationsResponse)

-- | List of operations performed by AWS Systems Manager for SAP.
listOperationsResponse_operations :: Lens.Lens' ListOperationsResponse (Prelude.Maybe [Operation])
listOperationsResponse_operations = Lens.lens (\ListOperationsResponse' {operations} -> operations) (\s@ListOperationsResponse' {} a -> s {operations = a} :: ListOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOperationsResponse_httpStatus :: Lens.Lens' ListOperationsResponse Prelude.Int
listOperationsResponse_httpStatus = Lens.lens (\ListOperationsResponse' {httpStatus} -> httpStatus) (\s@ListOperationsResponse' {} a -> s {httpStatus = a} :: ListOperationsResponse)

instance Prelude.NFData ListOperationsResponse where
  rnf ListOperationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
