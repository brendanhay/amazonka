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
-- Module      : Amazonka.Route53AutoNaming.ListOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists operations that match the criteria that you specify.
--
-- This operation returns paginated results.
module Amazonka.Route53AutoNaming.ListOperations
  ( -- * Creating a Request
    ListOperations (..),
    newListOperations,

    -- * Request Lenses
    listOperations_filters,
    listOperations_maxResults,
    listOperations_nextToken,

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
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | A complex type that contains specifications for the operations that you
    -- want to list, for example, operations that you started between a
    -- specified start date and end date.
    --
    -- If you specify more than one filter, an operation must match all filters
    -- to be returned by @ListOperations@.
    filters :: Prelude.Maybe [OperationFilter],
    -- | The maximum number of items that you want Cloud Map to return in the
    -- response to a @ListOperations@ request. If you don\'t specify a value
    -- for @MaxResults@, Cloud Map returns up to 100 operations.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first @ListOperations@ request, omit this value.
    --
    -- If the response contains @NextToken@, submit another @ListOperations@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- Cloud Map gets @MaxResults@ operations and then filters them based on
    -- the specified criteria. It\'s possible that no operations in the first
    -- @MaxResults@ operations matched the specified criteria but that
    -- subsequent groups of @MaxResults@ operations do contain operations that
    -- match the criteria.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filters', 'listOperations_filters' - A complex type that contains specifications for the operations that you
-- want to list, for example, operations that you started between a
-- specified start date and end date.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListOperations@.
--
-- 'maxResults', 'listOperations_maxResults' - The maximum number of items that you want Cloud Map to return in the
-- response to a @ListOperations@ request. If you don\'t specify a value
-- for @MaxResults@, Cloud Map returns up to 100 operations.
--
-- 'nextToken', 'listOperations_nextToken' - For the first @ListOperations@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListOperations@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ operations and then filters them based on
-- the specified criteria. It\'s possible that no operations in the first
-- @MaxResults@ operations matched the specified criteria but that
-- subsequent groups of @MaxResults@ operations do contain operations that
-- match the criteria.
newListOperations ::
  ListOperations
newListOperations =
  ListOperations'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A complex type that contains specifications for the operations that you
-- want to list, for example, operations that you started between a
-- specified start date and end date.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListOperations@.
listOperations_filters :: Lens.Lens' ListOperations (Prelude.Maybe [OperationFilter])
listOperations_filters = Lens.lens (\ListOperations' {filters} -> filters) (\s@ListOperations' {} a -> s {filters = a} :: ListOperations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items that you want Cloud Map to return in the
-- response to a @ListOperations@ request. If you don\'t specify a value
-- for @MaxResults@, Cloud Map returns up to 100 operations.
listOperations_maxResults :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Natural)
listOperations_maxResults = Lens.lens (\ListOperations' {maxResults} -> maxResults) (\s@ListOperations' {} a -> s {maxResults = a} :: ListOperations)

-- | For the first @ListOperations@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListOperations@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ operations and then filters them based on
-- the specified criteria. It\'s possible that no operations in the first
-- @MaxResults@ operations matched the specified criteria but that
-- subsequent groups of @MaxResults@ operations do contain operations that
-- match the criteria.
listOperations_nextToken :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Text)
listOperations_nextToken = Lens.lens (\ListOperations' {nextToken} -> nextToken) (\s@ListOperations' {} a -> s {nextToken = a} :: ListOperations)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listOperations_nextToken
          Lens..~ rs
          Lens.^? listOperationsResponse_nextToken Prelude.. Lens._Just

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
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListOperations where
  rnf ListOperations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.ListOperations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
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
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListOperations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListOperations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | If the response contains @NextToken@, submit another @ListOperations@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- Cloud Map gets @MaxResults@ operations and then filters them based on
    -- the specified criteria. It\'s possible that no operations in the first
    -- @MaxResults@ operations matched the specified criteria but that
    -- subsequent groups of @MaxResults@ operations do contain operations that
    -- match the criteria.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information about the operations that match the specified
    -- criteria.
    operations :: Prelude.Maybe [OperationSummary],
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
-- 'nextToken', 'listOperationsResponse_nextToken' - If the response contains @NextToken@, submit another @ListOperations@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ operations and then filters them based on
-- the specified criteria. It\'s possible that no operations in the first
-- @MaxResults@ operations matched the specified criteria but that
-- subsequent groups of @MaxResults@ operations do contain operations that
-- match the criteria.
--
-- 'operations', 'listOperationsResponse_operations' - Summary information about the operations that match the specified
-- criteria.
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

-- | If the response contains @NextToken@, submit another @ListOperations@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ operations and then filters them based on
-- the specified criteria. It\'s possible that no operations in the first
-- @MaxResults@ operations matched the specified criteria but that
-- subsequent groups of @MaxResults@ operations do contain operations that
-- match the criteria.
listOperationsResponse_nextToken :: Lens.Lens' ListOperationsResponse (Prelude.Maybe Prelude.Text)
listOperationsResponse_nextToken = Lens.lens (\ListOperationsResponse' {nextToken} -> nextToken) (\s@ListOperationsResponse' {} a -> s {nextToken = a} :: ListOperationsResponse)

-- | Summary information about the operations that match the specified
-- criteria.
listOperationsResponse_operations :: Lens.Lens' ListOperationsResponse (Prelude.Maybe [OperationSummary])
listOperationsResponse_operations = Lens.lens (\ListOperationsResponse' {operations} -> operations) (\s@ListOperationsResponse' {} a -> s {operations = a} :: ListOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOperationsResponse_httpStatus :: Lens.Lens' ListOperationsResponse Prelude.Int
listOperationsResponse_httpStatus = Lens.lens (\ListOperationsResponse' {httpStatus} -> httpStatus) (\s@ListOperationsResponse' {} a -> s {httpStatus = a} :: ListOperationsResponse)

instance Prelude.NFData ListOperationsResponse where
  rnf ListOperationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
