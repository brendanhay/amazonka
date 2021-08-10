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
-- Module      : Network.AWS.Route53AutoNaming.ListOperations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists operations that match the criteria that you specify.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListOperations
  ( -- * Creating a Request
    ListOperations (..),
    newListOperations,

    -- * Request Lenses
    listOperations_nextToken,
    listOperations_maxResults,
    listOperations_filters,

    -- * Destructuring the Response
    ListOperationsResponse (..),
    newListOperationsResponse,

    -- * Response Lenses
    listOperationsResponse_nextToken,
    listOperationsResponse_operations,
    listOperationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | For the first @ListOperations@ request, omit this value.
    --
    -- If the response contains @NextToken@, submit another @ListOperations@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- AWS Cloud Map gets @MaxResults@ operations and then filters them based
    -- on the specified criteria. It\'s possible that no operations in the
    -- first @MaxResults@ operations matched the specified criteria but that
    -- subsequent groups of @MaxResults@ operations do contain operations that
    -- match the criteria.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items that you want AWS Cloud Map to return in the
    -- response to a @ListOperations@ request. If you don\'t specify a value
    -- for @MaxResults@, AWS Cloud Map returns up to 100 operations.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A complex type that contains specifications for the operations that you
    -- want to list, for example, operations that you started between a
    -- specified start date and end date.
    --
    -- If you specify more than one filter, an operation must match all filters
    -- to be returned by @ListOperations@.
    filters :: Prelude.Maybe [OperationFilter]
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
-- 'nextToken', 'listOperations_nextToken' - For the first @ListOperations@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListOperations@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ operations and then filters them based
-- on the specified criteria. It\'s possible that no operations in the
-- first @MaxResults@ operations matched the specified criteria but that
-- subsequent groups of @MaxResults@ operations do contain operations that
-- match the criteria.
--
-- 'maxResults', 'listOperations_maxResults' - The maximum number of items that you want AWS Cloud Map to return in the
-- response to a @ListOperations@ request. If you don\'t specify a value
-- for @MaxResults@, AWS Cloud Map returns up to 100 operations.
--
-- 'filters', 'listOperations_filters' - A complex type that contains specifications for the operations that you
-- want to list, for example, operations that you started between a
-- specified start date and end date.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListOperations@.
newListOperations ::
  ListOperations
newListOperations =
  ListOperations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | For the first @ListOperations@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListOperations@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ operations and then filters them based
-- on the specified criteria. It\'s possible that no operations in the
-- first @MaxResults@ operations matched the specified criteria but that
-- subsequent groups of @MaxResults@ operations do contain operations that
-- match the criteria.
listOperations_nextToken :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Text)
listOperations_nextToken = Lens.lens (\ListOperations' {nextToken} -> nextToken) (\s@ListOperations' {} a -> s {nextToken = a} :: ListOperations)

-- | The maximum number of items that you want AWS Cloud Map to return in the
-- response to a @ListOperations@ request. If you don\'t specify a value
-- for @MaxResults@, AWS Cloud Map returns up to 100 operations.
listOperations_maxResults :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Natural)
listOperations_maxResults = Lens.lens (\ListOperations' {maxResults} -> maxResults) (\s@ListOperations' {} a -> s {maxResults = a} :: ListOperations)

-- | A complex type that contains specifications for the operations that you
-- want to list, for example, operations that you started between a
-- specified start date and end date.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListOperations@.
listOperations_filters :: Lens.Lens' ListOperations (Prelude.Maybe [OperationFilter])
listOperations_filters = Lens.lens (\ListOperations' {filters} -> filters) (\s@ListOperations' {} a -> s {filters = a} :: ListOperations) Prelude.. Lens.mapping Lens._Coerce

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOperations

instance Prelude.NFData ListOperations

instance Core.ToHeaders ListOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.ListOperations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListOperations where
  toJSON ListOperations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath ListOperations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListOperations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | If the response contains @NextToken@, submit another @ListOperations@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- AWS Cloud Map gets @MaxResults@ operations and then filters them based
    -- on the specified criteria. It\'s possible that no operations in the
    -- first @MaxResults@ operations matched the specified criteria but that
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
-- AWS Cloud Map gets @MaxResults@ operations and then filters them based
-- on the specified criteria. It\'s possible that no operations in the
-- first @MaxResults@ operations matched the specified criteria but that
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
-- AWS Cloud Map gets @MaxResults@ operations and then filters them based
-- on the specified criteria. It\'s possible that no operations in the
-- first @MaxResults@ operations matched the specified criteria but that
-- subsequent groups of @MaxResults@ operations do contain operations that
-- match the criteria.
listOperationsResponse_nextToken :: Lens.Lens' ListOperationsResponse (Prelude.Maybe Prelude.Text)
listOperationsResponse_nextToken = Lens.lens (\ListOperationsResponse' {nextToken} -> nextToken) (\s@ListOperationsResponse' {} a -> s {nextToken = a} :: ListOperationsResponse)

-- | Summary information about the operations that match the specified
-- criteria.
listOperationsResponse_operations :: Lens.Lens' ListOperationsResponse (Prelude.Maybe [OperationSummary])
listOperationsResponse_operations = Lens.lens (\ListOperationsResponse' {operations} -> operations) (\s@ListOperationsResponse' {} a -> s {operations = a} :: ListOperationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOperationsResponse_httpStatus :: Lens.Lens' ListOperationsResponse Prelude.Int
listOperationsResponse_httpStatus = Lens.lens (\ListOperationsResponse' {httpStatus} -> httpStatus) (\s@ListOperationsResponse' {} a -> s {httpStatus = a} :: ListOperationsResponse)

instance Prelude.NFData ListOperationsResponse
