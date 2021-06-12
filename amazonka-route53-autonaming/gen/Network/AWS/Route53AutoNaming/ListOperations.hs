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
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items that you want AWS Cloud Map to return in the
    -- response to a @ListOperations@ request. If you don\'t specify a value
    -- for @MaxResults@, AWS Cloud Map returns up to 100 operations.
    maxResults :: Core.Maybe Core.Natural,
    -- | A complex type that contains specifications for the operations that you
    -- want to list, for example, operations that you started between a
    -- specified start date and end date.
    --
    -- If you specify more than one filter, an operation must match all filters
    -- to be returned by @ListOperations@.
    filters :: Core.Maybe [OperationFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
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
listOperations_nextToken :: Lens.Lens' ListOperations (Core.Maybe Core.Text)
listOperations_nextToken = Lens.lens (\ListOperations' {nextToken} -> nextToken) (\s@ListOperations' {} a -> s {nextToken = a} :: ListOperations)

-- | The maximum number of items that you want AWS Cloud Map to return in the
-- response to a @ListOperations@ request. If you don\'t specify a value
-- for @MaxResults@, AWS Cloud Map returns up to 100 operations.
listOperations_maxResults :: Lens.Lens' ListOperations (Core.Maybe Core.Natural)
listOperations_maxResults = Lens.lens (\ListOperations' {maxResults} -> maxResults) (\s@ListOperations' {} a -> s {maxResults = a} :: ListOperations)

-- | A complex type that contains specifications for the operations that you
-- want to list, for example, operations that you started between a
-- specified start date and end date.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListOperations@.
listOperations_filters :: Lens.Lens' ListOperations (Core.Maybe [OperationFilter])
listOperations_filters = Lens.lens (\ListOperations' {filters} -> filters) (\s@ListOperations' {} a -> s {filters = a} :: ListOperations) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOperationsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOperationsResponse_operations Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOperations_nextToken
          Lens..~ rs
          Lens.^? listOperationsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListOperations where
  type
    AWSResponse ListOperations =
      ListOperationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOperations

instance Core.NFData ListOperations

instance Core.ToHeaders ListOperations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.ListOperations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOperations where
  toJSON ListOperations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListOperations where
  toPath = Core.const "/"

instance Core.ToQuery ListOperations where
  toQuery = Core.const Core.mempty

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
    nextToken :: Core.Maybe Core.Text,
    -- | Summary information about the operations that match the specified
    -- criteria.
    operations :: Core.Maybe [OperationSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListOperationsResponse
newListOperationsResponse pHttpStatus_ =
  ListOperationsResponse'
    { nextToken = Core.Nothing,
      operations = Core.Nothing,
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
listOperationsResponse_nextToken :: Lens.Lens' ListOperationsResponse (Core.Maybe Core.Text)
listOperationsResponse_nextToken = Lens.lens (\ListOperationsResponse' {nextToken} -> nextToken) (\s@ListOperationsResponse' {} a -> s {nextToken = a} :: ListOperationsResponse)

-- | Summary information about the operations that match the specified
-- criteria.
listOperationsResponse_operations :: Lens.Lens' ListOperationsResponse (Core.Maybe [OperationSummary])
listOperationsResponse_operations = Lens.lens (\ListOperationsResponse' {operations} -> operations) (\s@ListOperationsResponse' {} a -> s {operations = a} :: ListOperationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOperationsResponse_httpStatus :: Lens.Lens' ListOperationsResponse Core.Int
listOperationsResponse_httpStatus = Lens.lens (\ListOperationsResponse' {httpStatus} -> httpStatus) (\s@ListOperationsResponse' {} a -> s {httpStatus = a} :: ListOperationsResponse)

instance Core.NFData ListOperationsResponse
