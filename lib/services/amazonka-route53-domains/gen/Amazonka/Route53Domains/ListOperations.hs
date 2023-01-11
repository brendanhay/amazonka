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
-- Module      : Amazonka.Route53Domains.ListOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of the operations that return an operation
-- ID and that have ever been performed on domains that were registered by
-- the current account.
--
-- This command runs only in the us-east-1 Region.
--
-- This operation returns paginated results.
module Amazonka.Route53Domains.ListOperations
  ( -- * Creating a Request
    ListOperations (..),
    newListOperations,

    -- * Request Lenses
    listOperations_marker,
    listOperations_maxItems,
    listOperations_sortBy,
    listOperations_sortOrder,
    listOperations_status,
    listOperations_submittedSince,
    listOperations_type,

    -- * Destructuring the Response
    ListOperationsResponse (..),
    newListOperationsResponse,

    -- * Response Lenses
    listOperationsResponse_nextPageMarker,
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
import Amazonka.Route53Domains.Types

-- | The ListOperations request includes the following elements.
--
-- /See:/ 'newListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | For an initial request for a list of operations, omit this element. If
    -- the number of operations that are not yet complete is greater than the
    -- value that you specified for @MaxItems@, you can use @Marker@ to return
    -- additional operations. Get the value of @NextPageMarker@ from the
    -- previous response, and submit another request that includes the value of
    -- @NextPageMarker@ in the @Marker@ element.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Number of domains to be returned.
    --
    -- Default: 20
    maxItems :: Prelude.Maybe Prelude.Int,
    -- | The sort type for returned values.
    sortBy :: Prelude.Maybe ListOperationsSortAttributeName,
    -- | The sort order ofr returned values, either ascending or descending.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The status of the operations.
    status :: Prelude.Maybe [OperationStatus],
    -- | An optional parameter that lets you get information about all the
    -- operations that you submitted after a specified date and time. Specify
    -- the date and time in Unix time format and Coordinated Universal time
    -- (UTC).
    submittedSince :: Prelude.Maybe Data.POSIX,
    -- | An arrays of the domains operation types.
    type' :: Prelude.Maybe [OperationType]
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
-- 'marker', 'listOperations_marker' - For an initial request for a list of operations, omit this element. If
-- the number of operations that are not yet complete is greater than the
-- value that you specified for @MaxItems@, you can use @Marker@ to return
-- additional operations. Get the value of @NextPageMarker@ from the
-- previous response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
--
-- 'maxItems', 'listOperations_maxItems' - Number of domains to be returned.
--
-- Default: 20
--
-- 'sortBy', 'listOperations_sortBy' - The sort type for returned values.
--
-- 'sortOrder', 'listOperations_sortOrder' - The sort order ofr returned values, either ascending or descending.
--
-- 'status', 'listOperations_status' - The status of the operations.
--
-- 'submittedSince', 'listOperations_submittedSince' - An optional parameter that lets you get information about all the
-- operations that you submitted after a specified date and time. Specify
-- the date and time in Unix time format and Coordinated Universal time
-- (UTC).
--
-- 'type'', 'listOperations_type' - An arrays of the domains operation types.
newListOperations ::
  ListOperations
newListOperations =
  ListOperations'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      status = Prelude.Nothing,
      submittedSince = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | For an initial request for a list of operations, omit this element. If
-- the number of operations that are not yet complete is greater than the
-- value that you specified for @MaxItems@, you can use @Marker@ to return
-- additional operations. Get the value of @NextPageMarker@ from the
-- previous response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
listOperations_marker :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Text)
listOperations_marker = Lens.lens (\ListOperations' {marker} -> marker) (\s@ListOperations' {} a -> s {marker = a} :: ListOperations)

-- | Number of domains to be returned.
--
-- Default: 20
listOperations_maxItems :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Int)
listOperations_maxItems = Lens.lens (\ListOperations' {maxItems} -> maxItems) (\s@ListOperations' {} a -> s {maxItems = a} :: ListOperations)

-- | The sort type for returned values.
listOperations_sortBy :: Lens.Lens' ListOperations (Prelude.Maybe ListOperationsSortAttributeName)
listOperations_sortBy = Lens.lens (\ListOperations' {sortBy} -> sortBy) (\s@ListOperations' {} a -> s {sortBy = a} :: ListOperations)

-- | The sort order ofr returned values, either ascending or descending.
listOperations_sortOrder :: Lens.Lens' ListOperations (Prelude.Maybe SortOrder)
listOperations_sortOrder = Lens.lens (\ListOperations' {sortOrder} -> sortOrder) (\s@ListOperations' {} a -> s {sortOrder = a} :: ListOperations)

-- | The status of the operations.
listOperations_status :: Lens.Lens' ListOperations (Prelude.Maybe [OperationStatus])
listOperations_status = Lens.lens (\ListOperations' {status} -> status) (\s@ListOperations' {} a -> s {status = a} :: ListOperations) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that lets you get information about all the
-- operations that you submitted after a specified date and time. Specify
-- the date and time in Unix time format and Coordinated Universal time
-- (UTC).
listOperations_submittedSince :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.UTCTime)
listOperations_submittedSince = Lens.lens (\ListOperations' {submittedSince} -> submittedSince) (\s@ListOperations' {} a -> s {submittedSince = a} :: ListOperations) Prelude.. Lens.mapping Data._Time

-- | An arrays of the domains operation types.
listOperations_type :: Lens.Lens' ListOperations (Prelude.Maybe [OperationType])
listOperations_type = Lens.lens (\ListOperations' {type'} -> type') (\s@ListOperations' {} a -> s {type' = a} :: ListOperations) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOperationsResponse_nextPageMarker
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
          Prelude.& listOperations_marker
          Lens..~ rs
          Lens.^? listOperationsResponse_nextPageMarker
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
            Prelude.<$> (x Data..?> "NextPageMarker")
            Prelude.<*> (x Data..?> "Operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOperations where
  hashWithSalt _salt ListOperations' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` submittedSince
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListOperations where
  rnf ListOperations' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf submittedSince
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.ListOperations" ::
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
          [ ("Marker" Data..=) Prelude.<$> marker,
            ("MaxItems" Data..=) Prelude.<$> maxItems,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("Status" Data..=) Prelude.<$> status,
            ("SubmittedSince" Data..=)
              Prelude.<$> submittedSince,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

instance Data.ToPath ListOperations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListOperations where
  toQuery = Prelude.const Prelude.mempty

-- | The ListOperations response includes the following elements.
--
-- /See:/ 'newListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | If there are more operations than you specified for @MaxItems@ in the
    -- request, submit another request and include the value of
    -- @NextPageMarker@ in the value of @Marker@.
    nextPageMarker :: Prelude.Maybe Prelude.Text,
    -- | Lists summaries of the operations.
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
-- 'nextPageMarker', 'listOperationsResponse_nextPageMarker' - If there are more operations than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- 'operations', 'listOperationsResponse_operations' - Lists summaries of the operations.
--
-- 'httpStatus', 'listOperationsResponse_httpStatus' - The response's http status code.
newListOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOperationsResponse
newListOperationsResponse pHttpStatus_ =
  ListOperationsResponse'
    { nextPageMarker =
        Prelude.Nothing,
      operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more operations than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
listOperationsResponse_nextPageMarker :: Lens.Lens' ListOperationsResponse (Prelude.Maybe Prelude.Text)
listOperationsResponse_nextPageMarker = Lens.lens (\ListOperationsResponse' {nextPageMarker} -> nextPageMarker) (\s@ListOperationsResponse' {} a -> s {nextPageMarker = a} :: ListOperationsResponse)

-- | Lists summaries of the operations.
listOperationsResponse_operations :: Lens.Lens' ListOperationsResponse (Prelude.Maybe [OperationSummary])
listOperationsResponse_operations = Lens.lens (\ListOperationsResponse' {operations} -> operations) (\s@ListOperationsResponse' {} a -> s {operations = a} :: ListOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOperationsResponse_httpStatus :: Lens.Lens' ListOperationsResponse Prelude.Int
listOperationsResponse_httpStatus = Lens.lens (\ListOperationsResponse' {httpStatus} -> httpStatus) (\s@ListOperationsResponse' {} a -> s {httpStatus = a} :: ListOperationsResponse)

instance Prelude.NFData ListOperationsResponse where
  rnf ListOperationsResponse' {..} =
    Prelude.rnf nextPageMarker
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
