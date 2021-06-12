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
-- Module      : Network.AWS.Route53Domains.ListOperations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of the operations that return an operation
-- ID and that have ever been performed on domains that were registered by
-- the current account.
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListOperations
  ( -- * Creating a Request
    ListOperations (..),
    newListOperations,

    -- * Request Lenses
    listOperations_submittedSince,
    listOperations_maxItems,
    listOperations_marker,

    -- * Destructuring the Response
    ListOperationsResponse (..),
    newListOperationsResponse,

    -- * Response Lenses
    listOperationsResponse_nextPageMarker,
    listOperationsResponse_httpStatus,
    listOperationsResponse_operations,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The ListOperations request includes the following elements.
--
-- /See:/ 'newListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | An optional parameter that lets you get information about all the
    -- operations that you submitted after a specified date and time. Specify
    -- the date and time in Unix time format and Coordinated Universal time
    -- (UTC).
    submittedSince :: Core.Maybe Core.POSIX,
    -- | Number of domains to be returned.
    --
    -- Default: 20
    maxItems :: Core.Maybe Core.Int,
    -- | For an initial request for a list of operations, omit this element. If
    -- the number of operations that are not yet complete is greater than the
    -- value that you specified for @MaxItems@, you can use @Marker@ to return
    -- additional operations. Get the value of @NextPageMarker@ from the
    -- previous response, and submit another request that includes the value of
    -- @NextPageMarker@ in the @Marker@ element.
    marker :: Core.Maybe Core.Text
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
-- 'submittedSince', 'listOperations_submittedSince' - An optional parameter that lets you get information about all the
-- operations that you submitted after a specified date and time. Specify
-- the date and time in Unix time format and Coordinated Universal time
-- (UTC).
--
-- 'maxItems', 'listOperations_maxItems' - Number of domains to be returned.
--
-- Default: 20
--
-- 'marker', 'listOperations_marker' - For an initial request for a list of operations, omit this element. If
-- the number of operations that are not yet complete is greater than the
-- value that you specified for @MaxItems@, you can use @Marker@ to return
-- additional operations. Get the value of @NextPageMarker@ from the
-- previous response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
newListOperations ::
  ListOperations
newListOperations =
  ListOperations'
    { submittedSince = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | An optional parameter that lets you get information about all the
-- operations that you submitted after a specified date and time. Specify
-- the date and time in Unix time format and Coordinated Universal time
-- (UTC).
listOperations_submittedSince :: Lens.Lens' ListOperations (Core.Maybe Core.UTCTime)
listOperations_submittedSince = Lens.lens (\ListOperations' {submittedSince} -> submittedSince) (\s@ListOperations' {} a -> s {submittedSince = a} :: ListOperations) Core.. Lens.mapping Core._Time

-- | Number of domains to be returned.
--
-- Default: 20
listOperations_maxItems :: Lens.Lens' ListOperations (Core.Maybe Core.Int)
listOperations_maxItems = Lens.lens (\ListOperations' {maxItems} -> maxItems) (\s@ListOperations' {} a -> s {maxItems = a} :: ListOperations)

-- | For an initial request for a list of operations, omit this element. If
-- the number of operations that are not yet complete is greater than the
-- value that you specified for @MaxItems@, you can use @Marker@ to return
-- additional operations. Get the value of @NextPageMarker@ from the
-- previous response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
listOperations_marker :: Lens.Lens' ListOperations (Core.Maybe Core.Text)
listOperations_marker = Lens.lens (\ListOperations' {marker} -> marker) (\s@ListOperations' {} a -> s {marker = a} :: ListOperations)

instance Core.AWSPager ListOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOperationsResponse_nextPageMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listOperationsResponse_operations) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOperations_marker
          Lens..~ rs
          Lens.^? listOperationsResponse_nextPageMarker
            Core.. Lens._Just

instance Core.AWSRequest ListOperations where
  type
    AWSResponse ListOperations =
      ListOperationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Core.<$> (x Core..?> "NextPageMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Operations" Core..!@ Core.mempty)
      )

instance Core.Hashable ListOperations

instance Core.NFData ListOperations

instance Core.ToHeaders ListOperations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.ListOperations" ::
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
          [ ("SubmittedSince" Core..=) Core.<$> submittedSince,
            ("MaxItems" Core..=) Core.<$> maxItems,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.ToPath ListOperations where
  toPath = Core.const "/"

instance Core.ToQuery ListOperations where
  toQuery = Core.const Core.mempty

-- | The ListOperations response includes the following elements.
--
-- /See:/ 'newListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | If there are more operations than you specified for @MaxItems@ in the
    -- request, submit another request and include the value of
    -- @NextPageMarker@ in the value of @Marker@.
    nextPageMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Lists summaries of the operations.
    operations :: [OperationSummary]
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
-- 'nextPageMarker', 'listOperationsResponse_nextPageMarker' - If there are more operations than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- 'httpStatus', 'listOperationsResponse_httpStatus' - The response's http status code.
--
-- 'operations', 'listOperationsResponse_operations' - Lists summaries of the operations.
newListOperationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOperationsResponse
newListOperationsResponse pHttpStatus_ =
  ListOperationsResponse'
    { nextPageMarker =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      operations = Core.mempty
    }

-- | If there are more operations than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
listOperationsResponse_nextPageMarker :: Lens.Lens' ListOperationsResponse (Core.Maybe Core.Text)
listOperationsResponse_nextPageMarker = Lens.lens (\ListOperationsResponse' {nextPageMarker} -> nextPageMarker) (\s@ListOperationsResponse' {} a -> s {nextPageMarker = a} :: ListOperationsResponse)

-- | The response's http status code.
listOperationsResponse_httpStatus :: Lens.Lens' ListOperationsResponse Core.Int
listOperationsResponse_httpStatus = Lens.lens (\ListOperationsResponse' {httpStatus} -> httpStatus) (\s@ListOperationsResponse' {} a -> s {httpStatus = a} :: ListOperationsResponse)

-- | Lists summaries of the operations.
listOperationsResponse_operations :: Lens.Lens' ListOperationsResponse [OperationSummary]
listOperationsResponse_operations = Lens.lens (\ListOperationsResponse' {operations} -> operations) (\s@ListOperationsResponse' {} a -> s {operations = a} :: ListOperationsResponse) Core.. Lens._Coerce

instance Core.NFData ListOperationsResponse
