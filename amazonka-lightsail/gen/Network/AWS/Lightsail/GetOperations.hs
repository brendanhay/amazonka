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
-- Module      : Network.AWS.Lightsail.GetOperations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all operations.
--
-- Results are returned from oldest to newest, up to a maximum of 200.
-- Results can be paged by making each subsequent call to @GetOperations@
-- use the maximum (last) @statusChangedAt@ value from the previous
-- request.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetOperations
  ( -- * Creating a Request
    GetOperations (..),
    newGetOperations,

    -- * Request Lenses
    getOperations_pageToken,

    -- * Destructuring the Response
    GetOperationsResponse (..),
    newGetOperationsResponse,

    -- * Response Lenses
    getOperationsResponse_operations,
    getOperationsResponse_nextPageToken,
    getOperationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOperations' smart constructor.
data GetOperations = GetOperations'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetOperations@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getOperations_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperations@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetOperations ::
  GetOperations
newGetOperations =
  GetOperations' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperations@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getOperations_pageToken :: Lens.Lens' GetOperations (Core.Maybe Core.Text)
getOperations_pageToken = Lens.lens (\GetOperations' {pageToken} -> pageToken) (\s@GetOperations' {} a -> s {pageToken = a} :: GetOperations)

instance Core.AWSPager GetOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getOperationsResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getOperationsResponse_operations Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getOperations_pageToken
          Lens..~ rs
          Lens.^? getOperationsResponse_nextPageToken Core.. Lens._Just

instance Core.AWSRequest GetOperations where
  type
    AWSResponse GetOperations =
      GetOperationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationsResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetOperations

instance Core.NFData GetOperations

instance Core.ToHeaders GetOperations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetOperations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetOperations where
  toJSON GetOperations' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetOperations where
  toPath = Core.const "/"

instance Core.ToQuery GetOperations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetOperationsResponse' smart constructor.
data GetOperationsResponse = GetOperationsResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetOperations@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'getOperationsResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'nextPageToken', 'getOperationsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetOperations@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getOperationsResponse_httpStatus' - The response's http status code.
newGetOperationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetOperationsResponse
newGetOperationsResponse pHttpStatus_ =
  GetOperationsResponse'
    { operations = Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
getOperationsResponse_operations :: Lens.Lens' GetOperationsResponse (Core.Maybe [Operation])
getOperationsResponse_operations = Lens.lens (\GetOperationsResponse' {operations} -> operations) (\s@GetOperationsResponse' {} a -> s {operations = a} :: GetOperationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetOperations@ request
-- and specify the next page token using the @pageToken@ parameter.
getOperationsResponse_nextPageToken :: Lens.Lens' GetOperationsResponse (Core.Maybe Core.Text)
getOperationsResponse_nextPageToken = Lens.lens (\GetOperationsResponse' {nextPageToken} -> nextPageToken) (\s@GetOperationsResponse' {} a -> s {nextPageToken = a} :: GetOperationsResponse)

-- | The response's http status code.
getOperationsResponse_httpStatus :: Lens.Lens' GetOperationsResponse Core.Int
getOperationsResponse_httpStatus = Lens.lens (\GetOperationsResponse' {httpStatus} -> httpStatus) (\s@GetOperationsResponse' {} a -> s {httpStatus = a} :: GetOperationsResponse)

instance Core.NFData GetOperationsResponse
