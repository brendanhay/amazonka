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
-- Module      : Amazonka.Lightsail.GetOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Lightsail.GetOperations
  ( -- * Creating a Request
    GetOperations (..),
    newGetOperations,

    -- * Request Lenses
    getOperations_pageToken,

    -- * Destructuring the Response
    GetOperationsResponse (..),
    newGetOperationsResponse,

    -- * Response Lenses
    getOperationsResponse_nextPageToken,
    getOperationsResponse_operations,
    getOperationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOperations' smart constructor.
data GetOperations = GetOperations'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetOperations@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  GetOperations' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperations@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getOperations_pageToken :: Lens.Lens' GetOperations (Prelude.Maybe Prelude.Text)
getOperations_pageToken = Lens.lens (\GetOperations' {pageToken} -> pageToken) (\s@GetOperations' {} a -> s {pageToken = a} :: GetOperations)

instance Core.AWSPager GetOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getOperationsResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getOperationsResponse_operations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getOperations_pageToken
          Lens..~ rs
          Lens.^? getOperationsResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetOperations where
  type
    AWSResponse GetOperations =
      GetOperationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationsResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOperations where
  hashWithSalt _salt GetOperations' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetOperations where
  rnf GetOperations' {..} = Prelude.rnf pageToken

instance Data.ToHeaders GetOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetOperations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetOperations where
  toJSON GetOperations' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetOperations where
  toPath = Prelude.const "/"

instance Data.ToQuery GetOperations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOperationsResponse' smart constructor.
data GetOperationsResponse = GetOperationsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetOperations@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getOperationsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetOperations@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'operations', 'getOperationsResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'getOperationsResponse_httpStatus' - The response's http status code.
newGetOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOperationsResponse
newGetOperationsResponse pHttpStatus_ =
  GetOperationsResponse'
    { nextPageToken =
        Prelude.Nothing,
      operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetOperations@ request
-- and specify the next page token using the @pageToken@ parameter.
getOperationsResponse_nextPageToken :: Lens.Lens' GetOperationsResponse (Prelude.Maybe Prelude.Text)
getOperationsResponse_nextPageToken = Lens.lens (\GetOperationsResponse' {nextPageToken} -> nextPageToken) (\s@GetOperationsResponse' {} a -> s {nextPageToken = a} :: GetOperationsResponse)

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
getOperationsResponse_operations :: Lens.Lens' GetOperationsResponse (Prelude.Maybe [Operation])
getOperationsResponse_operations = Lens.lens (\GetOperationsResponse' {operations} -> operations) (\s@GetOperationsResponse' {} a -> s {operations = a} :: GetOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getOperationsResponse_httpStatus :: Lens.Lens' GetOperationsResponse Prelude.Int
getOperationsResponse_httpStatus = Lens.lens (\GetOperationsResponse' {httpStatus} -> httpStatus) (\s@GetOperationsResponse' {} a -> s {httpStatus = a} :: GetOperationsResponse)

instance Prelude.NFData GetOperationsResponse where
  rnf GetOperationsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
