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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your databases in Amazon Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabases
  ( -- * Creating a Request
    GetRelationalDatabases (..),
    newGetRelationalDatabases,

    -- * Request Lenses
    getRelationalDatabases_pageToken,

    -- * Destructuring the Response
    GetRelationalDatabasesResponse (..),
    newGetRelationalDatabasesResponse,

    -- * Response Lenses
    getRelationalDatabasesResponse_nextPageToken,
    getRelationalDatabasesResponse_relationalDatabases,
    getRelationalDatabasesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabases' smart constructor.
data GetRelationalDatabases = GetRelationalDatabases'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabases@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRelationalDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getRelationalDatabases_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabases@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
newGetRelationalDatabases ::
  GetRelationalDatabases
newGetRelationalDatabases =
  GetRelationalDatabases' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabases@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getRelationalDatabases_pageToken :: Lens.Lens' GetRelationalDatabases (Core.Maybe Core.Text)
getRelationalDatabases_pageToken = Lens.lens (\GetRelationalDatabases' {pageToken} -> pageToken) (\s@GetRelationalDatabases' {} a -> s {pageToken = a} :: GetRelationalDatabases)

instance Core.AWSPager GetRelationalDatabases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabasesResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabasesResponse_relationalDatabases
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getRelationalDatabases_pageToken
          Lens..~ rs
          Lens.^? getRelationalDatabasesResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest GetRelationalDatabases where
  type
    AWSResponse GetRelationalDatabases =
      GetRelationalDatabasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabasesResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> ( x Core..?> "relationalDatabases"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRelationalDatabases

instance Core.NFData GetRelationalDatabases

instance Core.ToHeaders GetRelationalDatabases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabases" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRelationalDatabases where
  toJSON GetRelationalDatabases' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetRelationalDatabases where
  toPath = Core.const "/"

instance Core.ToQuery GetRelationalDatabases where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRelationalDatabasesResponse' smart constructor.
data GetRelationalDatabasesResponse = GetRelationalDatabasesResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabases@ request and specify the next page token using
    -- the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | An object describing the result of your get relational databases
    -- request.
    relationalDatabases :: Core.Maybe [RelationalDatabase],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRelationalDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getRelationalDatabasesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabases@ request and specify the next page token using
-- the @pageToken@ parameter.
--
-- 'relationalDatabases', 'getRelationalDatabasesResponse_relationalDatabases' - An object describing the result of your get relational databases
-- request.
--
-- 'httpStatus', 'getRelationalDatabasesResponse_httpStatus' - The response's http status code.
newGetRelationalDatabasesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRelationalDatabasesResponse
newGetRelationalDatabasesResponse pHttpStatus_ =
  GetRelationalDatabasesResponse'
    { nextPageToken =
        Core.Nothing,
      relationalDatabases = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabases@ request and specify the next page token using
-- the @pageToken@ parameter.
getRelationalDatabasesResponse_nextPageToken :: Lens.Lens' GetRelationalDatabasesResponse (Core.Maybe Core.Text)
getRelationalDatabasesResponse_nextPageToken = Lens.lens (\GetRelationalDatabasesResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabasesResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabasesResponse)

-- | An object describing the result of your get relational databases
-- request.
getRelationalDatabasesResponse_relationalDatabases :: Lens.Lens' GetRelationalDatabasesResponse (Core.Maybe [RelationalDatabase])
getRelationalDatabasesResponse_relationalDatabases = Lens.lens (\GetRelationalDatabasesResponse' {relationalDatabases} -> relationalDatabases) (\s@GetRelationalDatabasesResponse' {} a -> s {relationalDatabases = a} :: GetRelationalDatabasesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getRelationalDatabasesResponse_httpStatus :: Lens.Lens' GetRelationalDatabasesResponse Core.Int
getRelationalDatabasesResponse_httpStatus = Lens.lens (\GetRelationalDatabasesResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabasesResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabasesResponse)

instance Core.NFData GetRelationalDatabasesResponse
