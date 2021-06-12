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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your database snapshots in Amazon
-- Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
  ( -- * Creating a Request
    GetRelationalDatabaseSnapshots (..),
    newGetRelationalDatabaseSnapshots,

    -- * Request Lenses
    getRelationalDatabaseSnapshots_pageToken,

    -- * Destructuring the Response
    GetRelationalDatabaseSnapshotsResponse (..),
    newGetRelationalDatabaseSnapshotsResponse,

    -- * Response Lenses
    getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots,
    getRelationalDatabaseSnapshotsResponse_nextPageToken,
    getRelationalDatabaseSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseSnapshots' smart constructor.
data GetRelationalDatabaseSnapshots = GetRelationalDatabaseSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getRelationalDatabaseSnapshots_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
newGetRelationalDatabaseSnapshots ::
  GetRelationalDatabaseSnapshots
newGetRelationalDatabaseSnapshots =
  GetRelationalDatabaseSnapshots'
    { pageToken =
        Core.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getRelationalDatabaseSnapshots_pageToken :: Lens.Lens' GetRelationalDatabaseSnapshots (Core.Maybe Core.Text)
getRelationalDatabaseSnapshots_pageToken = Lens.lens (\GetRelationalDatabaseSnapshots' {pageToken} -> pageToken) (\s@GetRelationalDatabaseSnapshots' {} a -> s {pageToken = a} :: GetRelationalDatabaseSnapshots)

instance Core.AWSPager GetRelationalDatabaseSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseSnapshotsResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getRelationalDatabaseSnapshots_pageToken
          Lens..~ rs
          Lens.^? getRelationalDatabaseSnapshotsResponse_nextPageToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    GetRelationalDatabaseSnapshots
  where
  type
    AWSResponse GetRelationalDatabaseSnapshots =
      GetRelationalDatabaseSnapshotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseSnapshotsResponse'
            Core.<$> ( x Core..?> "relationalDatabaseSnapshots"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRelationalDatabaseSnapshots

instance Core.NFData GetRelationalDatabaseSnapshots

instance
  Core.ToHeaders
    GetRelationalDatabaseSnapshots
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseSnapshots" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRelationalDatabaseSnapshots where
  toJSON GetRelationalDatabaseSnapshots' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetRelationalDatabaseSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery GetRelationalDatabaseSnapshots where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRelationalDatabaseSnapshotsResponse' smart constructor.
data GetRelationalDatabaseSnapshotsResponse = GetRelationalDatabaseSnapshotsResponse'
  { -- | An object describing the result of your get relational database
    -- snapshots request.
    relationalDatabaseSnapshots :: Core.Maybe [RelationalDatabaseSnapshot],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabaseSnapshots@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseSnapshots', 'getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots' - An object describing the result of your get relational database
-- snapshots request.
--
-- 'nextPageToken', 'getRelationalDatabaseSnapshotsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseSnapshots@ request and specify the next page token
-- using the @pageToken@ parameter.
--
-- 'httpStatus', 'getRelationalDatabaseSnapshotsResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseSnapshotsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRelationalDatabaseSnapshotsResponse
newGetRelationalDatabaseSnapshotsResponse
  pHttpStatus_ =
    GetRelationalDatabaseSnapshotsResponse'
      { relationalDatabaseSnapshots =
          Core.Nothing,
        nextPageToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object describing the result of your get relational database
-- snapshots request.
getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Core.Maybe [RelationalDatabaseSnapshot])
getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots = Lens.lens (\GetRelationalDatabaseSnapshotsResponse' {relationalDatabaseSnapshots} -> relationalDatabaseSnapshots) (\s@GetRelationalDatabaseSnapshotsResponse' {} a -> s {relationalDatabaseSnapshots = a} :: GetRelationalDatabaseSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseSnapshots@ request and specify the next page token
-- using the @pageToken@ parameter.
getRelationalDatabaseSnapshotsResponse_nextPageToken :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Core.Maybe Core.Text)
getRelationalDatabaseSnapshotsResponse_nextPageToken = Lens.lens (\GetRelationalDatabaseSnapshotsResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabaseSnapshotsResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabaseSnapshotsResponse)

-- | The response's http status code.
getRelationalDatabaseSnapshotsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse Core.Int
getRelationalDatabaseSnapshotsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseSnapshotsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseSnapshotsResponse)

instance
  Core.NFData
    GetRelationalDatabaseSnapshotsResponse
