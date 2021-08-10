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
import qualified Network.AWS.Prelude as Prelude
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
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getRelationalDatabaseSnapshots_pageToken :: Lens.Lens' GetRelationalDatabaseSnapshots (Prelude.Maybe Prelude.Text)
getRelationalDatabaseSnapshots_pageToken = Lens.lens (\GetRelationalDatabaseSnapshots' {pageToken} -> pageToken) (\s@GetRelationalDatabaseSnapshots' {} a -> s {pageToken = a} :: GetRelationalDatabaseSnapshots)

instance Core.AWSPager GetRelationalDatabaseSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseSnapshotsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getRelationalDatabaseSnapshots_pageToken
          Lens..~ rs
          Lens.^? getRelationalDatabaseSnapshotsResponse_nextPageToken
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..?> "relationalDatabaseSnapshots"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseSnapshots

instance
  Prelude.NFData
    GetRelationalDatabaseSnapshots

instance
  Core.ToHeaders
    GetRelationalDatabaseSnapshots
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRelationalDatabaseSnapshots where
  toJSON GetRelationalDatabaseSnapshots' {..} =
    Core.object
      ( Prelude.catMaybes
          [("pageToken" Core..=) Prelude.<$> pageToken]
      )

instance Core.ToPath GetRelationalDatabaseSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRelationalDatabaseSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseSnapshotsResponse' smart constructor.
data GetRelationalDatabaseSnapshotsResponse = GetRelationalDatabaseSnapshotsResponse'
  { -- | An object describing the result of your get relational database
    -- snapshots request.
    relationalDatabaseSnapshots :: Prelude.Maybe [RelationalDatabaseSnapshot],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabaseSnapshots@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetRelationalDatabaseSnapshotsResponse
newGetRelationalDatabaseSnapshotsResponse
  pHttpStatus_ =
    GetRelationalDatabaseSnapshotsResponse'
      { relationalDatabaseSnapshots =
          Prelude.Nothing,
        nextPageToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object describing the result of your get relational database
-- snapshots request.
getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Prelude.Maybe [RelationalDatabaseSnapshot])
getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots = Lens.lens (\GetRelationalDatabaseSnapshotsResponse' {relationalDatabaseSnapshots} -> relationalDatabaseSnapshots) (\s@GetRelationalDatabaseSnapshotsResponse' {} a -> s {relationalDatabaseSnapshots = a} :: GetRelationalDatabaseSnapshotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseSnapshots@ request and specify the next page token
-- using the @pageToken@ parameter.
getRelationalDatabaseSnapshotsResponse_nextPageToken :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabaseSnapshotsResponse_nextPageToken = Lens.lens (\GetRelationalDatabaseSnapshotsResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabaseSnapshotsResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabaseSnapshotsResponse)

-- | The response's http status code.
getRelationalDatabaseSnapshotsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse Prelude.Int
getRelationalDatabaseSnapshotsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseSnapshotsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseSnapshotsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseSnapshotsResponse
