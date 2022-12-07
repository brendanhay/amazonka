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
-- Module      : Amazonka.Lightsail.GetRelationalDatabaseSnapshots
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your database snapshots in Amazon
-- Lightsail.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetRelationalDatabaseSnapshots
  ( -- * Creating a Request
    GetRelationalDatabaseSnapshots (..),
    newGetRelationalDatabaseSnapshots,

    -- * Request Lenses
    getRelationalDatabaseSnapshots_pageToken,

    -- * Destructuring the Response
    GetRelationalDatabaseSnapshotsResponse (..),
    newGetRelationalDatabaseSnapshotsResponse,

    -- * Response Lenses
    getRelationalDatabaseSnapshotsResponse_nextPageToken,
    getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots,
    getRelationalDatabaseSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseSnapshotsResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> ( x Data..?> "relationalDatabaseSnapshots"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseSnapshots
  where
  hashWithSalt
    _salt
    GetRelationalDatabaseSnapshots' {..} =
      _salt `Prelude.hashWithSalt` pageToken

instance
  Prelude.NFData
    GetRelationalDatabaseSnapshots
  where
  rnf GetRelationalDatabaseSnapshots' {..} =
    Prelude.rnf pageToken

instance
  Data.ToHeaders
    GetRelationalDatabaseSnapshots
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetRelationalDatabaseSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRelationalDatabaseSnapshots where
  toJSON GetRelationalDatabaseSnapshots' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetRelationalDatabaseSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRelationalDatabaseSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseSnapshotsResponse' smart constructor.
data GetRelationalDatabaseSnapshotsResponse = GetRelationalDatabaseSnapshotsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabaseSnapshots@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An object describing the result of your get relational database
    -- snapshots request.
    relationalDatabaseSnapshots :: Prelude.Maybe [RelationalDatabaseSnapshot],
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
-- 'nextPageToken', 'getRelationalDatabaseSnapshotsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseSnapshots@ request and specify the next page token
-- using the @pageToken@ parameter.
--
-- 'relationalDatabaseSnapshots', 'getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots' - An object describing the result of your get relational database
-- snapshots request.
--
-- 'httpStatus', 'getRelationalDatabaseSnapshotsResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseSnapshotsResponse
newGetRelationalDatabaseSnapshotsResponse
  pHttpStatus_ =
    GetRelationalDatabaseSnapshotsResponse'
      { nextPageToken =
          Prelude.Nothing,
        relationalDatabaseSnapshots =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

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

-- | An object describing the result of your get relational database
-- snapshots request.
getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Prelude.Maybe [RelationalDatabaseSnapshot])
getRelationalDatabaseSnapshotsResponse_relationalDatabaseSnapshots = Lens.lens (\GetRelationalDatabaseSnapshotsResponse' {relationalDatabaseSnapshots} -> relationalDatabaseSnapshots) (\s@GetRelationalDatabaseSnapshotsResponse' {} a -> s {relationalDatabaseSnapshots = a} :: GetRelationalDatabaseSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRelationalDatabaseSnapshotsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse Prelude.Int
getRelationalDatabaseSnapshotsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseSnapshotsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseSnapshotsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseSnapshotsResponse
  where
  rnf GetRelationalDatabaseSnapshotsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf relationalDatabaseSnapshots
      `Prelude.seq` Prelude.rnf httpStatus
