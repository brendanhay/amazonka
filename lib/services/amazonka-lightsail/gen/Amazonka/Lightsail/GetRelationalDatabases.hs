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
-- Module      : Amazonka.Lightsail.GetRelationalDatabases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your databases in Amazon Lightsail.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetRelationalDatabases
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRelationalDatabases' smart constructor.
data GetRelationalDatabases = GetRelationalDatabases'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabases@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  GetRelationalDatabases'
    { pageToken =
        Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabases@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getRelationalDatabases_pageToken :: Lens.Lens' GetRelationalDatabases (Prelude.Maybe Prelude.Text)
getRelationalDatabases_pageToken = Lens.lens (\GetRelationalDatabases' {pageToken} -> pageToken) (\s@GetRelationalDatabases' {} a -> s {pageToken = a} :: GetRelationalDatabases)

instance Core.AWSPager GetRelationalDatabases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabasesResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabasesResponse_relationalDatabases
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getRelationalDatabases_pageToken
          Lens..~ rs
          Lens.^? getRelationalDatabasesResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetRelationalDatabases where
  type
    AWSResponse GetRelationalDatabases =
      GetRelationalDatabasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabasesResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> ( x Data..?> "relationalDatabases"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRelationalDatabases where
  hashWithSalt _salt GetRelationalDatabases' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetRelationalDatabases where
  rnf GetRelationalDatabases' {..} =
    Prelude.rnf pageToken

instance Data.ToHeaders GetRelationalDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetRelationalDatabases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRelationalDatabases where
  toJSON GetRelationalDatabases' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetRelationalDatabases where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRelationalDatabases where
  toQuery = Prelude.const Prelude.mempty

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
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An object describing the result of your get relational databases
    -- request.
    relationalDatabases :: Prelude.Maybe [RelationalDatabase],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetRelationalDatabasesResponse
newGetRelationalDatabasesResponse pHttpStatus_ =
  GetRelationalDatabasesResponse'
    { nextPageToken =
        Prelude.Nothing,
      relationalDatabases = Prelude.Nothing,
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
getRelationalDatabasesResponse_nextPageToken :: Lens.Lens' GetRelationalDatabasesResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabasesResponse_nextPageToken = Lens.lens (\GetRelationalDatabasesResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabasesResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabasesResponse)

-- | An object describing the result of your get relational databases
-- request.
getRelationalDatabasesResponse_relationalDatabases :: Lens.Lens' GetRelationalDatabasesResponse (Prelude.Maybe [RelationalDatabase])
getRelationalDatabasesResponse_relationalDatabases = Lens.lens (\GetRelationalDatabasesResponse' {relationalDatabases} -> relationalDatabases) (\s@GetRelationalDatabasesResponse' {} a -> s {relationalDatabases = a} :: GetRelationalDatabasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRelationalDatabasesResponse_httpStatus :: Lens.Lens' GetRelationalDatabasesResponse Prelude.Int
getRelationalDatabasesResponse_httpStatus = Lens.lens (\GetRelationalDatabasesResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabasesResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabasesResponse)

instance
  Prelude.NFData
    GetRelationalDatabasesResponse
  where
  rnf GetRelationalDatabasesResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf relationalDatabases
      `Prelude.seq` Prelude.rnf httpStatus
