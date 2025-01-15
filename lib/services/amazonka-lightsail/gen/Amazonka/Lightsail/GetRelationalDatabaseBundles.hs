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
-- Module      : Amazonka.Lightsail.GetRelationalDatabaseBundles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available in Amazon Lightsail. A
-- bundle describes the performance specifications for a database.
--
-- You can use a bundle ID to create a new database with explicit
-- performance specifications.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetRelationalDatabaseBundles
  ( -- * Creating a Request
    GetRelationalDatabaseBundles (..),
    newGetRelationalDatabaseBundles,

    -- * Request Lenses
    getRelationalDatabaseBundles_includeInactive,
    getRelationalDatabaseBundles_pageToken,

    -- * Destructuring the Response
    GetRelationalDatabaseBundlesResponse (..),
    newGetRelationalDatabaseBundlesResponse,

    -- * Response Lenses
    getRelationalDatabaseBundlesResponse_bundles,
    getRelationalDatabaseBundlesResponse_nextPageToken,
    getRelationalDatabaseBundlesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRelationalDatabaseBundles' smart constructor.
data GetRelationalDatabaseBundles = GetRelationalDatabaseBundles'
  { -- | A Boolean value that indicates whether to include inactive (unavailable)
    -- bundles in the response of your request.
    includeInactive :: Prelude.Maybe Prelude.Bool,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseBundles@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseBundles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeInactive', 'getRelationalDatabaseBundles_includeInactive' - A Boolean value that indicates whether to include inactive (unavailable)
-- bundles in the response of your request.
--
-- 'pageToken', 'getRelationalDatabaseBundles_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBundles@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
newGetRelationalDatabaseBundles ::
  GetRelationalDatabaseBundles
newGetRelationalDatabaseBundles =
  GetRelationalDatabaseBundles'
    { includeInactive =
        Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | A Boolean value that indicates whether to include inactive (unavailable)
-- bundles in the response of your request.
getRelationalDatabaseBundles_includeInactive :: Lens.Lens' GetRelationalDatabaseBundles (Prelude.Maybe Prelude.Bool)
getRelationalDatabaseBundles_includeInactive = Lens.lens (\GetRelationalDatabaseBundles' {includeInactive} -> includeInactive) (\s@GetRelationalDatabaseBundles' {} a -> s {includeInactive = a} :: GetRelationalDatabaseBundles)

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBundles@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getRelationalDatabaseBundles_pageToken :: Lens.Lens' GetRelationalDatabaseBundles (Prelude.Maybe Prelude.Text)
getRelationalDatabaseBundles_pageToken = Lens.lens (\GetRelationalDatabaseBundles' {pageToken} -> pageToken) (\s@GetRelationalDatabaseBundles' {} a -> s {pageToken = a} :: GetRelationalDatabaseBundles)

instance Core.AWSPager GetRelationalDatabaseBundles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseBundlesResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseBundlesResponse_bundles
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getRelationalDatabaseBundles_pageToken
              Lens..~ rs
              Lens.^? getRelationalDatabaseBundlesResponse_nextPageToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetRelationalDatabaseBundles where
  type
    AWSResponse GetRelationalDatabaseBundles =
      GetRelationalDatabaseBundlesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseBundlesResponse'
            Prelude.<$> (x Data..?> "bundles" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseBundles
  where
  hashWithSalt _salt GetRelationalDatabaseBundles' {..} =
    _salt
      `Prelude.hashWithSalt` includeInactive
      `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetRelationalDatabaseBundles where
  rnf GetRelationalDatabaseBundles' {..} =
    Prelude.rnf includeInactive `Prelude.seq`
      Prelude.rnf pageToken

instance Data.ToHeaders GetRelationalDatabaseBundles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetRelationalDatabaseBundles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRelationalDatabaseBundles where
  toJSON GetRelationalDatabaseBundles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("includeInactive" Data..=)
              Prelude.<$> includeInactive,
            ("pageToken" Data..=) Prelude.<$> pageToken
          ]
      )

instance Data.ToPath GetRelationalDatabaseBundles where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRelationalDatabaseBundles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseBundlesResponse' smart constructor.
data GetRelationalDatabaseBundlesResponse = GetRelationalDatabaseBundlesResponse'
  { -- | An object describing the result of your get relational database bundles
    -- request.
    bundles :: Prelude.Maybe [RelationalDatabaseBundle],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabaseBundles@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseBundlesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundles', 'getRelationalDatabaseBundlesResponse_bundles' - An object describing the result of your get relational database bundles
-- request.
--
-- 'nextPageToken', 'getRelationalDatabaseBundlesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseBundles@ request and specify the next page token
-- using the @pageToken@ parameter.
--
-- 'httpStatus', 'getRelationalDatabaseBundlesResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseBundlesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseBundlesResponse
newGetRelationalDatabaseBundlesResponse pHttpStatus_ =
  GetRelationalDatabaseBundlesResponse'
    { bundles =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object describing the result of your get relational database bundles
-- request.
getRelationalDatabaseBundlesResponse_bundles :: Lens.Lens' GetRelationalDatabaseBundlesResponse (Prelude.Maybe [RelationalDatabaseBundle])
getRelationalDatabaseBundlesResponse_bundles = Lens.lens (\GetRelationalDatabaseBundlesResponse' {bundles} -> bundles) (\s@GetRelationalDatabaseBundlesResponse' {} a -> s {bundles = a} :: GetRelationalDatabaseBundlesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseBundles@ request and specify the next page token
-- using the @pageToken@ parameter.
getRelationalDatabaseBundlesResponse_nextPageToken :: Lens.Lens' GetRelationalDatabaseBundlesResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabaseBundlesResponse_nextPageToken = Lens.lens (\GetRelationalDatabaseBundlesResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabaseBundlesResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabaseBundlesResponse)

-- | The response's http status code.
getRelationalDatabaseBundlesResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseBundlesResponse Prelude.Int
getRelationalDatabaseBundlesResponse_httpStatus = Lens.lens (\GetRelationalDatabaseBundlesResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseBundlesResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseBundlesResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseBundlesResponse
  where
  rnf GetRelationalDatabaseBundlesResponse' {..} =
    Prelude.rnf bundles `Prelude.seq`
      Prelude.rnf nextPageToken `Prelude.seq`
        Prelude.rnf httpStatus
