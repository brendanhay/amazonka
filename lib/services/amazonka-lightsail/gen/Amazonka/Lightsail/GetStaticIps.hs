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
-- Module      : Amazonka.Lightsail.GetStaticIps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all static IPs in the user\'s account.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetStaticIps
  ( -- * Creating a Request
    GetStaticIps (..),
    newGetStaticIps,

    -- * Request Lenses
    getStaticIps_pageToken,

    -- * Destructuring the Response
    GetStaticIpsResponse (..),
    newGetStaticIpsResponse,

    -- * Response Lenses
    getStaticIpsResponse_nextPageToken,
    getStaticIpsResponse_staticIps,
    getStaticIpsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStaticIps' smart constructor.
data GetStaticIps = GetStaticIps'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetStaticIps@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStaticIps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getStaticIps_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetStaticIps@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetStaticIps ::
  GetStaticIps
newGetStaticIps =
  GetStaticIps' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetStaticIps@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getStaticIps_pageToken :: Lens.Lens' GetStaticIps (Prelude.Maybe Prelude.Text)
getStaticIps_pageToken = Lens.lens (\GetStaticIps' {pageToken} -> pageToken) (\s@GetStaticIps' {} a -> s {pageToken = a} :: GetStaticIps)

instance Core.AWSPager GetStaticIps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getStaticIpsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getStaticIpsResponse_staticIps Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getStaticIps_pageToken
          Lens..~ rs
          Lens.^? getStaticIpsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetStaticIps where
  type AWSResponse GetStaticIps = GetStaticIpsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStaticIpsResponse'
            Prelude.<$> (x Data..?> "nextPageToken")
            Prelude.<*> (x Data..?> "staticIps" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStaticIps where
  hashWithSalt _salt GetStaticIps' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetStaticIps where
  rnf GetStaticIps' {..} = Prelude.rnf pageToken

instance Data.ToHeaders GetStaticIps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetStaticIps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetStaticIps where
  toJSON GetStaticIps' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetStaticIps where
  toPath = Prelude.const "/"

instance Data.ToQuery GetStaticIps where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStaticIpsResponse' smart constructor.
data GetStaticIpsResponse = GetStaticIpsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetStaticIps@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs containing information about your get static
    -- IPs request.
    staticIps :: Prelude.Maybe [StaticIp],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStaticIpsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getStaticIpsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetStaticIps@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'staticIps', 'getStaticIpsResponse_staticIps' - An array of key-value pairs containing information about your get static
-- IPs request.
--
-- 'httpStatus', 'getStaticIpsResponse_httpStatus' - The response's http status code.
newGetStaticIpsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStaticIpsResponse
newGetStaticIpsResponse pHttpStatus_ =
  GetStaticIpsResponse'
    { nextPageToken =
        Prelude.Nothing,
      staticIps = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetStaticIps@ request
-- and specify the next page token using the @pageToken@ parameter.
getStaticIpsResponse_nextPageToken :: Lens.Lens' GetStaticIpsResponse (Prelude.Maybe Prelude.Text)
getStaticIpsResponse_nextPageToken = Lens.lens (\GetStaticIpsResponse' {nextPageToken} -> nextPageToken) (\s@GetStaticIpsResponse' {} a -> s {nextPageToken = a} :: GetStaticIpsResponse)

-- | An array of key-value pairs containing information about your get static
-- IPs request.
getStaticIpsResponse_staticIps :: Lens.Lens' GetStaticIpsResponse (Prelude.Maybe [StaticIp])
getStaticIpsResponse_staticIps = Lens.lens (\GetStaticIpsResponse' {staticIps} -> staticIps) (\s@GetStaticIpsResponse' {} a -> s {staticIps = a} :: GetStaticIpsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getStaticIpsResponse_httpStatus :: Lens.Lens' GetStaticIpsResponse Prelude.Int
getStaticIpsResponse_httpStatus = Lens.lens (\GetStaticIpsResponse' {httpStatus} -> httpStatus) (\s@GetStaticIpsResponse' {} a -> s {httpStatus = a} :: GetStaticIpsResponse)

instance Prelude.NFData GetStaticIpsResponse where
  rnf GetStaticIpsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf staticIps
      `Prelude.seq` Prelude.rnf httpStatus
