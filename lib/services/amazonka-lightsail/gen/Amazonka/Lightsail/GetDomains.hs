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
-- Module      : Amazonka.Lightsail.GetDomains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all domains in the user\'s account.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetDomains
  ( -- * Creating a Request
    GetDomains (..),
    newGetDomains,

    -- * Request Lenses
    getDomains_pageToken,

    -- * Destructuring the Response
    GetDomainsResponse (..),
    newGetDomainsResponse,

    -- * Response Lenses
    getDomainsResponse_domains,
    getDomainsResponse_nextPageToken,
    getDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDomains' smart constructor.
data GetDomains = GetDomains'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDomains@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getDomains_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDomains@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetDomains ::
  GetDomains
newGetDomains =
  GetDomains' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDomains@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getDomains_pageToken :: Lens.Lens' GetDomains (Prelude.Maybe Prelude.Text)
getDomains_pageToken = Lens.lens (\GetDomains' {pageToken} -> pageToken) (\s@GetDomains' {} a -> s {pageToken = a} :: GetDomains)

instance Core.AWSPager GetDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDomainsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDomainsResponse_domains Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getDomains_pageToken
          Lens..~ rs
          Lens.^? getDomainsResponse_nextPageToken Prelude.. Lens._Just

instance Core.AWSRequest GetDomains where
  type AWSResponse GetDomains = GetDomainsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainsResponse'
            Prelude.<$> (x Data..?> "domains" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomains where
  hashWithSalt _salt GetDomains' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetDomains where
  rnf GetDomains' {..} = Prelude.rnf pageToken

instance Data.ToHeaders GetDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetDomains" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDomains where
  toJSON GetDomains' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetDomains where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDomains where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDomainsResponse' smart constructor.
data GetDomainsResponse = GetDomainsResponse'
  { -- | An array of key-value pairs containing information about each of the
    -- domain entries in the user\'s account.
    domains :: Prelude.Maybe [Domain],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetDomains@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domains', 'getDomainsResponse_domains' - An array of key-value pairs containing information about each of the
-- domain entries in the user\'s account.
--
-- 'nextPageToken', 'getDomainsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDomains@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getDomainsResponse_httpStatus' - The response's http status code.
newGetDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDomainsResponse
newGetDomainsResponse pHttpStatus_ =
  GetDomainsResponse'
    { domains = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about each of the
-- domain entries in the user\'s account.
getDomainsResponse_domains :: Lens.Lens' GetDomainsResponse (Prelude.Maybe [Domain])
getDomainsResponse_domains = Lens.lens (\GetDomainsResponse' {domains} -> domains) (\s@GetDomainsResponse' {} a -> s {domains = a} :: GetDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDomains@ request
-- and specify the next page token using the @pageToken@ parameter.
getDomainsResponse_nextPageToken :: Lens.Lens' GetDomainsResponse (Prelude.Maybe Prelude.Text)
getDomainsResponse_nextPageToken = Lens.lens (\GetDomainsResponse' {nextPageToken} -> nextPageToken) (\s@GetDomainsResponse' {} a -> s {nextPageToken = a} :: GetDomainsResponse)

-- | The response's http status code.
getDomainsResponse_httpStatus :: Lens.Lens' GetDomainsResponse Prelude.Int
getDomainsResponse_httpStatus = Lens.lens (\GetDomainsResponse' {httpStatus} -> httpStatus) (\s@GetDomainsResponse' {} a -> s {httpStatus = a} :: GetDomainsResponse)

instance Prelude.NFData GetDomainsResponse where
  rnf GetDomainsResponse' {..} =
    Prelude.rnf domains
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
