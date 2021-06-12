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
-- Module      : Network.AWS.Lightsail.GetDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all domains in the user\'s account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDomains
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDomains' smart constructor.
data GetDomains = GetDomains'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDomains@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newGetDomains = GetDomains' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDomains@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getDomains_pageToken :: Lens.Lens' GetDomains (Core.Maybe Core.Text)
getDomains_pageToken = Lens.lens (\GetDomains' {pageToken} -> pageToken) (\s@GetDomains' {} a -> s {pageToken = a} :: GetDomains)

instance Core.AWSPager GetDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDomainsResponse_nextPageToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getDomainsResponse_domains Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDomains_pageToken
          Lens..~ rs
          Lens.^? getDomainsResponse_nextPageToken Core.. Lens._Just

instance Core.AWSRequest GetDomains where
  type AWSResponse GetDomains = GetDomainsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainsResponse'
            Core.<$> (x Core..?> "domains" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDomains

instance Core.NFData GetDomains

instance Core.ToHeaders GetDomains where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.GetDomains" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDomains where
  toJSON GetDomains' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetDomains where
  toPath = Core.const "/"

instance Core.ToQuery GetDomains where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDomainsResponse' smart constructor.
data GetDomainsResponse = GetDomainsResponse'
  { -- | An array of key-value pairs containing information about each of the
    -- domain entries in the user\'s account.
    domains :: Core.Maybe [Domain],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetDomains@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetDomainsResponse
newGetDomainsResponse pHttpStatus_ =
  GetDomainsResponse'
    { domains = Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about each of the
-- domain entries in the user\'s account.
getDomainsResponse_domains :: Lens.Lens' GetDomainsResponse (Core.Maybe [Domain])
getDomainsResponse_domains = Lens.lens (\GetDomainsResponse' {domains} -> domains) (\s@GetDomainsResponse' {} a -> s {domains = a} :: GetDomainsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDomains@ request
-- and specify the next page token using the @pageToken@ parameter.
getDomainsResponse_nextPageToken :: Lens.Lens' GetDomainsResponse (Core.Maybe Core.Text)
getDomainsResponse_nextPageToken = Lens.lens (\GetDomainsResponse' {nextPageToken} -> nextPageToken) (\s@GetDomainsResponse' {} a -> s {nextPageToken = a} :: GetDomainsResponse)

-- | The response's http status code.
getDomainsResponse_httpStatus :: Lens.Lens' GetDomainsResponse Core.Int
getDomainsResponse_httpStatus = Lens.lens (\GetDomainsResponse' {httpStatus} -> httpStatus) (\s@GetDomainsResponse' {} a -> s {httpStatus = a} :: GetDomainsResponse)

instance Core.NFData GetDomainsResponse
