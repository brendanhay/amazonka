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
-- Module      : Network.AWS.Lightsail.GetStaticIps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all static IPs in the user\'s account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetStaticIps
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetStaticIps' smart constructor.
data GetStaticIps = GetStaticIps'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetStaticIps@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  GetStaticIps' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetStaticIps@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getStaticIps_pageToken :: Lens.Lens' GetStaticIps (Core.Maybe Core.Text)
getStaticIps_pageToken = Lens.lens (\GetStaticIps' {pageToken} -> pageToken) (\s@GetStaticIps' {} a -> s {pageToken = a} :: GetStaticIps)

instance Core.AWSPager GetStaticIps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getStaticIpsResponse_nextPageToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getStaticIpsResponse_staticIps Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getStaticIps_pageToken
          Lens..~ rs
          Lens.^? getStaticIpsResponse_nextPageToken Core.. Lens._Just

instance Core.AWSRequest GetStaticIps where
  type AWSResponse GetStaticIps = GetStaticIpsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStaticIpsResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> (x Core..?> "staticIps" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetStaticIps

instance Core.NFData GetStaticIps

instance Core.ToHeaders GetStaticIps where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetStaticIps" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetStaticIps where
  toJSON GetStaticIps' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetStaticIps where
  toPath = Core.const "/"

instance Core.ToQuery GetStaticIps where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetStaticIpsResponse' smart constructor.
data GetStaticIpsResponse = GetStaticIpsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetStaticIps@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | An array of key-value pairs containing information about your get static
    -- IPs request.
    staticIps :: Core.Maybe [StaticIp],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetStaticIpsResponse
newGetStaticIpsResponse pHttpStatus_ =
  GetStaticIpsResponse'
    { nextPageToken = Core.Nothing,
      staticIps = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetStaticIps@ request
-- and specify the next page token using the @pageToken@ parameter.
getStaticIpsResponse_nextPageToken :: Lens.Lens' GetStaticIpsResponse (Core.Maybe Core.Text)
getStaticIpsResponse_nextPageToken = Lens.lens (\GetStaticIpsResponse' {nextPageToken} -> nextPageToken) (\s@GetStaticIpsResponse' {} a -> s {nextPageToken = a} :: GetStaticIpsResponse)

-- | An array of key-value pairs containing information about your get static
-- IPs request.
getStaticIpsResponse_staticIps :: Lens.Lens' GetStaticIpsResponse (Core.Maybe [StaticIp])
getStaticIpsResponse_staticIps = Lens.lens (\GetStaticIpsResponse' {staticIps} -> staticIps) (\s@GetStaticIpsResponse' {} a -> s {staticIps = a} :: GetStaticIpsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getStaticIpsResponse_httpStatus :: Lens.Lens' GetStaticIpsResponse Core.Int
getStaticIpsResponse_httpStatus = Lens.lens (\GetStaticIpsResponse' {httpStatus} -> httpStatus) (\s@GetStaticIpsResponse' {} a -> s {httpStatus = a} :: GetStaticIpsResponse)

instance Core.NFData GetStaticIpsResponse
