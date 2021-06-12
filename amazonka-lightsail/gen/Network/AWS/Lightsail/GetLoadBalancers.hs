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
-- Module      : Network.AWS.Lightsail.GetLoadBalancers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all load balancers in an account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetLoadBalancers
  ( -- * Creating a Request
    GetLoadBalancers (..),
    newGetLoadBalancers,

    -- * Request Lenses
    getLoadBalancers_pageToken,

    -- * Destructuring the Response
    GetLoadBalancersResponse (..),
    newGetLoadBalancersResponse,

    -- * Response Lenses
    getLoadBalancersResponse_nextPageToken,
    getLoadBalancersResponse_loadBalancers,
    getLoadBalancersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLoadBalancers' smart constructor.
data GetLoadBalancers = GetLoadBalancers'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetLoadBalancers@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLoadBalancers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getLoadBalancers_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetLoadBalancers@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
newGetLoadBalancers ::
  GetLoadBalancers
newGetLoadBalancers =
  GetLoadBalancers' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetLoadBalancers@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getLoadBalancers_pageToken :: Lens.Lens' GetLoadBalancers (Core.Maybe Core.Text)
getLoadBalancers_pageToken = Lens.lens (\GetLoadBalancers' {pageToken} -> pageToken) (\s@GetLoadBalancers' {} a -> s {pageToken = a} :: GetLoadBalancers)

instance Core.AWSPager GetLoadBalancers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getLoadBalancersResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getLoadBalancersResponse_loadBalancers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getLoadBalancers_pageToken
          Lens..~ rs
          Lens.^? getLoadBalancersResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest GetLoadBalancers where
  type
    AWSResponse GetLoadBalancers =
      GetLoadBalancersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancersResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> (x Core..?> "loadBalancers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLoadBalancers

instance Core.NFData GetLoadBalancers

instance Core.ToHeaders GetLoadBalancers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetLoadBalancers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetLoadBalancers where
  toJSON GetLoadBalancers' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetLoadBalancers where
  toPath = Core.const "/"

instance Core.ToQuery GetLoadBalancers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLoadBalancersResponse' smart constructor.
data GetLoadBalancersResponse = GetLoadBalancersResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetLoadBalancers@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | An array of LoadBalancer objects describing your load balancers.
    loadBalancers :: Core.Maybe [LoadBalancer],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLoadBalancersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getLoadBalancersResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetLoadBalancers@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'loadBalancers', 'getLoadBalancersResponse_loadBalancers' - An array of LoadBalancer objects describing your load balancers.
--
-- 'httpStatus', 'getLoadBalancersResponse_httpStatus' - The response's http status code.
newGetLoadBalancersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLoadBalancersResponse
newGetLoadBalancersResponse pHttpStatus_ =
  GetLoadBalancersResponse'
    { nextPageToken =
        Core.Nothing,
      loadBalancers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetLoadBalancers@
-- request and specify the next page token using the @pageToken@ parameter.
getLoadBalancersResponse_nextPageToken :: Lens.Lens' GetLoadBalancersResponse (Core.Maybe Core.Text)
getLoadBalancersResponse_nextPageToken = Lens.lens (\GetLoadBalancersResponse' {nextPageToken} -> nextPageToken) (\s@GetLoadBalancersResponse' {} a -> s {nextPageToken = a} :: GetLoadBalancersResponse)

-- | An array of LoadBalancer objects describing your load balancers.
getLoadBalancersResponse_loadBalancers :: Lens.Lens' GetLoadBalancersResponse (Core.Maybe [LoadBalancer])
getLoadBalancersResponse_loadBalancers = Lens.lens (\GetLoadBalancersResponse' {loadBalancers} -> loadBalancers) (\s@GetLoadBalancersResponse' {} a -> s {loadBalancers = a} :: GetLoadBalancersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getLoadBalancersResponse_httpStatus :: Lens.Lens' GetLoadBalancersResponse Core.Int
getLoadBalancersResponse_httpStatus = Lens.lens (\GetLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@GetLoadBalancersResponse' {} a -> s {httpStatus = a} :: GetLoadBalancersResponse)

instance Core.NFData GetLoadBalancersResponse
