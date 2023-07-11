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
-- Module      : Amazonka.Lightsail.GetLoadBalancers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all load balancers in an account.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetLoadBalancers
  ( -- * Creating a Request
    GetLoadBalancers (..),
    newGetLoadBalancers,

    -- * Request Lenses
    getLoadBalancers_pageToken,

    -- * Destructuring the Response
    GetLoadBalancersResponse (..),
    newGetLoadBalancersResponse,

    -- * Response Lenses
    getLoadBalancersResponse_loadBalancers,
    getLoadBalancersResponse_nextPageToken,
    getLoadBalancersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLoadBalancers' smart constructor.
data GetLoadBalancers = GetLoadBalancers'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetLoadBalancers@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  GetLoadBalancers' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetLoadBalancers@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getLoadBalancers_pageToken :: Lens.Lens' GetLoadBalancers (Prelude.Maybe Prelude.Text)
getLoadBalancers_pageToken = Lens.lens (\GetLoadBalancers' {pageToken} -> pageToken) (\s@GetLoadBalancers' {} a -> s {pageToken = a} :: GetLoadBalancers)

instance Core.AWSPager GetLoadBalancers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getLoadBalancersResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getLoadBalancersResponse_loadBalancers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getLoadBalancers_pageToken
          Lens..~ rs
          Lens.^? getLoadBalancersResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetLoadBalancers where
  type
    AWSResponse GetLoadBalancers =
      GetLoadBalancersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancersResponse'
            Prelude.<$> (x Data..?> "loadBalancers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoadBalancers where
  hashWithSalt _salt GetLoadBalancers' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetLoadBalancers where
  rnf GetLoadBalancers' {..} = Prelude.rnf pageToken

instance Data.ToHeaders GetLoadBalancers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetLoadBalancers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLoadBalancers where
  toJSON GetLoadBalancers' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetLoadBalancers where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLoadBalancers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoadBalancersResponse' smart constructor.
data GetLoadBalancersResponse = GetLoadBalancersResponse'
  { -- | An array of LoadBalancer objects describing your load balancers.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetLoadBalancers@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoadBalancersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancers', 'getLoadBalancersResponse_loadBalancers' - An array of LoadBalancer objects describing your load balancers.
--
-- 'nextPageToken', 'getLoadBalancersResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetLoadBalancers@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getLoadBalancersResponse_httpStatus' - The response's http status code.
newGetLoadBalancersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoadBalancersResponse
newGetLoadBalancersResponse pHttpStatus_ =
  GetLoadBalancersResponse'
    { loadBalancers =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of LoadBalancer objects describing your load balancers.
getLoadBalancersResponse_loadBalancers :: Lens.Lens' GetLoadBalancersResponse (Prelude.Maybe [LoadBalancer])
getLoadBalancersResponse_loadBalancers = Lens.lens (\GetLoadBalancersResponse' {loadBalancers} -> loadBalancers) (\s@GetLoadBalancersResponse' {} a -> s {loadBalancers = a} :: GetLoadBalancersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetLoadBalancers@
-- request and specify the next page token using the @pageToken@ parameter.
getLoadBalancersResponse_nextPageToken :: Lens.Lens' GetLoadBalancersResponse (Prelude.Maybe Prelude.Text)
getLoadBalancersResponse_nextPageToken = Lens.lens (\GetLoadBalancersResponse' {nextPageToken} -> nextPageToken) (\s@GetLoadBalancersResponse' {} a -> s {nextPageToken = a} :: GetLoadBalancersResponse)

-- | The response's http status code.
getLoadBalancersResponse_httpStatus :: Lens.Lens' GetLoadBalancersResponse Prelude.Int
getLoadBalancersResponse_httpStatus = Lens.lens (\GetLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@GetLoadBalancersResponse' {} a -> s {httpStatus = a} :: GetLoadBalancersResponse)

instance Prelude.NFData GetLoadBalancersResponse where
  rnf GetLoadBalancersResponse' {..} =
    Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
