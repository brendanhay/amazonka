{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLoadBalancers' smart constructor.
data GetLoadBalancers = GetLoadBalancers'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetLoadBalancers@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager GetLoadBalancers where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getLoadBalancersResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getLoadBalancersResponse_loadBalancers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getLoadBalancers_pageToken
          Lens..~ rs
          Lens.^? getLoadBalancersResponse_nextPageToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetLoadBalancers where
  type Rs GetLoadBalancers = GetLoadBalancersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancersResponse'
            Prelude.<$> (x Prelude..?> "nextPageToken")
            Prelude.<*> ( x Prelude..?> "loadBalancers"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoadBalancers

instance Prelude.NFData GetLoadBalancers

instance Prelude.ToHeaders GetLoadBalancers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetLoadBalancers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetLoadBalancers where
  toJSON GetLoadBalancers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("pageToken" Prelude..=) Prelude.<$> pageToken]
      )

instance Prelude.ToPath GetLoadBalancers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetLoadBalancers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoadBalancersResponse' smart constructor.
data GetLoadBalancersResponse = GetLoadBalancersResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetLoadBalancers@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of LoadBalancer objects describing your load balancers.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetLoadBalancersResponse
newGetLoadBalancersResponse pHttpStatus_ =
  GetLoadBalancersResponse'
    { nextPageToken =
        Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetLoadBalancers@
-- request and specify the next page token using the @pageToken@ parameter.
getLoadBalancersResponse_nextPageToken :: Lens.Lens' GetLoadBalancersResponse (Prelude.Maybe Prelude.Text)
getLoadBalancersResponse_nextPageToken = Lens.lens (\GetLoadBalancersResponse' {nextPageToken} -> nextPageToken) (\s@GetLoadBalancersResponse' {} a -> s {nextPageToken = a} :: GetLoadBalancersResponse)

-- | An array of LoadBalancer objects describing your load balancers.
getLoadBalancersResponse_loadBalancers :: Lens.Lens' GetLoadBalancersResponse (Prelude.Maybe [LoadBalancer])
getLoadBalancersResponse_loadBalancers = Lens.lens (\GetLoadBalancersResponse' {loadBalancers} -> loadBalancers) (\s@GetLoadBalancersResponse' {} a -> s {loadBalancers = a} :: GetLoadBalancersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getLoadBalancersResponse_httpStatus :: Lens.Lens' GetLoadBalancersResponse Prelude.Int
getLoadBalancersResponse_httpStatus = Lens.lens (\GetLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@GetLoadBalancersResponse' {} a -> s {httpStatus = a} :: GetLoadBalancersResponse)

instance Prelude.NFData GetLoadBalancersResponse
