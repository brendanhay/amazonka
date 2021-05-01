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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetStaticIps' smart constructor.
data GetStaticIps = GetStaticIps'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetStaticIps@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager GetStaticIps where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getStaticIpsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getStaticIpsResponse_staticIps Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getStaticIps_pageToken
          Lens..~ rs
          Lens.^? getStaticIpsResponse_nextPageToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetStaticIps where
  type Rs GetStaticIps = GetStaticIpsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStaticIpsResponse'
            Prelude.<$> (x Prelude..?> "nextPageToken")
            Prelude.<*> ( x Prelude..?> "staticIps"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStaticIps

instance Prelude.NFData GetStaticIps

instance Prelude.ToHeaders GetStaticIps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetStaticIps" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetStaticIps where
  toJSON GetStaticIps' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("pageToken" Prelude..=) Prelude.<$> pageToken]
      )

instance Prelude.ToPath GetStaticIps where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetStaticIps where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getStaticIpsResponse_staticIps = Lens.lens (\GetStaticIpsResponse' {staticIps} -> staticIps) (\s@GetStaticIpsResponse' {} a -> s {staticIps = a} :: GetStaticIpsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getStaticIpsResponse_httpStatus :: Lens.Lens' GetStaticIpsResponse Prelude.Int
getStaticIpsResponse_httpStatus = Lens.lens (\GetStaticIpsResponse' {httpStatus} -> httpStatus) (\s@GetStaticIpsResponse' {} a -> s {httpStatus = a} :: GetStaticIpsResponse)

instance Prelude.NFData GetStaticIpsResponse
