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
-- Module      : Network.AWS.Lightsail.GetBundles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available for purchase. A bundle
-- describes the specs for your virtual private server (or /instance/).
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetBundles
  ( -- * Creating a Request
    GetBundles (..),
    newGetBundles,

    -- * Request Lenses
    getBundles_pageToken,
    getBundles_includeInactive,

    -- * Destructuring the Response
    GetBundlesResponse (..),
    newGetBundlesResponse,

    -- * Response Lenses
    getBundlesResponse_nextPageToken,
    getBundlesResponse_bundles,
    getBundlesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBundles' smart constructor.
data GetBundles = GetBundles'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetBundles@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that indicates whether to include inactive bundle
    -- results in your request.
    includeInactive :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBundles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getBundles_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBundles@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
--
-- 'includeInactive', 'getBundles_includeInactive' - A Boolean value that indicates whether to include inactive bundle
-- results in your request.
newGetBundles ::
  GetBundles
newGetBundles =
  GetBundles'
    { pageToken = Prelude.Nothing,
      includeInactive = Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBundles@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getBundles_pageToken :: Lens.Lens' GetBundles (Prelude.Maybe Prelude.Text)
getBundles_pageToken = Lens.lens (\GetBundles' {pageToken} -> pageToken) (\s@GetBundles' {} a -> s {pageToken = a} :: GetBundles)

-- | A Boolean value that indicates whether to include inactive bundle
-- results in your request.
getBundles_includeInactive :: Lens.Lens' GetBundles (Prelude.Maybe Prelude.Bool)
getBundles_includeInactive = Lens.lens (\GetBundles' {includeInactive} -> includeInactive) (\s@GetBundles' {} a -> s {includeInactive = a} :: GetBundles)

instance Core.AWSPager GetBundles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBundlesResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getBundlesResponse_bundles Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getBundles_pageToken
          Lens..~ rs
          Lens.^? getBundlesResponse_nextPageToken Prelude.. Lens._Just

instance Core.AWSRequest GetBundles where
  type AWSResponse GetBundles = GetBundlesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBundlesResponse'
            Prelude.<$> (x Core..?> "nextPageToken")
            Prelude.<*> (x Core..?> "bundles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBundles

instance Prelude.NFData GetBundles

instance Core.ToHeaders GetBundles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetBundles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBundles where
  toJSON GetBundles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("pageToken" Core..=) Prelude.<$> pageToken,
            ("includeInactive" Core..=)
              Prelude.<$> includeInactive
          ]
      )

instance Core.ToPath GetBundles where
  toPath = Prelude.const "/"

instance Core.ToQuery GetBundles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBundlesResponse' smart constructor.
data GetBundlesResponse = GetBundlesResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetBundles@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs that contains information about the
    -- available bundles.
    bundles :: Prelude.Maybe [Bundle],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBundlesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getBundlesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBundles@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'bundles', 'getBundlesResponse_bundles' - An array of key-value pairs that contains information about the
-- available bundles.
--
-- 'httpStatus', 'getBundlesResponse_httpStatus' - The response's http status code.
newGetBundlesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBundlesResponse
newGetBundlesResponse pHttpStatus_ =
  GetBundlesResponse'
    { nextPageToken =
        Prelude.Nothing,
      bundles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBundles@ request
-- and specify the next page token using the @pageToken@ parameter.
getBundlesResponse_nextPageToken :: Lens.Lens' GetBundlesResponse (Prelude.Maybe Prelude.Text)
getBundlesResponse_nextPageToken = Lens.lens (\GetBundlesResponse' {nextPageToken} -> nextPageToken) (\s@GetBundlesResponse' {} a -> s {nextPageToken = a} :: GetBundlesResponse)

-- | An array of key-value pairs that contains information about the
-- available bundles.
getBundlesResponse_bundles :: Lens.Lens' GetBundlesResponse (Prelude.Maybe [Bundle])
getBundlesResponse_bundles = Lens.lens (\GetBundlesResponse' {bundles} -> bundles) (\s@GetBundlesResponse' {} a -> s {bundles = a} :: GetBundlesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBundlesResponse_httpStatus :: Lens.Lens' GetBundlesResponse Prelude.Int
getBundlesResponse_httpStatus = Lens.lens (\GetBundlesResponse' {httpStatus} -> httpStatus) (\s@GetBundlesResponse' {} a -> s {httpStatus = a} :: GetBundlesResponse)

instance Prelude.NFData GetBundlesResponse
