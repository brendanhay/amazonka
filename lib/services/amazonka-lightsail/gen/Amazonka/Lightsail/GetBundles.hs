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
-- Module      : Amazonka.Lightsail.GetBundles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the bundles that you can apply to an Amazon Lightsail instance
-- when you create it.
--
-- A bundle describes the specifications of an instance, such as the
-- monthly cost, amount of memory, the number of vCPUs, amount of storage
-- space, and monthly network data transfer quota.
--
-- Bundles are referred to as /instance plans/ in the Lightsail console.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetBundles
  ( -- * Creating a Request
    GetBundles (..),
    newGetBundles,

    -- * Request Lenses
    getBundles_includeInactive,
    getBundles_pageToken,

    -- * Destructuring the Response
    GetBundlesResponse (..),
    newGetBundlesResponse,

    -- * Response Lenses
    getBundlesResponse_bundles,
    getBundlesResponse_nextPageToken,
    getBundlesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBundles' smart constructor.
data GetBundles = GetBundles'
  { -- | A Boolean value that indicates whether to include inactive (unavailable)
    -- bundles in the response of your request.
    includeInactive :: Prelude.Maybe Prelude.Bool,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetBundles@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
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
-- 'includeInactive', 'getBundles_includeInactive' - A Boolean value that indicates whether to include inactive (unavailable)
-- bundles in the response of your request.
--
-- 'pageToken', 'getBundles_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBundles@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetBundles ::
  GetBundles
newGetBundles =
  GetBundles'
    { includeInactive = Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | A Boolean value that indicates whether to include inactive (unavailable)
-- bundles in the response of your request.
getBundles_includeInactive :: Lens.Lens' GetBundles (Prelude.Maybe Prelude.Bool)
getBundles_includeInactive = Lens.lens (\GetBundles' {includeInactive} -> includeInactive) (\s@GetBundles' {} a -> s {includeInactive = a} :: GetBundles)

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBundles@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getBundles_pageToken :: Lens.Lens' GetBundles (Prelude.Maybe Prelude.Text)
getBundles_pageToken = Lens.lens (\GetBundles' {pageToken} -> pageToken) (\s@GetBundles' {} a -> s {pageToken = a} :: GetBundles)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBundlesResponse'
            Prelude.<$> (x Data..?> "bundles" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBundles where
  hashWithSalt _salt GetBundles' {..} =
    _salt `Prelude.hashWithSalt` includeInactive
      `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetBundles where
  rnf GetBundles' {..} =
    Prelude.rnf includeInactive
      `Prelude.seq` Prelude.rnf pageToken

instance Data.ToHeaders GetBundles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetBundles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBundles where
  toJSON GetBundles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("includeInactive" Data..=)
              Prelude.<$> includeInactive,
            ("pageToken" Data..=) Prelude.<$> pageToken
          ]
      )

instance Data.ToPath GetBundles where
  toPath = Prelude.const "/"

instance Data.ToQuery GetBundles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBundlesResponse' smart constructor.
data GetBundlesResponse = GetBundlesResponse'
  { -- | An array of key-value pairs that contains information about the
    -- available bundles.
    bundles :: Prelude.Maybe [Bundle],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetBundles@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
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
-- 'bundles', 'getBundlesResponse_bundles' - An array of key-value pairs that contains information about the
-- available bundles.
--
-- 'nextPageToken', 'getBundlesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBundles@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getBundlesResponse_httpStatus' - The response's http status code.
newGetBundlesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBundlesResponse
newGetBundlesResponse pHttpStatus_ =
  GetBundlesResponse'
    { bundles = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs that contains information about the
-- available bundles.
getBundlesResponse_bundles :: Lens.Lens' GetBundlesResponse (Prelude.Maybe [Bundle])
getBundlesResponse_bundles = Lens.lens (\GetBundlesResponse' {bundles} -> bundles) (\s@GetBundlesResponse' {} a -> s {bundles = a} :: GetBundlesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBundles@ request
-- and specify the next page token using the @pageToken@ parameter.
getBundlesResponse_nextPageToken :: Lens.Lens' GetBundlesResponse (Prelude.Maybe Prelude.Text)
getBundlesResponse_nextPageToken = Lens.lens (\GetBundlesResponse' {nextPageToken} -> nextPageToken) (\s@GetBundlesResponse' {} a -> s {nextPageToken = a} :: GetBundlesResponse)

-- | The response's http status code.
getBundlesResponse_httpStatus :: Lens.Lens' GetBundlesResponse Prelude.Int
getBundlesResponse_httpStatus = Lens.lens (\GetBundlesResponse' {httpStatus} -> httpStatus) (\s@GetBundlesResponse' {} a -> s {httpStatus = a} :: GetBundlesResponse)

instance Prelude.NFData GetBundlesResponse where
  rnf GetBundlesResponse' {..} =
    Prelude.rnf bundles
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
