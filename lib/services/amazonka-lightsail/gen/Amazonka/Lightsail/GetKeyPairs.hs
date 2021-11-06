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
-- Module      : Amazonka.Lightsail.GetKeyPairs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all key pairs in the user\'s account.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetKeyPairs
  ( -- * Creating a Request
    GetKeyPairs (..),
    newGetKeyPairs,

    -- * Request Lenses
    getKeyPairs_pageToken,

    -- * Destructuring the Response
    GetKeyPairsResponse (..),
    newGetKeyPairsResponse,

    -- * Response Lenses
    getKeyPairsResponse_nextPageToken,
    getKeyPairsResponse_keyPairs,
    getKeyPairsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKeyPairs' smart constructor.
data GetKeyPairs = GetKeyPairs'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetKeyPairs@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyPairs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getKeyPairs_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetKeyPairs@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetKeyPairs ::
  GetKeyPairs
newGetKeyPairs =
  GetKeyPairs' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetKeyPairs@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getKeyPairs_pageToken :: Lens.Lens' GetKeyPairs (Prelude.Maybe Prelude.Text)
getKeyPairs_pageToken = Lens.lens (\GetKeyPairs' {pageToken} -> pageToken) (\s@GetKeyPairs' {} a -> s {pageToken = a} :: GetKeyPairs)

instance Core.AWSPager GetKeyPairs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getKeyPairsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getKeyPairsResponse_keyPairs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getKeyPairs_pageToken
          Lens..~ rs
          Lens.^? getKeyPairsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetKeyPairs where
  type AWSResponse GetKeyPairs = GetKeyPairsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyPairsResponse'
            Prelude.<$> (x Core..?> "nextPageToken")
            Prelude.<*> (x Core..?> "keyPairs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKeyPairs

instance Prelude.NFData GetKeyPairs

instance Core.ToHeaders GetKeyPairs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetKeyPairs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetKeyPairs where
  toJSON GetKeyPairs' {..} =
    Core.object
      ( Prelude.catMaybes
          [("pageToken" Core..=) Prelude.<$> pageToken]
      )

instance Core.ToPath GetKeyPairs where
  toPath = Prelude.const "/"

instance Core.ToQuery GetKeyPairs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyPairsResponse' smart constructor.
data GetKeyPairsResponse = GetKeyPairsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetKeyPairs@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs containing information about the key pairs.
    keyPairs :: Prelude.Maybe [KeyPair],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyPairsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getKeyPairsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetKeyPairs@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'keyPairs', 'getKeyPairsResponse_keyPairs' - An array of key-value pairs containing information about the key pairs.
--
-- 'httpStatus', 'getKeyPairsResponse_httpStatus' - The response's http status code.
newGetKeyPairsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKeyPairsResponse
newGetKeyPairsResponse pHttpStatus_ =
  GetKeyPairsResponse'
    { nextPageToken =
        Prelude.Nothing,
      keyPairs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetKeyPairs@ request
-- and specify the next page token using the @pageToken@ parameter.
getKeyPairsResponse_nextPageToken :: Lens.Lens' GetKeyPairsResponse (Prelude.Maybe Prelude.Text)
getKeyPairsResponse_nextPageToken = Lens.lens (\GetKeyPairsResponse' {nextPageToken} -> nextPageToken) (\s@GetKeyPairsResponse' {} a -> s {nextPageToken = a} :: GetKeyPairsResponse)

-- | An array of key-value pairs containing information about the key pairs.
getKeyPairsResponse_keyPairs :: Lens.Lens' GetKeyPairsResponse (Prelude.Maybe [KeyPair])
getKeyPairsResponse_keyPairs = Lens.lens (\GetKeyPairsResponse' {keyPairs} -> keyPairs) (\s@GetKeyPairsResponse' {} a -> s {keyPairs = a} :: GetKeyPairsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getKeyPairsResponse_httpStatus :: Lens.Lens' GetKeyPairsResponse Prelude.Int
getKeyPairsResponse_httpStatus = Lens.lens (\GetKeyPairsResponse' {httpStatus} -> httpStatus) (\s@GetKeyPairsResponse' {} a -> s {httpStatus = a} :: GetKeyPairsResponse)

instance Prelude.NFData GetKeyPairsResponse
