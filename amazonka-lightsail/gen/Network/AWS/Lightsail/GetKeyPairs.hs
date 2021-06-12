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
-- Module      : Network.AWS.Lightsail.GetKeyPairs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all key pairs in the user\'s account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetKeyPairs
  ( -- * Creating a Request
    GetKeyPairs (..),
    newGetKeyPairs,

    -- * Request Lenses
    getKeyPairs_pageToken,

    -- * Destructuring the Response
    GetKeyPairsResponse (..),
    newGetKeyPairsResponse,

    -- * Response Lenses
    getKeyPairsResponse_keyPairs,
    getKeyPairsResponse_nextPageToken,
    getKeyPairsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetKeyPairs' smart constructor.
data GetKeyPairs = GetKeyPairs'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetKeyPairs@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  GetKeyPairs' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetKeyPairs@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getKeyPairs_pageToken :: Lens.Lens' GetKeyPairs (Core.Maybe Core.Text)
getKeyPairs_pageToken = Lens.lens (\GetKeyPairs' {pageToken} -> pageToken) (\s@GetKeyPairs' {} a -> s {pageToken = a} :: GetKeyPairs)

instance Core.AWSPager GetKeyPairs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getKeyPairsResponse_nextPageToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getKeyPairsResponse_keyPairs Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getKeyPairs_pageToken
          Lens..~ rs
          Lens.^? getKeyPairsResponse_nextPageToken Core.. Lens._Just

instance Core.AWSRequest GetKeyPairs where
  type AWSResponse GetKeyPairs = GetKeyPairsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyPairsResponse'
            Core.<$> (x Core..?> "keyPairs" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetKeyPairs

instance Core.NFData GetKeyPairs

instance Core.ToHeaders GetKeyPairs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetKeyPairs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetKeyPairs where
  toJSON GetKeyPairs' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetKeyPairs where
  toPath = Core.const "/"

instance Core.ToQuery GetKeyPairs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetKeyPairsResponse' smart constructor.
data GetKeyPairsResponse = GetKeyPairsResponse'
  { -- | An array of key-value pairs containing information about the key pairs.
    keyPairs :: Core.Maybe [KeyPair],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetKeyPairs@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetKeyPairsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairs', 'getKeyPairsResponse_keyPairs' - An array of key-value pairs containing information about the key pairs.
--
-- 'nextPageToken', 'getKeyPairsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetKeyPairs@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getKeyPairsResponse_httpStatus' - The response's http status code.
newGetKeyPairsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetKeyPairsResponse
newGetKeyPairsResponse pHttpStatus_ =
  GetKeyPairsResponse'
    { keyPairs = Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the key pairs.
getKeyPairsResponse_keyPairs :: Lens.Lens' GetKeyPairsResponse (Core.Maybe [KeyPair])
getKeyPairsResponse_keyPairs = Lens.lens (\GetKeyPairsResponse' {keyPairs} -> keyPairs) (\s@GetKeyPairsResponse' {} a -> s {keyPairs = a} :: GetKeyPairsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetKeyPairs@ request
-- and specify the next page token using the @pageToken@ parameter.
getKeyPairsResponse_nextPageToken :: Lens.Lens' GetKeyPairsResponse (Core.Maybe Core.Text)
getKeyPairsResponse_nextPageToken = Lens.lens (\GetKeyPairsResponse' {nextPageToken} -> nextPageToken) (\s@GetKeyPairsResponse' {} a -> s {nextPageToken = a} :: GetKeyPairsResponse)

-- | The response's http status code.
getKeyPairsResponse_httpStatus :: Lens.Lens' GetKeyPairsResponse Core.Int
getKeyPairsResponse_httpStatus = Lens.lens (\GetKeyPairsResponse' {httpStatus} -> httpStatus) (\s@GetKeyPairsResponse' {} a -> s {httpStatus = a} :: GetKeyPairsResponse)

instance Core.NFData GetKeyPairsResponse
