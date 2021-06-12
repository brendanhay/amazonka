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
-- Module      : Network.AWS.Lightsail.GetActiveNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the names of all active (not deleted) resources.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetActiveNames
  ( -- * Creating a Request
    GetActiveNames (..),
    newGetActiveNames,

    -- * Request Lenses
    getActiveNames_pageToken,

    -- * Destructuring the Response
    GetActiveNamesResponse (..),
    newGetActiveNamesResponse,

    -- * Response Lenses
    getActiveNamesResponse_nextPageToken,
    getActiveNamesResponse_activeNames,
    getActiveNamesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetActiveNames' smart constructor.
data GetActiveNames = GetActiveNames'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetActiveNames@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetActiveNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getActiveNames_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
newGetActiveNames ::
  GetActiveNames
newGetActiveNames =
  GetActiveNames' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getActiveNames_pageToken :: Lens.Lens' GetActiveNames (Core.Maybe Core.Text)
getActiveNames_pageToken = Lens.lens (\GetActiveNames' {pageToken} -> pageToken) (\s@GetActiveNames' {} a -> s {pageToken = a} :: GetActiveNames)

instance Core.AWSPager GetActiveNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getActiveNamesResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getActiveNamesResponse_activeNames Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getActiveNames_pageToken
          Lens..~ rs
          Lens.^? getActiveNamesResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest GetActiveNames where
  type
    AWSResponse GetActiveNames =
      GetActiveNamesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetActiveNamesResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> (x Core..?> "activeNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetActiveNames

instance Core.NFData GetActiveNames

instance Core.ToHeaders GetActiveNames where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetActiveNames" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetActiveNames where
  toJSON GetActiveNames' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetActiveNames where
  toPath = Core.const "/"

instance Core.ToQuery GetActiveNames where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetActiveNamesResponse' smart constructor.
data GetActiveNamesResponse = GetActiveNamesResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetActiveNames@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The list of active names returned by the get active names request.
    activeNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetActiveNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getActiveNamesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetActiveNames@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'activeNames', 'getActiveNamesResponse_activeNames' - The list of active names returned by the get active names request.
--
-- 'httpStatus', 'getActiveNamesResponse_httpStatus' - The response's http status code.
newGetActiveNamesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetActiveNamesResponse
newGetActiveNamesResponse pHttpStatus_ =
  GetActiveNamesResponse'
    { nextPageToken =
        Core.Nothing,
      activeNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetActiveNames@
-- request and specify the next page token using the @pageToken@ parameter.
getActiveNamesResponse_nextPageToken :: Lens.Lens' GetActiveNamesResponse (Core.Maybe Core.Text)
getActiveNamesResponse_nextPageToken = Lens.lens (\GetActiveNamesResponse' {nextPageToken} -> nextPageToken) (\s@GetActiveNamesResponse' {} a -> s {nextPageToken = a} :: GetActiveNamesResponse)

-- | The list of active names returned by the get active names request.
getActiveNamesResponse_activeNames :: Lens.Lens' GetActiveNamesResponse (Core.Maybe [Core.Text])
getActiveNamesResponse_activeNames = Lens.lens (\GetActiveNamesResponse' {activeNames} -> activeNames) (\s@GetActiveNamesResponse' {} a -> s {activeNames = a} :: GetActiveNamesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getActiveNamesResponse_httpStatus :: Lens.Lens' GetActiveNamesResponse Core.Int
getActiveNamesResponse_httpStatus = Lens.lens (\GetActiveNamesResponse' {httpStatus} -> httpStatus) (\s@GetActiveNamesResponse' {} a -> s {httpStatus = a} :: GetActiveNamesResponse)

instance Core.NFData GetActiveNamesResponse
