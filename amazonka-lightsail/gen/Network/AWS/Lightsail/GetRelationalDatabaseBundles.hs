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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBundles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available in Amazon Lightsail. A
-- bundle describes the performance specifications for a database.
--
-- You can use a bundle ID to create a new database with explicit
-- performance specifications.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBundles
  ( -- * Creating a Request
    GetRelationalDatabaseBundles (..),
    newGetRelationalDatabaseBundles,

    -- * Request Lenses
    getRelationalDatabaseBundles_pageToken,

    -- * Destructuring the Response
    GetRelationalDatabaseBundlesResponse (..),
    newGetRelationalDatabaseBundlesResponse,

    -- * Response Lenses
    getRelationalDatabaseBundlesResponse_nextPageToken,
    getRelationalDatabaseBundlesResponse_bundles,
    getRelationalDatabaseBundlesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseBundles' smart constructor.
data GetRelationalDatabaseBundles = GetRelationalDatabaseBundles'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseBundles@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseBundles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getRelationalDatabaseBundles_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBundles@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
newGetRelationalDatabaseBundles ::
  GetRelationalDatabaseBundles
newGetRelationalDatabaseBundles =
  GetRelationalDatabaseBundles'
    { pageToken =
        Core.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBundles@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getRelationalDatabaseBundles_pageToken :: Lens.Lens' GetRelationalDatabaseBundles (Core.Maybe Core.Text)
getRelationalDatabaseBundles_pageToken = Lens.lens (\GetRelationalDatabaseBundles' {pageToken} -> pageToken) (\s@GetRelationalDatabaseBundles' {} a -> s {pageToken = a} :: GetRelationalDatabaseBundles)

instance Core.AWSPager GetRelationalDatabaseBundles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseBundlesResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getRelationalDatabaseBundlesResponse_bundles
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getRelationalDatabaseBundles_pageToken
          Lens..~ rs
          Lens.^? getRelationalDatabaseBundlesResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest GetRelationalDatabaseBundles where
  type
    AWSResponse GetRelationalDatabaseBundles =
      GetRelationalDatabaseBundlesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseBundlesResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> (x Core..?> "bundles" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRelationalDatabaseBundles

instance Core.NFData GetRelationalDatabaseBundles

instance Core.ToHeaders GetRelationalDatabaseBundles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseBundles" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRelationalDatabaseBundles where
  toJSON GetRelationalDatabaseBundles' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetRelationalDatabaseBundles where
  toPath = Core.const "/"

instance Core.ToQuery GetRelationalDatabaseBundles where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRelationalDatabaseBundlesResponse' smart constructor.
data GetRelationalDatabaseBundlesResponse = GetRelationalDatabaseBundlesResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetRelationalDatabaseBundles@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | An object describing the result of your get relational database bundles
    -- request.
    bundles :: Core.Maybe [RelationalDatabaseBundle],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseBundlesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getRelationalDatabaseBundlesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseBundles@ request and specify the next page token
-- using the @pageToken@ parameter.
--
-- 'bundles', 'getRelationalDatabaseBundlesResponse_bundles' - An object describing the result of your get relational database bundles
-- request.
--
-- 'httpStatus', 'getRelationalDatabaseBundlesResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseBundlesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRelationalDatabaseBundlesResponse
newGetRelationalDatabaseBundlesResponse pHttpStatus_ =
  GetRelationalDatabaseBundlesResponse'
    { nextPageToken =
        Core.Nothing,
      bundles = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetRelationalDatabaseBundles@ request and specify the next page token
-- using the @pageToken@ parameter.
getRelationalDatabaseBundlesResponse_nextPageToken :: Lens.Lens' GetRelationalDatabaseBundlesResponse (Core.Maybe Core.Text)
getRelationalDatabaseBundlesResponse_nextPageToken = Lens.lens (\GetRelationalDatabaseBundlesResponse' {nextPageToken} -> nextPageToken) (\s@GetRelationalDatabaseBundlesResponse' {} a -> s {nextPageToken = a} :: GetRelationalDatabaseBundlesResponse)

-- | An object describing the result of your get relational database bundles
-- request.
getRelationalDatabaseBundlesResponse_bundles :: Lens.Lens' GetRelationalDatabaseBundlesResponse (Core.Maybe [RelationalDatabaseBundle])
getRelationalDatabaseBundlesResponse_bundles = Lens.lens (\GetRelationalDatabaseBundlesResponse' {bundles} -> bundles) (\s@GetRelationalDatabaseBundlesResponse' {} a -> s {bundles = a} :: GetRelationalDatabaseBundlesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getRelationalDatabaseBundlesResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseBundlesResponse Core.Int
getRelationalDatabaseBundlesResponse_httpStatus = Lens.lens (\GetRelationalDatabaseBundlesResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseBundlesResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseBundlesResponse)

instance
  Core.NFData
    GetRelationalDatabaseBundlesResponse
