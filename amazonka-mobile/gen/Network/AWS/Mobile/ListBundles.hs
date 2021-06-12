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
-- Module      : Network.AWS.Mobile.ListBundles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all available bundles.
--
-- This operation returns paginated results.
module Network.AWS.Mobile.ListBundles
  ( -- * Creating a Request
    ListBundles (..),
    newListBundles,

    -- * Request Lenses
    listBundles_nextToken,
    listBundles_maxResults,

    -- * Destructuring the Response
    ListBundlesResponse (..),
    newListBundlesResponse,

    -- * Response Lenses
    listBundlesResponse_nextToken,
    listBundlesResponse_bundleList,
    listBundlesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure to request all available bundles.
--
-- /See:/ 'newListBundles' smart constructor.
data ListBundles = ListBundles'
  { -- | Pagination token. Set to null to start listing bundles from start. If
    -- non-null pagination token is returned in a result, then pass its value
    -- in here in another request to list more bundles.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of records to list in a single response.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBundles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBundles_nextToken' - Pagination token. Set to null to start listing bundles from start. If
-- non-null pagination token is returned in a result, then pass its value
-- in here in another request to list more bundles.
--
-- 'maxResults', 'listBundles_maxResults' - Maximum number of records to list in a single response.
newListBundles ::
  ListBundles
newListBundles =
  ListBundles'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Pagination token. Set to null to start listing bundles from start. If
-- non-null pagination token is returned in a result, then pass its value
-- in here in another request to list more bundles.
listBundles_nextToken :: Lens.Lens' ListBundles (Core.Maybe Core.Text)
listBundles_nextToken = Lens.lens (\ListBundles' {nextToken} -> nextToken) (\s@ListBundles' {} a -> s {nextToken = a} :: ListBundles)

-- | Maximum number of records to list in a single response.
listBundles_maxResults :: Lens.Lens' ListBundles (Core.Maybe Core.Int)
listBundles_maxResults = Lens.lens (\ListBundles' {maxResults} -> maxResults) (\s@ListBundles' {} a -> s {maxResults = a} :: ListBundles)

instance Core.AWSPager ListBundles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBundlesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBundlesResponse_bundleList Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBundles_nextToken
          Lens..~ rs
          Lens.^? listBundlesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListBundles where
  type AWSResponse ListBundles = ListBundlesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBundlesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "bundleList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBundles

instance Core.NFData ListBundles

instance Core.ToHeaders ListBundles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListBundles where
  toPath = Core.const "/bundles"

instance Core.ToQuery ListBundles where
  toQuery ListBundles' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Result structure contains a list of all available bundles with details.
--
-- /See:/ 'newListBundlesResponse' smart constructor.
data ListBundlesResponse = ListBundlesResponse'
  { -- | Pagination token. If non-null pagination token is returned in a result,
    -- then pass its value in another request to fetch more entries.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of bundles.
    bundleList :: Core.Maybe [BundleDetails],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBundlesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBundlesResponse_nextToken' - Pagination token. If non-null pagination token is returned in a result,
-- then pass its value in another request to fetch more entries.
--
-- 'bundleList', 'listBundlesResponse_bundleList' - A list of bundles.
--
-- 'httpStatus', 'listBundlesResponse_httpStatus' - The response's http status code.
newListBundlesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBundlesResponse
newListBundlesResponse pHttpStatus_ =
  ListBundlesResponse'
    { nextToken = Core.Nothing,
      bundleList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token. If non-null pagination token is returned in a result,
-- then pass its value in another request to fetch more entries.
listBundlesResponse_nextToken :: Lens.Lens' ListBundlesResponse (Core.Maybe Core.Text)
listBundlesResponse_nextToken = Lens.lens (\ListBundlesResponse' {nextToken} -> nextToken) (\s@ListBundlesResponse' {} a -> s {nextToken = a} :: ListBundlesResponse)

-- | A list of bundles.
listBundlesResponse_bundleList :: Lens.Lens' ListBundlesResponse (Core.Maybe [BundleDetails])
listBundlesResponse_bundleList = Lens.lens (\ListBundlesResponse' {bundleList} -> bundleList) (\s@ListBundlesResponse' {} a -> s {bundleList = a} :: ListBundlesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBundlesResponse_httpStatus :: Lens.Lens' ListBundlesResponse Core.Int
listBundlesResponse_httpStatus = Lens.lens (\ListBundlesResponse' {httpStatus} -> httpStatus) (\s@ListBundlesResponse' {} a -> s {httpStatus = a} :: ListBundlesResponse)

instance Core.NFData ListBundlesResponse
