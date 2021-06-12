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
-- Module      : Network.AWS.ServiceCatalog.ListLaunchPaths
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the paths to the specified product. A path is how the user has
-- access to a specified product, and is necessary when provisioning a
-- product. A path also determines the constraints put on the product.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListLaunchPaths
  ( -- * Creating a Request
    ListLaunchPaths (..),
    newListLaunchPaths,

    -- * Request Lenses
    listLaunchPaths_pageSize,
    listLaunchPaths_pageToken,
    listLaunchPaths_acceptLanguage,
    listLaunchPaths_productId,

    -- * Destructuring the Response
    ListLaunchPathsResponse (..),
    newListLaunchPathsResponse,

    -- * Response Lenses
    listLaunchPathsResponse_launchPathSummaries,
    listLaunchPathsResponse_nextPageToken,
    listLaunchPathsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListLaunchPaths' smart constructor.
data ListLaunchPaths = ListLaunchPaths'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLaunchPaths' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listLaunchPaths_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listLaunchPaths_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listLaunchPaths_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'listLaunchPaths_productId' - The product identifier.
newListLaunchPaths ::
  -- | 'productId'
  Core.Text ->
  ListLaunchPaths
newListLaunchPaths pProductId_ =
  ListLaunchPaths'
    { pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      acceptLanguage = Core.Nothing,
      productId = pProductId_
    }

-- | The maximum number of items to return with this call.
listLaunchPaths_pageSize :: Lens.Lens' ListLaunchPaths (Core.Maybe Core.Natural)
listLaunchPaths_pageSize = Lens.lens (\ListLaunchPaths' {pageSize} -> pageSize) (\s@ListLaunchPaths' {} a -> s {pageSize = a} :: ListLaunchPaths)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listLaunchPaths_pageToken :: Lens.Lens' ListLaunchPaths (Core.Maybe Core.Text)
listLaunchPaths_pageToken = Lens.lens (\ListLaunchPaths' {pageToken} -> pageToken) (\s@ListLaunchPaths' {} a -> s {pageToken = a} :: ListLaunchPaths)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listLaunchPaths_acceptLanguage :: Lens.Lens' ListLaunchPaths (Core.Maybe Core.Text)
listLaunchPaths_acceptLanguage = Lens.lens (\ListLaunchPaths' {acceptLanguage} -> acceptLanguage) (\s@ListLaunchPaths' {} a -> s {acceptLanguage = a} :: ListLaunchPaths)

-- | The product identifier.
listLaunchPaths_productId :: Lens.Lens' ListLaunchPaths Core.Text
listLaunchPaths_productId = Lens.lens (\ListLaunchPaths' {productId} -> productId) (\s@ListLaunchPaths' {} a -> s {productId = a} :: ListLaunchPaths)

instance Core.AWSPager ListLaunchPaths where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLaunchPathsResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listLaunchPathsResponse_launchPathSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listLaunchPaths_pageToken
          Lens..~ rs
          Lens.^? listLaunchPathsResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest ListLaunchPaths where
  type
    AWSResponse ListLaunchPaths =
      ListLaunchPathsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLaunchPathsResponse'
            Core.<$> ( x Core..?> "LaunchPathSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListLaunchPaths

instance Core.NFData ListLaunchPaths

instance Core.ToHeaders ListLaunchPaths where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListLaunchPaths" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListLaunchPaths where
  toJSON ListLaunchPaths' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("ProductId" Core..= productId)
          ]
      )

instance Core.ToPath ListLaunchPaths where
  toPath = Core.const "/"

instance Core.ToQuery ListLaunchPaths where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListLaunchPathsResponse' smart constructor.
data ListLaunchPathsResponse = ListLaunchPathsResponse'
  { -- | Information about the launch path.
    launchPathSummaries :: Core.Maybe [LaunchPathSummary],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLaunchPathsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchPathSummaries', 'listLaunchPathsResponse_launchPathSummaries' - Information about the launch path.
--
-- 'nextPageToken', 'listLaunchPathsResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listLaunchPathsResponse_httpStatus' - The response's http status code.
newListLaunchPathsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListLaunchPathsResponse
newListLaunchPathsResponse pHttpStatus_ =
  ListLaunchPathsResponse'
    { launchPathSummaries =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch path.
listLaunchPathsResponse_launchPathSummaries :: Lens.Lens' ListLaunchPathsResponse (Core.Maybe [LaunchPathSummary])
listLaunchPathsResponse_launchPathSummaries = Lens.lens (\ListLaunchPathsResponse' {launchPathSummaries} -> launchPathSummaries) (\s@ListLaunchPathsResponse' {} a -> s {launchPathSummaries = a} :: ListLaunchPathsResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listLaunchPathsResponse_nextPageToken :: Lens.Lens' ListLaunchPathsResponse (Core.Maybe Core.Text)
listLaunchPathsResponse_nextPageToken = Lens.lens (\ListLaunchPathsResponse' {nextPageToken} -> nextPageToken) (\s@ListLaunchPathsResponse' {} a -> s {nextPageToken = a} :: ListLaunchPathsResponse)

-- | The response's http status code.
listLaunchPathsResponse_httpStatus :: Lens.Lens' ListLaunchPathsResponse Core.Int
listLaunchPathsResponse_httpStatus = Lens.lens (\ListLaunchPathsResponse' {httpStatus} -> httpStatus) (\s@ListLaunchPathsResponse' {} a -> s {httpStatus = a} :: ListLaunchPathsResponse)

instance Core.NFData ListLaunchPathsResponse
