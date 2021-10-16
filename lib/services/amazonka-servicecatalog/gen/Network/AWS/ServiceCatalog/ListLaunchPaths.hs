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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListLaunchPaths' smart constructor.
data ListLaunchPaths = ListLaunchPaths'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListLaunchPaths
newListLaunchPaths pProductId_ =
  ListLaunchPaths'
    { pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      productId = pProductId_
    }

-- | The maximum number of items to return with this call.
listLaunchPaths_pageSize :: Lens.Lens' ListLaunchPaths (Prelude.Maybe Prelude.Natural)
listLaunchPaths_pageSize = Lens.lens (\ListLaunchPaths' {pageSize} -> pageSize) (\s@ListLaunchPaths' {} a -> s {pageSize = a} :: ListLaunchPaths)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listLaunchPaths_pageToken :: Lens.Lens' ListLaunchPaths (Prelude.Maybe Prelude.Text)
listLaunchPaths_pageToken = Lens.lens (\ListLaunchPaths' {pageToken} -> pageToken) (\s@ListLaunchPaths' {} a -> s {pageToken = a} :: ListLaunchPaths)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listLaunchPaths_acceptLanguage :: Lens.Lens' ListLaunchPaths (Prelude.Maybe Prelude.Text)
listLaunchPaths_acceptLanguage = Lens.lens (\ListLaunchPaths' {acceptLanguage} -> acceptLanguage) (\s@ListLaunchPaths' {} a -> s {acceptLanguage = a} :: ListLaunchPaths)

-- | The product identifier.
listLaunchPaths_productId :: Lens.Lens' ListLaunchPaths Prelude.Text
listLaunchPaths_productId = Lens.lens (\ListLaunchPaths' {productId} -> productId) (\s@ListLaunchPaths' {} a -> s {productId = a} :: ListLaunchPaths)

instance Core.AWSPager ListLaunchPaths where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLaunchPathsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLaunchPathsResponse_launchPathSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLaunchPaths_pageToken
          Lens..~ rs
          Lens.^? listLaunchPathsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLaunchPaths where
  type
    AWSResponse ListLaunchPaths =
      ListLaunchPathsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLaunchPathsResponse'
            Prelude.<$> ( x Core..?> "LaunchPathSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLaunchPaths

instance Prelude.NFData ListLaunchPaths

instance Core.ToHeaders ListLaunchPaths where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListLaunchPaths" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLaunchPaths where
  toJSON ListLaunchPaths' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("ProductId" Core..= productId)
          ]
      )

instance Core.ToPath ListLaunchPaths where
  toPath = Prelude.const "/"

instance Core.ToQuery ListLaunchPaths where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLaunchPathsResponse' smart constructor.
data ListLaunchPathsResponse = ListLaunchPathsResponse'
  { -- | Information about the launch path.
    launchPathSummaries :: Prelude.Maybe [LaunchPathSummary],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListLaunchPathsResponse
newListLaunchPathsResponse pHttpStatus_ =
  ListLaunchPathsResponse'
    { launchPathSummaries =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch path.
listLaunchPathsResponse_launchPathSummaries :: Lens.Lens' ListLaunchPathsResponse (Prelude.Maybe [LaunchPathSummary])
listLaunchPathsResponse_launchPathSummaries = Lens.lens (\ListLaunchPathsResponse' {launchPathSummaries} -> launchPathSummaries) (\s@ListLaunchPathsResponse' {} a -> s {launchPathSummaries = a} :: ListLaunchPathsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listLaunchPathsResponse_nextPageToken :: Lens.Lens' ListLaunchPathsResponse (Prelude.Maybe Prelude.Text)
listLaunchPathsResponse_nextPageToken = Lens.lens (\ListLaunchPathsResponse' {nextPageToken} -> nextPageToken) (\s@ListLaunchPathsResponse' {} a -> s {nextPageToken = a} :: ListLaunchPathsResponse)

-- | The response's http status code.
listLaunchPathsResponse_httpStatus :: Lens.Lens' ListLaunchPathsResponse Prelude.Int
listLaunchPathsResponse_httpStatus = Lens.lens (\ListLaunchPathsResponse' {httpStatus} -> httpStatus) (\s@ListLaunchPathsResponse' {} a -> s {httpStatus = a} :: ListLaunchPathsResponse)

instance Prelude.NFData ListLaunchPathsResponse
