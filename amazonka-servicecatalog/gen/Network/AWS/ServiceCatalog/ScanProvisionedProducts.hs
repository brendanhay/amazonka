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
-- Module      : Network.AWS.ServiceCatalog.ScanProvisionedProducts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the provisioned products that are available (not terminated).
--
-- To use additional filtering, see SearchProvisionedProducts.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ScanProvisionedProducts
  ( -- * Creating a Request
    ScanProvisionedProducts (..),
    newScanProvisionedProducts,

    -- * Request Lenses
    scanProvisionedProducts_pageSize,
    scanProvisionedProducts_pageToken,
    scanProvisionedProducts_accessLevelFilter,
    scanProvisionedProducts_acceptLanguage,

    -- * Destructuring the Response
    ScanProvisionedProductsResponse (..),
    newScanProvisionedProductsResponse,

    -- * Response Lenses
    scanProvisionedProductsResponse_provisionedProducts,
    scanProvisionedProductsResponse_nextPageToken,
    scanProvisionedProductsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newScanProvisionedProducts' smart constructor.
data ScanProvisionedProducts = ScanProvisionedProducts'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The access level to use to obtain results. The default is @User@.
    accessLevelFilter :: Prelude.Maybe AccessLevelFilter,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanProvisionedProducts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'scanProvisionedProducts_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'scanProvisionedProducts_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'accessLevelFilter', 'scanProvisionedProducts_accessLevelFilter' - The access level to use to obtain results. The default is @User@.
--
-- 'acceptLanguage', 'scanProvisionedProducts_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newScanProvisionedProducts ::
  ScanProvisionedProducts
newScanProvisionedProducts =
  ScanProvisionedProducts'
    { pageSize =
        Prelude.Nothing,
      pageToken = Prelude.Nothing,
      accessLevelFilter = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing
    }

-- | The maximum number of items to return with this call.
scanProvisionedProducts_pageSize :: Lens.Lens' ScanProvisionedProducts (Prelude.Maybe Prelude.Natural)
scanProvisionedProducts_pageSize = Lens.lens (\ScanProvisionedProducts' {pageSize} -> pageSize) (\s@ScanProvisionedProducts' {} a -> s {pageSize = a} :: ScanProvisionedProducts)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
scanProvisionedProducts_pageToken :: Lens.Lens' ScanProvisionedProducts (Prelude.Maybe Prelude.Text)
scanProvisionedProducts_pageToken = Lens.lens (\ScanProvisionedProducts' {pageToken} -> pageToken) (\s@ScanProvisionedProducts' {} a -> s {pageToken = a} :: ScanProvisionedProducts)

-- | The access level to use to obtain results. The default is @User@.
scanProvisionedProducts_accessLevelFilter :: Lens.Lens' ScanProvisionedProducts (Prelude.Maybe AccessLevelFilter)
scanProvisionedProducts_accessLevelFilter = Lens.lens (\ScanProvisionedProducts' {accessLevelFilter} -> accessLevelFilter) (\s@ScanProvisionedProducts' {} a -> s {accessLevelFilter = a} :: ScanProvisionedProducts)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
scanProvisionedProducts_acceptLanguage :: Lens.Lens' ScanProvisionedProducts (Prelude.Maybe Prelude.Text)
scanProvisionedProducts_acceptLanguage = Lens.lens (\ScanProvisionedProducts' {acceptLanguage} -> acceptLanguage) (\s@ScanProvisionedProducts' {} a -> s {acceptLanguage = a} :: ScanProvisionedProducts)

instance Core.AWSPager ScanProvisionedProducts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? scanProvisionedProductsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? scanProvisionedProductsResponse_provisionedProducts
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& scanProvisionedProducts_pageToken
          Lens..~ rs
          Lens.^? scanProvisionedProductsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ScanProvisionedProducts where
  type
    AWSResponse ScanProvisionedProducts =
      ScanProvisionedProductsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ScanProvisionedProductsResponse'
            Prelude.<$> ( x Core..?> "ProvisionedProducts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ScanProvisionedProducts

instance Prelude.NFData ScanProvisionedProducts

instance Core.ToHeaders ScanProvisionedProducts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ScanProvisionedProducts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ScanProvisionedProducts where
  toJSON ScanProvisionedProducts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("AccessLevelFilter" Core..=)
              Prelude.<$> accessLevelFilter,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage
          ]
      )

instance Core.ToPath ScanProvisionedProducts where
  toPath = Prelude.const "/"

instance Core.ToQuery ScanProvisionedProducts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newScanProvisionedProductsResponse' smart constructor.
data ScanProvisionedProductsResponse = ScanProvisionedProductsResponse'
  { -- | Information about the provisioned products.
    provisionedProducts :: Prelude.Maybe [ProvisionedProductDetail],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanProvisionedProductsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedProducts', 'scanProvisionedProductsResponse_provisionedProducts' - Information about the provisioned products.
--
-- 'nextPageToken', 'scanProvisionedProductsResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'scanProvisionedProductsResponse_httpStatus' - The response's http status code.
newScanProvisionedProductsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ScanProvisionedProductsResponse
newScanProvisionedProductsResponse pHttpStatus_ =
  ScanProvisionedProductsResponse'
    { provisionedProducts =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the provisioned products.
scanProvisionedProductsResponse_provisionedProducts :: Lens.Lens' ScanProvisionedProductsResponse (Prelude.Maybe [ProvisionedProductDetail])
scanProvisionedProductsResponse_provisionedProducts = Lens.lens (\ScanProvisionedProductsResponse' {provisionedProducts} -> provisionedProducts) (\s@ScanProvisionedProductsResponse' {} a -> s {provisionedProducts = a} :: ScanProvisionedProductsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
scanProvisionedProductsResponse_nextPageToken :: Lens.Lens' ScanProvisionedProductsResponse (Prelude.Maybe Prelude.Text)
scanProvisionedProductsResponse_nextPageToken = Lens.lens (\ScanProvisionedProductsResponse' {nextPageToken} -> nextPageToken) (\s@ScanProvisionedProductsResponse' {} a -> s {nextPageToken = a} :: ScanProvisionedProductsResponse)

-- | The response's http status code.
scanProvisionedProductsResponse_httpStatus :: Lens.Lens' ScanProvisionedProductsResponse Prelude.Int
scanProvisionedProductsResponse_httpStatus = Lens.lens (\ScanProvisionedProductsResponse' {httpStatus} -> httpStatus) (\s@ScanProvisionedProductsResponse' {} a -> s {httpStatus = a} :: ScanProvisionedProductsResponse)

instance
  Prelude.NFData
    ScanProvisionedProductsResponse
