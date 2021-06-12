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
-- Module      : Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the constraints for the specified portfolio and product.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
  ( -- * Creating a Request
    ListConstraintsForPortfolio (..),
    newListConstraintsForPortfolio,

    -- * Request Lenses
    listConstraintsForPortfolio_pageSize,
    listConstraintsForPortfolio_pageToken,
    listConstraintsForPortfolio_productId,
    listConstraintsForPortfolio_acceptLanguage,
    listConstraintsForPortfolio_portfolioId,

    -- * Destructuring the Response
    ListConstraintsForPortfolioResponse (..),
    newListConstraintsForPortfolioResponse,

    -- * Response Lenses
    listConstraintsForPortfolioResponse_constraintDetails,
    listConstraintsForPortfolioResponse_nextPageToken,
    listConstraintsForPortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListConstraintsForPortfolio' smart constructor.
data ListConstraintsForPortfolio = ListConstraintsForPortfolio'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The portfolio identifier.
    portfolioId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConstraintsForPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listConstraintsForPortfolio_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listConstraintsForPortfolio_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'productId', 'listConstraintsForPortfolio_productId' - The product identifier.
--
-- 'acceptLanguage', 'listConstraintsForPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'listConstraintsForPortfolio_portfolioId' - The portfolio identifier.
newListConstraintsForPortfolio ::
  -- | 'portfolioId'
  Core.Text ->
  ListConstraintsForPortfolio
newListConstraintsForPortfolio pPortfolioId_ =
  ListConstraintsForPortfolio'
    { pageSize =
        Core.Nothing,
      pageToken = Core.Nothing,
      productId = Core.Nothing,
      acceptLanguage = Core.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The maximum number of items to return with this call.
listConstraintsForPortfolio_pageSize :: Lens.Lens' ListConstraintsForPortfolio (Core.Maybe Core.Natural)
listConstraintsForPortfolio_pageSize = Lens.lens (\ListConstraintsForPortfolio' {pageSize} -> pageSize) (\s@ListConstraintsForPortfolio' {} a -> s {pageSize = a} :: ListConstraintsForPortfolio)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listConstraintsForPortfolio_pageToken :: Lens.Lens' ListConstraintsForPortfolio (Core.Maybe Core.Text)
listConstraintsForPortfolio_pageToken = Lens.lens (\ListConstraintsForPortfolio' {pageToken} -> pageToken) (\s@ListConstraintsForPortfolio' {} a -> s {pageToken = a} :: ListConstraintsForPortfolio)

-- | The product identifier.
listConstraintsForPortfolio_productId :: Lens.Lens' ListConstraintsForPortfolio (Core.Maybe Core.Text)
listConstraintsForPortfolio_productId = Lens.lens (\ListConstraintsForPortfolio' {productId} -> productId) (\s@ListConstraintsForPortfolio' {} a -> s {productId = a} :: ListConstraintsForPortfolio)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listConstraintsForPortfolio_acceptLanguage :: Lens.Lens' ListConstraintsForPortfolio (Core.Maybe Core.Text)
listConstraintsForPortfolio_acceptLanguage = Lens.lens (\ListConstraintsForPortfolio' {acceptLanguage} -> acceptLanguage) (\s@ListConstraintsForPortfolio' {} a -> s {acceptLanguage = a} :: ListConstraintsForPortfolio)

-- | The portfolio identifier.
listConstraintsForPortfolio_portfolioId :: Lens.Lens' ListConstraintsForPortfolio Core.Text
listConstraintsForPortfolio_portfolioId = Lens.lens (\ListConstraintsForPortfolio' {portfolioId} -> portfolioId) (\s@ListConstraintsForPortfolio' {} a -> s {portfolioId = a} :: ListConstraintsForPortfolio)

instance Core.AWSPager ListConstraintsForPortfolio where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConstraintsForPortfolioResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listConstraintsForPortfolioResponse_constraintDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listConstraintsForPortfolio_pageToken
          Lens..~ rs
          Lens.^? listConstraintsForPortfolioResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest ListConstraintsForPortfolio where
  type
    AWSResponse ListConstraintsForPortfolio =
      ListConstraintsForPortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConstraintsForPortfolioResponse'
            Core.<$> (x Core..?> "ConstraintDetails" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListConstraintsForPortfolio

instance Core.NFData ListConstraintsForPortfolio

instance Core.ToHeaders ListConstraintsForPortfolio where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListConstraintsForPortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListConstraintsForPortfolio where
  toJSON ListConstraintsForPortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("ProductId" Core..=) Core.<$> productId,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath ListConstraintsForPortfolio where
  toPath = Core.const "/"

instance Core.ToQuery ListConstraintsForPortfolio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListConstraintsForPortfolioResponse' smart constructor.
data ListConstraintsForPortfolioResponse = ListConstraintsForPortfolioResponse'
  { -- | Information about the constraints.
    constraintDetails :: Core.Maybe [ConstraintDetail],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConstraintsForPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintDetails', 'listConstraintsForPortfolioResponse_constraintDetails' - Information about the constraints.
--
-- 'nextPageToken', 'listConstraintsForPortfolioResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listConstraintsForPortfolioResponse_httpStatus' - The response's http status code.
newListConstraintsForPortfolioResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListConstraintsForPortfolioResponse
newListConstraintsForPortfolioResponse pHttpStatus_ =
  ListConstraintsForPortfolioResponse'
    { constraintDetails =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the constraints.
listConstraintsForPortfolioResponse_constraintDetails :: Lens.Lens' ListConstraintsForPortfolioResponse (Core.Maybe [ConstraintDetail])
listConstraintsForPortfolioResponse_constraintDetails = Lens.lens (\ListConstraintsForPortfolioResponse' {constraintDetails} -> constraintDetails) (\s@ListConstraintsForPortfolioResponse' {} a -> s {constraintDetails = a} :: ListConstraintsForPortfolioResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listConstraintsForPortfolioResponse_nextPageToken :: Lens.Lens' ListConstraintsForPortfolioResponse (Core.Maybe Core.Text)
listConstraintsForPortfolioResponse_nextPageToken = Lens.lens (\ListConstraintsForPortfolioResponse' {nextPageToken} -> nextPageToken) (\s@ListConstraintsForPortfolioResponse' {} a -> s {nextPageToken = a} :: ListConstraintsForPortfolioResponse)

-- | The response's http status code.
listConstraintsForPortfolioResponse_httpStatus :: Lens.Lens' ListConstraintsForPortfolioResponse Core.Int
listConstraintsForPortfolioResponse_httpStatus = Lens.lens (\ListConstraintsForPortfolioResponse' {httpStatus} -> httpStatus) (\s@ListConstraintsForPortfolioResponse' {} a -> s {httpStatus = a} :: ListConstraintsForPortfolioResponse)

instance
  Core.NFData
    ListConstraintsForPortfolioResponse
