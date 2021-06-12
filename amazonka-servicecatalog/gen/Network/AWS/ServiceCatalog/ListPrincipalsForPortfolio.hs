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
-- Module      : Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all principal ARNs associated with the specified portfolio.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
  ( -- * Creating a Request
    ListPrincipalsForPortfolio (..),
    newListPrincipalsForPortfolio,

    -- * Request Lenses
    listPrincipalsForPortfolio_pageSize,
    listPrincipalsForPortfolio_pageToken,
    listPrincipalsForPortfolio_acceptLanguage,
    listPrincipalsForPortfolio_portfolioId,

    -- * Destructuring the Response
    ListPrincipalsForPortfolioResponse (..),
    newListPrincipalsForPortfolioResponse,

    -- * Response Lenses
    listPrincipalsForPortfolioResponse_principals,
    listPrincipalsForPortfolioResponse_nextPageToken,
    listPrincipalsForPortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListPrincipalsForPortfolio' smart constructor.
data ListPrincipalsForPortfolio = ListPrincipalsForPortfolio'
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
    -- | The portfolio identifier.
    portfolioId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPrincipalsForPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listPrincipalsForPortfolio_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listPrincipalsForPortfolio_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listPrincipalsForPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'listPrincipalsForPortfolio_portfolioId' - The portfolio identifier.
newListPrincipalsForPortfolio ::
  -- | 'portfolioId'
  Core.Text ->
  ListPrincipalsForPortfolio
newListPrincipalsForPortfolio pPortfolioId_ =
  ListPrincipalsForPortfolio'
    { pageSize =
        Core.Nothing,
      pageToken = Core.Nothing,
      acceptLanguage = Core.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The maximum number of items to return with this call.
listPrincipalsForPortfolio_pageSize :: Lens.Lens' ListPrincipalsForPortfolio (Core.Maybe Core.Natural)
listPrincipalsForPortfolio_pageSize = Lens.lens (\ListPrincipalsForPortfolio' {pageSize} -> pageSize) (\s@ListPrincipalsForPortfolio' {} a -> s {pageSize = a} :: ListPrincipalsForPortfolio)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPrincipalsForPortfolio_pageToken :: Lens.Lens' ListPrincipalsForPortfolio (Core.Maybe Core.Text)
listPrincipalsForPortfolio_pageToken = Lens.lens (\ListPrincipalsForPortfolio' {pageToken} -> pageToken) (\s@ListPrincipalsForPortfolio' {} a -> s {pageToken = a} :: ListPrincipalsForPortfolio)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPrincipalsForPortfolio_acceptLanguage :: Lens.Lens' ListPrincipalsForPortfolio (Core.Maybe Core.Text)
listPrincipalsForPortfolio_acceptLanguage = Lens.lens (\ListPrincipalsForPortfolio' {acceptLanguage} -> acceptLanguage) (\s@ListPrincipalsForPortfolio' {} a -> s {acceptLanguage = a} :: ListPrincipalsForPortfolio)

-- | The portfolio identifier.
listPrincipalsForPortfolio_portfolioId :: Lens.Lens' ListPrincipalsForPortfolio Core.Text
listPrincipalsForPortfolio_portfolioId = Lens.lens (\ListPrincipalsForPortfolio' {portfolioId} -> portfolioId) (\s@ListPrincipalsForPortfolio' {} a -> s {portfolioId = a} :: ListPrincipalsForPortfolio)

instance Core.AWSPager ListPrincipalsForPortfolio where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPrincipalsForPortfolioResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPrincipalsForPortfolioResponse_principals
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPrincipalsForPortfolio_pageToken
          Lens..~ rs
          Lens.^? listPrincipalsForPortfolioResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest ListPrincipalsForPortfolio where
  type
    AWSResponse ListPrincipalsForPortfolio =
      ListPrincipalsForPortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPrincipalsForPortfolioResponse'
            Core.<$> (x Core..?> "Principals" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPrincipalsForPortfolio

instance Core.NFData ListPrincipalsForPortfolio

instance Core.ToHeaders ListPrincipalsForPortfolio where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListPrincipalsForPortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPrincipalsForPortfolio where
  toJSON ListPrincipalsForPortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath ListPrincipalsForPortfolio where
  toPath = Core.const "/"

instance Core.ToQuery ListPrincipalsForPortfolio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPrincipalsForPortfolioResponse' smart constructor.
data ListPrincipalsForPortfolioResponse = ListPrincipalsForPortfolioResponse'
  { -- | The IAM principals (users or roles) associated with the portfolio.
    principals :: Core.Maybe [Principal],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPrincipalsForPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principals', 'listPrincipalsForPortfolioResponse_principals' - The IAM principals (users or roles) associated with the portfolio.
--
-- 'nextPageToken', 'listPrincipalsForPortfolioResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listPrincipalsForPortfolioResponse_httpStatus' - The response's http status code.
newListPrincipalsForPortfolioResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPrincipalsForPortfolioResponse
newListPrincipalsForPortfolioResponse pHttpStatus_ =
  ListPrincipalsForPortfolioResponse'
    { principals =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IAM principals (users or roles) associated with the portfolio.
listPrincipalsForPortfolioResponse_principals :: Lens.Lens' ListPrincipalsForPortfolioResponse (Core.Maybe [Principal])
listPrincipalsForPortfolioResponse_principals = Lens.lens (\ListPrincipalsForPortfolioResponse' {principals} -> principals) (\s@ListPrincipalsForPortfolioResponse' {} a -> s {principals = a} :: ListPrincipalsForPortfolioResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPrincipalsForPortfolioResponse_nextPageToken :: Lens.Lens' ListPrincipalsForPortfolioResponse (Core.Maybe Core.Text)
listPrincipalsForPortfolioResponse_nextPageToken = Lens.lens (\ListPrincipalsForPortfolioResponse' {nextPageToken} -> nextPageToken) (\s@ListPrincipalsForPortfolioResponse' {} a -> s {nextPageToken = a} :: ListPrincipalsForPortfolioResponse)

-- | The response's http status code.
listPrincipalsForPortfolioResponse_httpStatus :: Lens.Lens' ListPrincipalsForPortfolioResponse Core.Int
listPrincipalsForPortfolioResponse_httpStatus = Lens.lens (\ListPrincipalsForPortfolioResponse' {httpStatus} -> httpStatus) (\s@ListPrincipalsForPortfolioResponse' {} a -> s {httpStatus = a} :: ListPrincipalsForPortfolioResponse)

instance
  Core.NFData
    ListPrincipalsForPortfolioResponse
