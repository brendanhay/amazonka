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
-- Module      : Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios for which sharing was accepted by this account.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
  ( -- * Creating a Request
    ListAcceptedPortfolioShares (..),
    newListAcceptedPortfolioShares,

    -- * Request Lenses
    listAcceptedPortfolioShares_portfolioShareType,
    listAcceptedPortfolioShares_pageSize,
    listAcceptedPortfolioShares_pageToken,
    listAcceptedPortfolioShares_acceptLanguage,

    -- * Destructuring the Response
    ListAcceptedPortfolioSharesResponse (..),
    newListAcceptedPortfolioSharesResponse,

    -- * Response Lenses
    listAcceptedPortfolioSharesResponse_portfolioDetails,
    listAcceptedPortfolioSharesResponse_nextPageToken,
    listAcceptedPortfolioSharesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListAcceptedPortfolioShares' smart constructor.
data ListAcceptedPortfolioShares = ListAcceptedPortfolioShares'
  { -- | The type of shared portfolios to list. The default is to list imported
    -- portfolios.
    --
    -- -   @AWS_ORGANIZATIONS@ - List portfolios shared by the management
    --     account of your organization
    --
    -- -   @AWS_SERVICECATALOG@ - List default portfolios
    --
    -- -   @IMPORTED@ - List imported portfolios
    portfolioShareType :: Core.Maybe PortfolioShareType,
    -- | The maximum number of items to return with this call.
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
    acceptLanguage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAcceptedPortfolioShares' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioShareType', 'listAcceptedPortfolioShares_portfolioShareType' - The type of shared portfolios to list. The default is to list imported
-- portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - List portfolios shared by the management
--     account of your organization
--
-- -   @AWS_SERVICECATALOG@ - List default portfolios
--
-- -   @IMPORTED@ - List imported portfolios
--
-- 'pageSize', 'listAcceptedPortfolioShares_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listAcceptedPortfolioShares_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listAcceptedPortfolioShares_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newListAcceptedPortfolioShares ::
  ListAcceptedPortfolioShares
newListAcceptedPortfolioShares =
  ListAcceptedPortfolioShares'
    { portfolioShareType =
        Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The type of shared portfolios to list. The default is to list imported
-- portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - List portfolios shared by the management
--     account of your organization
--
-- -   @AWS_SERVICECATALOG@ - List default portfolios
--
-- -   @IMPORTED@ - List imported portfolios
listAcceptedPortfolioShares_portfolioShareType :: Lens.Lens' ListAcceptedPortfolioShares (Core.Maybe PortfolioShareType)
listAcceptedPortfolioShares_portfolioShareType = Lens.lens (\ListAcceptedPortfolioShares' {portfolioShareType} -> portfolioShareType) (\s@ListAcceptedPortfolioShares' {} a -> s {portfolioShareType = a} :: ListAcceptedPortfolioShares)

-- | The maximum number of items to return with this call.
listAcceptedPortfolioShares_pageSize :: Lens.Lens' ListAcceptedPortfolioShares (Core.Maybe Core.Natural)
listAcceptedPortfolioShares_pageSize = Lens.lens (\ListAcceptedPortfolioShares' {pageSize} -> pageSize) (\s@ListAcceptedPortfolioShares' {} a -> s {pageSize = a} :: ListAcceptedPortfolioShares)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listAcceptedPortfolioShares_pageToken :: Lens.Lens' ListAcceptedPortfolioShares (Core.Maybe Core.Text)
listAcceptedPortfolioShares_pageToken = Lens.lens (\ListAcceptedPortfolioShares' {pageToken} -> pageToken) (\s@ListAcceptedPortfolioShares' {} a -> s {pageToken = a} :: ListAcceptedPortfolioShares)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listAcceptedPortfolioShares_acceptLanguage :: Lens.Lens' ListAcceptedPortfolioShares (Core.Maybe Core.Text)
listAcceptedPortfolioShares_acceptLanguage = Lens.lens (\ListAcceptedPortfolioShares' {acceptLanguage} -> acceptLanguage) (\s@ListAcceptedPortfolioShares' {} a -> s {acceptLanguage = a} :: ListAcceptedPortfolioShares)

instance Core.AWSPager ListAcceptedPortfolioShares where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAcceptedPortfolioSharesResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAcceptedPortfolioSharesResponse_portfolioDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAcceptedPortfolioShares_pageToken
          Lens..~ rs
          Lens.^? listAcceptedPortfolioSharesResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest ListAcceptedPortfolioShares where
  type
    AWSResponse ListAcceptedPortfolioShares =
      ListAcceptedPortfolioSharesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAcceptedPortfolioSharesResponse'
            Core.<$> (x Core..?> "PortfolioDetails" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAcceptedPortfolioShares

instance Core.NFData ListAcceptedPortfolioShares

instance Core.ToHeaders ListAcceptedPortfolioShares where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListAcceptedPortfolioShares" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAcceptedPortfolioShares where
  toJSON ListAcceptedPortfolioShares' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PortfolioShareType" Core..=)
              Core.<$> portfolioShareType,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath ListAcceptedPortfolioShares where
  toPath = Core.const "/"

instance Core.ToQuery ListAcceptedPortfolioShares where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAcceptedPortfolioSharesResponse' smart constructor.
data ListAcceptedPortfolioSharesResponse = ListAcceptedPortfolioSharesResponse'
  { -- | Information about the portfolios.
    portfolioDetails :: Core.Maybe [PortfolioDetail],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAcceptedPortfolioSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioDetails', 'listAcceptedPortfolioSharesResponse_portfolioDetails' - Information about the portfolios.
--
-- 'nextPageToken', 'listAcceptedPortfolioSharesResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listAcceptedPortfolioSharesResponse_httpStatus' - The response's http status code.
newListAcceptedPortfolioSharesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAcceptedPortfolioSharesResponse
newListAcceptedPortfolioSharesResponse pHttpStatus_ =
  ListAcceptedPortfolioSharesResponse'
    { portfolioDetails =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the portfolios.
listAcceptedPortfolioSharesResponse_portfolioDetails :: Lens.Lens' ListAcceptedPortfolioSharesResponse (Core.Maybe [PortfolioDetail])
listAcceptedPortfolioSharesResponse_portfolioDetails = Lens.lens (\ListAcceptedPortfolioSharesResponse' {portfolioDetails} -> portfolioDetails) (\s@ListAcceptedPortfolioSharesResponse' {} a -> s {portfolioDetails = a} :: ListAcceptedPortfolioSharesResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listAcceptedPortfolioSharesResponse_nextPageToken :: Lens.Lens' ListAcceptedPortfolioSharesResponse (Core.Maybe Core.Text)
listAcceptedPortfolioSharesResponse_nextPageToken = Lens.lens (\ListAcceptedPortfolioSharesResponse' {nextPageToken} -> nextPageToken) (\s@ListAcceptedPortfolioSharesResponse' {} a -> s {nextPageToken = a} :: ListAcceptedPortfolioSharesResponse)

-- | The response's http status code.
listAcceptedPortfolioSharesResponse_httpStatus :: Lens.Lens' ListAcceptedPortfolioSharesResponse Core.Int
listAcceptedPortfolioSharesResponse_httpStatus = Lens.lens (\ListAcceptedPortfolioSharesResponse' {httpStatus} -> httpStatus) (\s@ListAcceptedPortfolioSharesResponse' {} a -> s {httpStatus = a} :: ListAcceptedPortfolioSharesResponse)

instance
  Core.NFData
    ListAcceptedPortfolioSharesResponse
