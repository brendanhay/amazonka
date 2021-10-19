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
    listAcceptedPortfolioShares_acceptLanguage,
    listAcceptedPortfolioShares_pageToken,
    listAcceptedPortfolioShares_pageSize,

    -- * Destructuring the Response
    ListAcceptedPortfolioSharesResponse (..),
    newListAcceptedPortfolioSharesResponse,

    -- * Response Lenses
    listAcceptedPortfolioSharesResponse_nextPageToken,
    listAcceptedPortfolioSharesResponse_portfolioDetails,
    listAcceptedPortfolioSharesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    portfolioShareType :: Prelude.Maybe PortfolioShareType,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'acceptLanguage', 'listAcceptedPortfolioShares_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pageToken', 'listAcceptedPortfolioShares_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'pageSize', 'listAcceptedPortfolioShares_pageSize' - The maximum number of items to return with this call.
newListAcceptedPortfolioShares ::
  ListAcceptedPortfolioShares
newListAcceptedPortfolioShares =
  ListAcceptedPortfolioShares'
    { portfolioShareType =
        Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
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
listAcceptedPortfolioShares_portfolioShareType :: Lens.Lens' ListAcceptedPortfolioShares (Prelude.Maybe PortfolioShareType)
listAcceptedPortfolioShares_portfolioShareType = Lens.lens (\ListAcceptedPortfolioShares' {portfolioShareType} -> portfolioShareType) (\s@ListAcceptedPortfolioShares' {} a -> s {portfolioShareType = a} :: ListAcceptedPortfolioShares)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listAcceptedPortfolioShares_acceptLanguage :: Lens.Lens' ListAcceptedPortfolioShares (Prelude.Maybe Prelude.Text)
listAcceptedPortfolioShares_acceptLanguage = Lens.lens (\ListAcceptedPortfolioShares' {acceptLanguage} -> acceptLanguage) (\s@ListAcceptedPortfolioShares' {} a -> s {acceptLanguage = a} :: ListAcceptedPortfolioShares)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listAcceptedPortfolioShares_pageToken :: Lens.Lens' ListAcceptedPortfolioShares (Prelude.Maybe Prelude.Text)
listAcceptedPortfolioShares_pageToken = Lens.lens (\ListAcceptedPortfolioShares' {pageToken} -> pageToken) (\s@ListAcceptedPortfolioShares' {} a -> s {pageToken = a} :: ListAcceptedPortfolioShares)

-- | The maximum number of items to return with this call.
listAcceptedPortfolioShares_pageSize :: Lens.Lens' ListAcceptedPortfolioShares (Prelude.Maybe Prelude.Natural)
listAcceptedPortfolioShares_pageSize = Lens.lens (\ListAcceptedPortfolioShares' {pageSize} -> pageSize) (\s@ListAcceptedPortfolioShares' {} a -> s {pageSize = a} :: ListAcceptedPortfolioShares)

instance Core.AWSPager ListAcceptedPortfolioShares where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAcceptedPortfolioSharesResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAcceptedPortfolioSharesResponse_portfolioDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAcceptedPortfolioShares_pageToken
          Lens..~ rs
          Lens.^? listAcceptedPortfolioSharesResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAcceptedPortfolioShares where
  type
    AWSResponse ListAcceptedPortfolioShares =
      ListAcceptedPortfolioSharesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAcceptedPortfolioSharesResponse'
            Prelude.<$> (x Core..?> "NextPageToken")
            Prelude.<*> ( x Core..?> "PortfolioDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAcceptedPortfolioShares

instance Prelude.NFData ListAcceptedPortfolioShares

instance Core.ToHeaders ListAcceptedPortfolioShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListAcceptedPortfolioShares" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAcceptedPortfolioShares where
  toJSON ListAcceptedPortfolioShares' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PortfolioShareType" Core..=)
              Prelude.<$> portfolioShareType,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("PageSize" Core..=) Prelude.<$> pageSize
          ]
      )

instance Core.ToPath ListAcceptedPortfolioShares where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAcceptedPortfolioShares where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAcceptedPortfolioSharesResponse' smart constructor.
data ListAcceptedPortfolioSharesResponse = ListAcceptedPortfolioSharesResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the portfolios.
    portfolioDetails :: Prelude.Maybe [PortfolioDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAcceptedPortfolioSharesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listAcceptedPortfolioSharesResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'portfolioDetails', 'listAcceptedPortfolioSharesResponse_portfolioDetails' - Information about the portfolios.
--
-- 'httpStatus', 'listAcceptedPortfolioSharesResponse_httpStatus' - The response's http status code.
newListAcceptedPortfolioSharesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAcceptedPortfolioSharesResponse
newListAcceptedPortfolioSharesResponse pHttpStatus_ =
  ListAcceptedPortfolioSharesResponse'
    { nextPageToken =
        Prelude.Nothing,
      portfolioDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listAcceptedPortfolioSharesResponse_nextPageToken :: Lens.Lens' ListAcceptedPortfolioSharesResponse (Prelude.Maybe Prelude.Text)
listAcceptedPortfolioSharesResponse_nextPageToken = Lens.lens (\ListAcceptedPortfolioSharesResponse' {nextPageToken} -> nextPageToken) (\s@ListAcceptedPortfolioSharesResponse' {} a -> s {nextPageToken = a} :: ListAcceptedPortfolioSharesResponse)

-- | Information about the portfolios.
listAcceptedPortfolioSharesResponse_portfolioDetails :: Lens.Lens' ListAcceptedPortfolioSharesResponse (Prelude.Maybe [PortfolioDetail])
listAcceptedPortfolioSharesResponse_portfolioDetails = Lens.lens (\ListAcceptedPortfolioSharesResponse' {portfolioDetails} -> portfolioDetails) (\s@ListAcceptedPortfolioSharesResponse' {} a -> s {portfolioDetails = a} :: ListAcceptedPortfolioSharesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAcceptedPortfolioSharesResponse_httpStatus :: Lens.Lens' ListAcceptedPortfolioSharesResponse Prelude.Int
listAcceptedPortfolioSharesResponse_httpStatus = Lens.lens (\ListAcceptedPortfolioSharesResponse' {httpStatus} -> httpStatus) (\s@ListAcceptedPortfolioSharesResponse' {} a -> s {httpStatus = a} :: ListAcceptedPortfolioSharesResponse)

instance
  Prelude.NFData
    ListAcceptedPortfolioSharesResponse
