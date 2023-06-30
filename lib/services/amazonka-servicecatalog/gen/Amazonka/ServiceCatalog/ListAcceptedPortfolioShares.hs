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
-- Module      : Amazonka.ServiceCatalog.ListAcceptedPortfolioShares
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all imported portfolios for which account-to-account shares were
-- accepted by this account. By specifying the @PortfolioShareType@, you
-- can list portfolios for which organizational shares were accepted by
-- this account.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListAcceptedPortfolioShares
  ( -- * Creating a Request
    ListAcceptedPortfolioShares (..),
    newListAcceptedPortfolioShares,

    -- * Request Lenses
    listAcceptedPortfolioShares_acceptLanguage,
    listAcceptedPortfolioShares_pageSize,
    listAcceptedPortfolioShares_pageToken,
    listAcceptedPortfolioShares_portfolioShareType,

    -- * Destructuring the Response
    ListAcceptedPortfolioSharesResponse (..),
    newListAcceptedPortfolioSharesResponse,

    -- * Response Lenses
    listAcceptedPortfolioSharesResponse_nextPageToken,
    listAcceptedPortfolioSharesResponse_portfolioDetails,
    listAcceptedPortfolioSharesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListAcceptedPortfolioShares' smart constructor.
data ListAcceptedPortfolioShares = ListAcceptedPortfolioShares'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The type of shared portfolios to list. The default is to list imported
    -- portfolios.
    --
    -- -   @AWS_ORGANIZATIONS@ - List portfolios accepted and shared via
    --     organizational sharing by the management account or delegated
    --     administrator of your organization.
    --
    -- -   @AWS_SERVICECATALOG@ - Deprecated type.
    --
    -- -   @IMPORTED@ - List imported portfolios that have been accepted and
    --     shared through account-to-account sharing.
    portfolioShareType :: Prelude.Maybe PortfolioShareType
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
-- 'acceptLanguage', 'listAcceptedPortfolioShares_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pageSize', 'listAcceptedPortfolioShares_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listAcceptedPortfolioShares_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'portfolioShareType', 'listAcceptedPortfolioShares_portfolioShareType' - The type of shared portfolios to list. The default is to list imported
-- portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - List portfolios accepted and shared via
--     organizational sharing by the management account or delegated
--     administrator of your organization.
--
-- -   @AWS_SERVICECATALOG@ - Deprecated type.
--
-- -   @IMPORTED@ - List imported portfolios that have been accepted and
--     shared through account-to-account sharing.
newListAcceptedPortfolioShares ::
  ListAcceptedPortfolioShares
newListAcceptedPortfolioShares =
  ListAcceptedPortfolioShares'
    { acceptLanguage =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      portfolioShareType = Prelude.Nothing
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listAcceptedPortfolioShares_acceptLanguage :: Lens.Lens' ListAcceptedPortfolioShares (Prelude.Maybe Prelude.Text)
listAcceptedPortfolioShares_acceptLanguage = Lens.lens (\ListAcceptedPortfolioShares' {acceptLanguage} -> acceptLanguage) (\s@ListAcceptedPortfolioShares' {} a -> s {acceptLanguage = a} :: ListAcceptedPortfolioShares)

-- | The maximum number of items to return with this call.
listAcceptedPortfolioShares_pageSize :: Lens.Lens' ListAcceptedPortfolioShares (Prelude.Maybe Prelude.Natural)
listAcceptedPortfolioShares_pageSize = Lens.lens (\ListAcceptedPortfolioShares' {pageSize} -> pageSize) (\s@ListAcceptedPortfolioShares' {} a -> s {pageSize = a} :: ListAcceptedPortfolioShares)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listAcceptedPortfolioShares_pageToken :: Lens.Lens' ListAcceptedPortfolioShares (Prelude.Maybe Prelude.Text)
listAcceptedPortfolioShares_pageToken = Lens.lens (\ListAcceptedPortfolioShares' {pageToken} -> pageToken) (\s@ListAcceptedPortfolioShares' {} a -> s {pageToken = a} :: ListAcceptedPortfolioShares)

-- | The type of shared portfolios to list. The default is to list imported
-- portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - List portfolios accepted and shared via
--     organizational sharing by the management account or delegated
--     administrator of your organization.
--
-- -   @AWS_SERVICECATALOG@ - Deprecated type.
--
-- -   @IMPORTED@ - List imported portfolios that have been accepted and
--     shared through account-to-account sharing.
listAcceptedPortfolioShares_portfolioShareType :: Lens.Lens' ListAcceptedPortfolioShares (Prelude.Maybe PortfolioShareType)
listAcceptedPortfolioShares_portfolioShareType = Lens.lens (\ListAcceptedPortfolioShares' {portfolioShareType} -> portfolioShareType) (\s@ListAcceptedPortfolioShares' {} a -> s {portfolioShareType = a} :: ListAcceptedPortfolioShares)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAcceptedPortfolioShares_pageToken
          Lens..~ rs
          Lens.^? listAcceptedPortfolioSharesResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAcceptedPortfolioShares where
  type
    AWSResponse ListAcceptedPortfolioShares =
      ListAcceptedPortfolioSharesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAcceptedPortfolioSharesResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> ( x
                            Data..?> "PortfolioDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAcceptedPortfolioShares where
  hashWithSalt _salt ListAcceptedPortfolioShares' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` portfolioShareType

instance Prelude.NFData ListAcceptedPortfolioShares where
  rnf ListAcceptedPortfolioShares' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf portfolioShareType

instance Data.ToHeaders ListAcceptedPortfolioShares where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListAcceptedPortfolioShares" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAcceptedPortfolioShares where
  toJSON ListAcceptedPortfolioShares' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            ("PortfolioShareType" Data..=)
              Prelude.<$> portfolioShareType
          ]
      )

instance Data.ToPath ListAcceptedPortfolioShares where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAcceptedPortfolioShares where
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
  where
  rnf ListAcceptedPortfolioSharesResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf portfolioDetails
      `Prelude.seq` Prelude.rnf httpStatus
