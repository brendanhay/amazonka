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
-- Module      : Amazonka.ServiceCatalog.ListPrincipalsForPortfolio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all @PrincipalARN@s and corresponding @PrincipalType@s associated
-- with the specified portfolio.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListPrincipalsForPortfolio
  ( -- * Creating a Request
    ListPrincipalsForPortfolio (..),
    newListPrincipalsForPortfolio,

    -- * Request Lenses
    listPrincipalsForPortfolio_acceptLanguage,
    listPrincipalsForPortfolio_pageSize,
    listPrincipalsForPortfolio_pageToken,
    listPrincipalsForPortfolio_portfolioId,

    -- * Destructuring the Response
    ListPrincipalsForPortfolioResponse (..),
    newListPrincipalsForPortfolioResponse,

    -- * Response Lenses
    listPrincipalsForPortfolioResponse_nextPageToken,
    listPrincipalsForPortfolioResponse_principals,
    listPrincipalsForPortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListPrincipalsForPortfolio' smart constructor.
data ListPrincipalsForPortfolio = ListPrincipalsForPortfolio'
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
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrincipalsForPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'listPrincipalsForPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pageSize', 'listPrincipalsForPortfolio_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listPrincipalsForPortfolio_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'portfolioId', 'listPrincipalsForPortfolio_portfolioId' - The portfolio identifier.
newListPrincipalsForPortfolio ::
  -- | 'portfolioId'
  Prelude.Text ->
  ListPrincipalsForPortfolio
newListPrincipalsForPortfolio pPortfolioId_ =
  ListPrincipalsForPortfolio'
    { acceptLanguage =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPrincipalsForPortfolio_acceptLanguage :: Lens.Lens' ListPrincipalsForPortfolio (Prelude.Maybe Prelude.Text)
listPrincipalsForPortfolio_acceptLanguage = Lens.lens (\ListPrincipalsForPortfolio' {acceptLanguage} -> acceptLanguage) (\s@ListPrincipalsForPortfolio' {} a -> s {acceptLanguage = a} :: ListPrincipalsForPortfolio)

-- | The maximum number of items to return with this call.
listPrincipalsForPortfolio_pageSize :: Lens.Lens' ListPrincipalsForPortfolio (Prelude.Maybe Prelude.Natural)
listPrincipalsForPortfolio_pageSize = Lens.lens (\ListPrincipalsForPortfolio' {pageSize} -> pageSize) (\s@ListPrincipalsForPortfolio' {} a -> s {pageSize = a} :: ListPrincipalsForPortfolio)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPrincipalsForPortfolio_pageToken :: Lens.Lens' ListPrincipalsForPortfolio (Prelude.Maybe Prelude.Text)
listPrincipalsForPortfolio_pageToken = Lens.lens (\ListPrincipalsForPortfolio' {pageToken} -> pageToken) (\s@ListPrincipalsForPortfolio' {} a -> s {pageToken = a} :: ListPrincipalsForPortfolio)

-- | The portfolio identifier.
listPrincipalsForPortfolio_portfolioId :: Lens.Lens' ListPrincipalsForPortfolio Prelude.Text
listPrincipalsForPortfolio_portfolioId = Lens.lens (\ListPrincipalsForPortfolio' {portfolioId} -> portfolioId) (\s@ListPrincipalsForPortfolio' {} a -> s {portfolioId = a} :: ListPrincipalsForPortfolio)

instance Core.AWSPager ListPrincipalsForPortfolio where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPrincipalsForPortfolioResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPrincipalsForPortfolioResponse_principals
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPrincipalsForPortfolio_pageToken
          Lens..~ rs
          Lens.^? listPrincipalsForPortfolioResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPrincipalsForPortfolio where
  type
    AWSResponse ListPrincipalsForPortfolio =
      ListPrincipalsForPortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPrincipalsForPortfolioResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (x Data..?> "Principals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPrincipalsForPortfolio where
  hashWithSalt _salt ListPrincipalsForPortfolio' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` portfolioId

instance Prelude.NFData ListPrincipalsForPortfolio where
  rnf ListPrincipalsForPortfolio' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf portfolioId

instance Data.ToHeaders ListPrincipalsForPortfolio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListPrincipalsForPortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPrincipalsForPortfolio where
  toJSON ListPrincipalsForPortfolio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            Prelude.Just ("PortfolioId" Data..= portfolioId)
          ]
      )

instance Data.ToPath ListPrincipalsForPortfolio where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPrincipalsForPortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPrincipalsForPortfolioResponse' smart constructor.
data ListPrincipalsForPortfolioResponse = ListPrincipalsForPortfolioResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The @PrincipalARN@s and corresponding @PrincipalType@s associated with
    -- the portfolio.
    principals :: Prelude.Maybe [Principal],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrincipalsForPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listPrincipalsForPortfolioResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'principals', 'listPrincipalsForPortfolioResponse_principals' - The @PrincipalARN@s and corresponding @PrincipalType@s associated with
-- the portfolio.
--
-- 'httpStatus', 'listPrincipalsForPortfolioResponse_httpStatus' - The response's http status code.
newListPrincipalsForPortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPrincipalsForPortfolioResponse
newListPrincipalsForPortfolioResponse pHttpStatus_ =
  ListPrincipalsForPortfolioResponse'
    { nextPageToken =
        Prelude.Nothing,
      principals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPrincipalsForPortfolioResponse_nextPageToken :: Lens.Lens' ListPrincipalsForPortfolioResponse (Prelude.Maybe Prelude.Text)
listPrincipalsForPortfolioResponse_nextPageToken = Lens.lens (\ListPrincipalsForPortfolioResponse' {nextPageToken} -> nextPageToken) (\s@ListPrincipalsForPortfolioResponse' {} a -> s {nextPageToken = a} :: ListPrincipalsForPortfolioResponse)

-- | The @PrincipalARN@s and corresponding @PrincipalType@s associated with
-- the portfolio.
listPrincipalsForPortfolioResponse_principals :: Lens.Lens' ListPrincipalsForPortfolioResponse (Prelude.Maybe [Principal])
listPrincipalsForPortfolioResponse_principals = Lens.lens (\ListPrincipalsForPortfolioResponse' {principals} -> principals) (\s@ListPrincipalsForPortfolioResponse' {} a -> s {principals = a} :: ListPrincipalsForPortfolioResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPrincipalsForPortfolioResponse_httpStatus :: Lens.Lens' ListPrincipalsForPortfolioResponse Prelude.Int
listPrincipalsForPortfolioResponse_httpStatus = Lens.lens (\ListPrincipalsForPortfolioResponse' {httpStatus} -> httpStatus) (\s@ListPrincipalsForPortfolioResponse' {} a -> s {httpStatus = a} :: ListPrincipalsForPortfolioResponse)

instance
  Prelude.NFData
    ListPrincipalsForPortfolioResponse
  where
  rnf ListPrincipalsForPortfolioResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf httpStatus
