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
-- Module      : Amazonka.ServiceCatalog.ListPortfolios
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios in the catalog.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListPortfolios
  ( -- * Creating a Request
    ListPortfolios (..),
    newListPortfolios,

    -- * Request Lenses
    listPortfolios_acceptLanguage,
    listPortfolios_pageSize,
    listPortfolios_pageToken,

    -- * Destructuring the Response
    ListPortfoliosResponse (..),
    newListPortfoliosResponse,

    -- * Response Lenses
    listPortfoliosResponse_nextPageToken,
    listPortfoliosResponse_portfolioDetails,
    listPortfoliosResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListPortfolios' smart constructor.
data ListPortfolios = ListPortfolios'
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
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPortfolios' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'listPortfolios_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pageSize', 'listPortfolios_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listPortfolios_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
newListPortfolios ::
  ListPortfolios
newListPortfolios =
  ListPortfolios'
    { acceptLanguage = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPortfolios_acceptLanguage :: Lens.Lens' ListPortfolios (Prelude.Maybe Prelude.Text)
listPortfolios_acceptLanguage = Lens.lens (\ListPortfolios' {acceptLanguage} -> acceptLanguage) (\s@ListPortfolios' {} a -> s {acceptLanguage = a} :: ListPortfolios)

-- | The maximum number of items to return with this call.
listPortfolios_pageSize :: Lens.Lens' ListPortfolios (Prelude.Maybe Prelude.Natural)
listPortfolios_pageSize = Lens.lens (\ListPortfolios' {pageSize} -> pageSize) (\s@ListPortfolios' {} a -> s {pageSize = a} :: ListPortfolios)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPortfolios_pageToken :: Lens.Lens' ListPortfolios (Prelude.Maybe Prelude.Text)
listPortfolios_pageToken = Lens.lens (\ListPortfolios' {pageToken} -> pageToken) (\s@ListPortfolios' {} a -> s {pageToken = a} :: ListPortfolios)

instance Core.AWSPager ListPortfolios where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPortfoliosResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPortfoliosResponse_portfolioDetails
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPortfolios_pageToken
              Lens..~ rs
              Lens.^? listPortfoliosResponse_nextPageToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPortfolios where
  type
    AWSResponse ListPortfolios =
      ListPortfoliosResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfoliosResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> ( x
                            Data..?> "PortfolioDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPortfolios where
  hashWithSalt _salt ListPortfolios' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken

instance Prelude.NFData ListPortfolios where
  rnf ListPortfolios' {..} =
    Prelude.rnf acceptLanguage `Prelude.seq`
      Prelude.rnf pageSize `Prelude.seq`
        Prelude.rnf pageToken

instance Data.ToHeaders ListPortfolios where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListPortfolios" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPortfolios where
  toJSON ListPortfolios' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken
          ]
      )

instance Data.ToPath ListPortfolios where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPortfolios where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPortfoliosResponse' smart constructor.
data ListPortfoliosResponse = ListPortfoliosResponse'
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
-- Create a value of 'ListPortfoliosResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listPortfoliosResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'portfolioDetails', 'listPortfoliosResponse_portfolioDetails' - Information about the portfolios.
--
-- 'httpStatus', 'listPortfoliosResponse_httpStatus' - The response's http status code.
newListPortfoliosResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPortfoliosResponse
newListPortfoliosResponse pHttpStatus_ =
  ListPortfoliosResponse'
    { nextPageToken =
        Prelude.Nothing,
      portfolioDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPortfoliosResponse_nextPageToken :: Lens.Lens' ListPortfoliosResponse (Prelude.Maybe Prelude.Text)
listPortfoliosResponse_nextPageToken = Lens.lens (\ListPortfoliosResponse' {nextPageToken} -> nextPageToken) (\s@ListPortfoliosResponse' {} a -> s {nextPageToken = a} :: ListPortfoliosResponse)

-- | Information about the portfolios.
listPortfoliosResponse_portfolioDetails :: Lens.Lens' ListPortfoliosResponse (Prelude.Maybe [PortfolioDetail])
listPortfoliosResponse_portfolioDetails = Lens.lens (\ListPortfoliosResponse' {portfolioDetails} -> portfolioDetails) (\s@ListPortfoliosResponse' {} a -> s {portfolioDetails = a} :: ListPortfoliosResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPortfoliosResponse_httpStatus :: Lens.Lens' ListPortfoliosResponse Prelude.Int
listPortfoliosResponse_httpStatus = Lens.lens (\ListPortfoliosResponse' {httpStatus} -> httpStatus) (\s@ListPortfoliosResponse' {} a -> s {httpStatus = a} :: ListPortfoliosResponse)

instance Prelude.NFData ListPortfoliosResponse where
  rnf ListPortfoliosResponse' {..} =
    Prelude.rnf nextPageToken `Prelude.seq`
      Prelude.rnf portfolioDetails `Prelude.seq`
        Prelude.rnf httpStatus
