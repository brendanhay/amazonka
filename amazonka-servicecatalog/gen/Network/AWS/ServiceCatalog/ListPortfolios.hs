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
-- Module      : Network.AWS.ServiceCatalog.ListPortfolios
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios in the catalog.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPortfolios
  ( -- * Creating a Request
    ListPortfolios (..),
    newListPortfolios,

    -- * Request Lenses
    listPortfolios_pageSize,
    listPortfolios_pageToken,
    listPortfolios_acceptLanguage,

    -- * Destructuring the Response
    ListPortfoliosResponse (..),
    newListPortfoliosResponse,

    -- * Response Lenses
    listPortfoliosResponse_portfolioDetails,
    listPortfoliosResponse_nextPageToken,
    listPortfoliosResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListPortfolios' smart constructor.
data ListPortfolios = ListPortfolios'
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
    acceptLanguage :: Prelude.Maybe Prelude.Text
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
-- 'pageSize', 'listPortfolios_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listPortfolios_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listPortfolios_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newListPortfolios ::
  ListPortfolios
newListPortfolios =
  ListPortfolios'
    { pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing
    }

-- | The maximum number of items to return with this call.
listPortfolios_pageSize :: Lens.Lens' ListPortfolios (Prelude.Maybe Prelude.Natural)
listPortfolios_pageSize = Lens.lens (\ListPortfolios' {pageSize} -> pageSize) (\s@ListPortfolios' {} a -> s {pageSize = a} :: ListPortfolios)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPortfolios_pageToken :: Lens.Lens' ListPortfolios (Prelude.Maybe Prelude.Text)
listPortfolios_pageToken = Lens.lens (\ListPortfolios' {pageToken} -> pageToken) (\s@ListPortfolios' {} a -> s {pageToken = a} :: ListPortfolios)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPortfolios_acceptLanguage :: Lens.Lens' ListPortfolios (Prelude.Maybe Prelude.Text)
listPortfolios_acceptLanguage = Lens.lens (\ListPortfolios' {acceptLanguage} -> acceptLanguage) (\s@ListPortfolios' {} a -> s {acceptLanguage = a} :: ListPortfolios)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfoliosResponse'
            Prelude.<$> ( x Core..?> "PortfolioDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPortfolios

instance Prelude.NFData ListPortfolios

instance Core.ToHeaders ListPortfolios where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListPortfolios" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPortfolios where
  toJSON ListPortfolios' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage
          ]
      )

instance Core.ToPath ListPortfolios where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPortfolios where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPortfoliosResponse' smart constructor.
data ListPortfoliosResponse = ListPortfoliosResponse'
  { -- | Information about the portfolios.
    portfolioDetails :: Prelude.Maybe [PortfolioDetail],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
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
-- 'portfolioDetails', 'listPortfoliosResponse_portfolioDetails' - Information about the portfolios.
--
-- 'nextPageToken', 'listPortfoliosResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listPortfoliosResponse_httpStatus' - The response's http status code.
newListPortfoliosResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPortfoliosResponse
newListPortfoliosResponse pHttpStatus_ =
  ListPortfoliosResponse'
    { portfolioDetails =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the portfolios.
listPortfoliosResponse_portfolioDetails :: Lens.Lens' ListPortfoliosResponse (Prelude.Maybe [PortfolioDetail])
listPortfoliosResponse_portfolioDetails = Lens.lens (\ListPortfoliosResponse' {portfolioDetails} -> portfolioDetails) (\s@ListPortfoliosResponse' {} a -> s {portfolioDetails = a} :: ListPortfoliosResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPortfoliosResponse_nextPageToken :: Lens.Lens' ListPortfoliosResponse (Prelude.Maybe Prelude.Text)
listPortfoliosResponse_nextPageToken = Lens.lens (\ListPortfoliosResponse' {nextPageToken} -> nextPageToken) (\s@ListPortfoliosResponse' {} a -> s {nextPageToken = a} :: ListPortfoliosResponse)

-- | The response's http status code.
listPortfoliosResponse_httpStatus :: Lens.Lens' ListPortfoliosResponse Prelude.Int
listPortfoliosResponse_httpStatus = Lens.lens (\ListPortfoliosResponse' {httpStatus} -> httpStatus) (\s@ListPortfoliosResponse' {} a -> s {httpStatus = a} :: ListPortfoliosResponse)

instance Prelude.NFData ListPortfoliosResponse
