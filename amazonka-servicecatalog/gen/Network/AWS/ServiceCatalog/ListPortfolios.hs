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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListPortfolios' smart constructor.
data ListPortfolios = ListPortfolios'
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
    acceptLanguage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The maximum number of items to return with this call.
listPortfolios_pageSize :: Lens.Lens' ListPortfolios (Core.Maybe Core.Natural)
listPortfolios_pageSize = Lens.lens (\ListPortfolios' {pageSize} -> pageSize) (\s@ListPortfolios' {} a -> s {pageSize = a} :: ListPortfolios)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPortfolios_pageToken :: Lens.Lens' ListPortfolios (Core.Maybe Core.Text)
listPortfolios_pageToken = Lens.lens (\ListPortfolios' {pageToken} -> pageToken) (\s@ListPortfolios' {} a -> s {pageToken = a} :: ListPortfolios)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPortfolios_acceptLanguage :: Lens.Lens' ListPortfolios (Core.Maybe Core.Text)
listPortfolios_acceptLanguage = Lens.lens (\ListPortfolios' {acceptLanguage} -> acceptLanguage) (\s@ListPortfolios' {} a -> s {acceptLanguage = a} :: ListPortfolios)

instance Core.AWSPager ListPortfolios where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPortfoliosResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPortfoliosResponse_portfolioDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPortfolios_pageToken
          Lens..~ rs
          Lens.^? listPortfoliosResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest ListPortfolios where
  type
    AWSResponse ListPortfolios =
      ListPortfoliosResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfoliosResponse'
            Core.<$> (x Core..?> "PortfolioDetails" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPortfolios

instance Core.NFData ListPortfolios

instance Core.ToHeaders ListPortfolios where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListPortfolios" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPortfolios where
  toJSON ListPortfolios' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath ListPortfolios where
  toPath = Core.const "/"

instance Core.ToQuery ListPortfolios where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPortfoliosResponse' smart constructor.
data ListPortfoliosResponse = ListPortfoliosResponse'
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
  Core.Int ->
  ListPortfoliosResponse
newListPortfoliosResponse pHttpStatus_ =
  ListPortfoliosResponse'
    { portfolioDetails =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the portfolios.
listPortfoliosResponse_portfolioDetails :: Lens.Lens' ListPortfoliosResponse (Core.Maybe [PortfolioDetail])
listPortfoliosResponse_portfolioDetails = Lens.lens (\ListPortfoliosResponse' {portfolioDetails} -> portfolioDetails) (\s@ListPortfoliosResponse' {} a -> s {portfolioDetails = a} :: ListPortfoliosResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPortfoliosResponse_nextPageToken :: Lens.Lens' ListPortfoliosResponse (Core.Maybe Core.Text)
listPortfoliosResponse_nextPageToken = Lens.lens (\ListPortfoliosResponse' {nextPageToken} -> nextPageToken) (\s@ListPortfoliosResponse' {} a -> s {nextPageToken = a} :: ListPortfoliosResponse)

-- | The response's http status code.
listPortfoliosResponse_httpStatus :: Lens.Lens' ListPortfoliosResponse Core.Int
listPortfoliosResponse_httpStatus = Lens.lens (\ListPortfoliosResponse' {httpStatus} -> httpStatus) (\s@ListPortfoliosResponse' {} a -> s {httpStatus = a} :: ListPortfoliosResponse)

instance Core.NFData ListPortfoliosResponse
