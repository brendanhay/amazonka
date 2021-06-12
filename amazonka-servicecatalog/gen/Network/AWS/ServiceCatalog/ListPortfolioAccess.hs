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
-- Module      : Network.AWS.ServiceCatalog.ListPortfolioAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account IDs that have access to the specified portfolio.
--
-- A delegated admin can list the accounts that have access to the shared
-- portfolio. Note that if a delegated admin is de-registered, they can no
-- longer perform this operation.
module Network.AWS.ServiceCatalog.ListPortfolioAccess
  ( -- * Creating a Request
    ListPortfolioAccess (..),
    newListPortfolioAccess,

    -- * Request Lenses
    listPortfolioAccess_pageSize,
    listPortfolioAccess_pageToken,
    listPortfolioAccess_organizationParentId,
    listPortfolioAccess_acceptLanguage,
    listPortfolioAccess_portfolioId,

    -- * Destructuring the Response
    ListPortfolioAccessResponse (..),
    newListPortfolioAccessResponse,

    -- * Response Lenses
    listPortfolioAccessResponse_accountIds,
    listPortfolioAccessResponse_nextPageToken,
    listPortfolioAccessResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListPortfolioAccess' smart constructor.
data ListPortfolioAccess = ListPortfolioAccess'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The ID of an organization node the portfolio is shared with. All
    -- children of this node with an inherited portfolio share will be
    -- returned.
    organizationParentId :: Core.Maybe Core.Text,
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
-- Create a value of 'ListPortfolioAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listPortfolioAccess_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listPortfolioAccess_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'organizationParentId', 'listPortfolioAccess_organizationParentId' - The ID of an organization node the portfolio is shared with. All
-- children of this node with an inherited portfolio share will be
-- returned.
--
-- 'acceptLanguage', 'listPortfolioAccess_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'listPortfolioAccess_portfolioId' - The portfolio identifier.
newListPortfolioAccess ::
  -- | 'portfolioId'
  Core.Text ->
  ListPortfolioAccess
newListPortfolioAccess pPortfolioId_ =
  ListPortfolioAccess'
    { pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      organizationParentId = Core.Nothing,
      acceptLanguage = Core.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The maximum number of items to return with this call.
listPortfolioAccess_pageSize :: Lens.Lens' ListPortfolioAccess (Core.Maybe Core.Natural)
listPortfolioAccess_pageSize = Lens.lens (\ListPortfolioAccess' {pageSize} -> pageSize) (\s@ListPortfolioAccess' {} a -> s {pageSize = a} :: ListPortfolioAccess)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPortfolioAccess_pageToken :: Lens.Lens' ListPortfolioAccess (Core.Maybe Core.Text)
listPortfolioAccess_pageToken = Lens.lens (\ListPortfolioAccess' {pageToken} -> pageToken) (\s@ListPortfolioAccess' {} a -> s {pageToken = a} :: ListPortfolioAccess)

-- | The ID of an organization node the portfolio is shared with. All
-- children of this node with an inherited portfolio share will be
-- returned.
listPortfolioAccess_organizationParentId :: Lens.Lens' ListPortfolioAccess (Core.Maybe Core.Text)
listPortfolioAccess_organizationParentId = Lens.lens (\ListPortfolioAccess' {organizationParentId} -> organizationParentId) (\s@ListPortfolioAccess' {} a -> s {organizationParentId = a} :: ListPortfolioAccess)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPortfolioAccess_acceptLanguage :: Lens.Lens' ListPortfolioAccess (Core.Maybe Core.Text)
listPortfolioAccess_acceptLanguage = Lens.lens (\ListPortfolioAccess' {acceptLanguage} -> acceptLanguage) (\s@ListPortfolioAccess' {} a -> s {acceptLanguage = a} :: ListPortfolioAccess)

-- | The portfolio identifier.
listPortfolioAccess_portfolioId :: Lens.Lens' ListPortfolioAccess Core.Text
listPortfolioAccess_portfolioId = Lens.lens (\ListPortfolioAccess' {portfolioId} -> portfolioId) (\s@ListPortfolioAccess' {} a -> s {portfolioId = a} :: ListPortfolioAccess)

instance Core.AWSRequest ListPortfolioAccess where
  type
    AWSResponse ListPortfolioAccess =
      ListPortfolioAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfolioAccessResponse'
            Core.<$> (x Core..?> "AccountIds" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPortfolioAccess

instance Core.NFData ListPortfolioAccess

instance Core.ToHeaders ListPortfolioAccess where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListPortfolioAccess" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPortfolioAccess where
  toJSON ListPortfolioAccess' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("OrganizationParentId" Core..=)
              Core.<$> organizationParentId,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath ListPortfolioAccess where
  toPath = Core.const "/"

instance Core.ToQuery ListPortfolioAccess where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPortfolioAccessResponse' smart constructor.
data ListPortfolioAccessResponse = ListPortfolioAccessResponse'
  { -- | Information about the AWS accounts with access to the portfolio.
    accountIds :: Core.Maybe [Core.Text],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPortfolioAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'listPortfolioAccessResponse_accountIds' - Information about the AWS accounts with access to the portfolio.
--
-- 'nextPageToken', 'listPortfolioAccessResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listPortfolioAccessResponse_httpStatus' - The response's http status code.
newListPortfolioAccessResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPortfolioAccessResponse
newListPortfolioAccessResponse pHttpStatus_ =
  ListPortfolioAccessResponse'
    { accountIds =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the AWS accounts with access to the portfolio.
listPortfolioAccessResponse_accountIds :: Lens.Lens' ListPortfolioAccessResponse (Core.Maybe [Core.Text])
listPortfolioAccessResponse_accountIds = Lens.lens (\ListPortfolioAccessResponse' {accountIds} -> accountIds) (\s@ListPortfolioAccessResponse' {} a -> s {accountIds = a} :: ListPortfolioAccessResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPortfolioAccessResponse_nextPageToken :: Lens.Lens' ListPortfolioAccessResponse (Core.Maybe Core.Text)
listPortfolioAccessResponse_nextPageToken = Lens.lens (\ListPortfolioAccessResponse' {nextPageToken} -> nextPageToken) (\s@ListPortfolioAccessResponse' {} a -> s {nextPageToken = a} :: ListPortfolioAccessResponse)

-- | The response's http status code.
listPortfolioAccessResponse_httpStatus :: Lens.Lens' ListPortfolioAccessResponse Core.Int
listPortfolioAccessResponse_httpStatus = Lens.lens (\ListPortfolioAccessResponse' {httpStatus} -> httpStatus) (\s@ListPortfolioAccessResponse' {} a -> s {httpStatus = a} :: ListPortfolioAccessResponse)

instance Core.NFData ListPortfolioAccessResponse
