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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListPortfolioAccess' smart constructor.
data ListPortfolioAccess = ListPortfolioAccess'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of an organization node the portfolio is shared with. All
    -- children of this node with an inherited portfolio share will be
    -- returned.
    organizationParentId :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListPortfolioAccess
newListPortfolioAccess pPortfolioId_ =
  ListPortfolioAccess'
    { pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      organizationParentId = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The maximum number of items to return with this call.
listPortfolioAccess_pageSize :: Lens.Lens' ListPortfolioAccess (Prelude.Maybe Prelude.Natural)
listPortfolioAccess_pageSize = Lens.lens (\ListPortfolioAccess' {pageSize} -> pageSize) (\s@ListPortfolioAccess' {} a -> s {pageSize = a} :: ListPortfolioAccess)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listPortfolioAccess_pageToken :: Lens.Lens' ListPortfolioAccess (Prelude.Maybe Prelude.Text)
listPortfolioAccess_pageToken = Lens.lens (\ListPortfolioAccess' {pageToken} -> pageToken) (\s@ListPortfolioAccess' {} a -> s {pageToken = a} :: ListPortfolioAccess)

-- | The ID of an organization node the portfolio is shared with. All
-- children of this node with an inherited portfolio share will be
-- returned.
listPortfolioAccess_organizationParentId :: Lens.Lens' ListPortfolioAccess (Prelude.Maybe Prelude.Text)
listPortfolioAccess_organizationParentId = Lens.lens (\ListPortfolioAccess' {organizationParentId} -> organizationParentId) (\s@ListPortfolioAccess' {} a -> s {organizationParentId = a} :: ListPortfolioAccess)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listPortfolioAccess_acceptLanguage :: Lens.Lens' ListPortfolioAccess (Prelude.Maybe Prelude.Text)
listPortfolioAccess_acceptLanguage = Lens.lens (\ListPortfolioAccess' {acceptLanguage} -> acceptLanguage) (\s@ListPortfolioAccess' {} a -> s {acceptLanguage = a} :: ListPortfolioAccess)

-- | The portfolio identifier.
listPortfolioAccess_portfolioId :: Lens.Lens' ListPortfolioAccess Prelude.Text
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
            Prelude.<$> (x Core..?> "AccountIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPortfolioAccess

instance Prelude.NFData ListPortfolioAccess

instance Core.ToHeaders ListPortfolioAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListPortfolioAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPortfolioAccess where
  toJSON ListPortfolioAccess' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("OrganizationParentId" Core..=)
              Prelude.<$> organizationParentId,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath ListPortfolioAccess where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPortfolioAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPortfolioAccessResponse' smart constructor.
data ListPortfolioAccessResponse = ListPortfolioAccessResponse'
  { -- | Information about the AWS accounts with access to the portfolio.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListPortfolioAccessResponse
newListPortfolioAccessResponse pHttpStatus_ =
  ListPortfolioAccessResponse'
    { accountIds =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the AWS accounts with access to the portfolio.
listPortfolioAccessResponse_accountIds :: Lens.Lens' ListPortfolioAccessResponse (Prelude.Maybe [Prelude.Text])
listPortfolioAccessResponse_accountIds = Lens.lens (\ListPortfolioAccessResponse' {accountIds} -> accountIds) (\s@ListPortfolioAccessResponse' {} a -> s {accountIds = a} :: ListPortfolioAccessResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listPortfolioAccessResponse_nextPageToken :: Lens.Lens' ListPortfolioAccessResponse (Prelude.Maybe Prelude.Text)
listPortfolioAccessResponse_nextPageToken = Lens.lens (\ListPortfolioAccessResponse' {nextPageToken} -> nextPageToken) (\s@ListPortfolioAccessResponse' {} a -> s {nextPageToken = a} :: ListPortfolioAccessResponse)

-- | The response's http status code.
listPortfolioAccessResponse_httpStatus :: Lens.Lens' ListPortfolioAccessResponse Prelude.Int
listPortfolioAccessResponse_httpStatus = Lens.lens (\ListPortfolioAccessResponse' {httpStatus} -> httpStatus) (\s@ListPortfolioAccessResponse' {} a -> s {httpStatus = a} :: ListPortfolioAccessResponse)

instance Prelude.NFData ListPortfolioAccessResponse
