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
-- Module      : Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the organization nodes that have access to the specified
-- portfolio. This API can only be called by the management account in the
-- organization or by a delegated admin.
--
-- If a delegated admin is de-registered, they can no longer perform this
-- operation.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListOrganizationPortfolioAccess
  ( -- * Creating a Request
    ListOrganizationPortfolioAccess (..),
    newListOrganizationPortfolioAccess,

    -- * Request Lenses
    listOrganizationPortfolioAccess_pageSize,
    listOrganizationPortfolioAccess_pageToken,
    listOrganizationPortfolioAccess_acceptLanguage,
    listOrganizationPortfolioAccess_portfolioId,
    listOrganizationPortfolioAccess_organizationNodeType,

    -- * Destructuring the Response
    ListOrganizationPortfolioAccessResponse (..),
    newListOrganizationPortfolioAccessResponse,

    -- * Response Lenses
    listOrganizationPortfolioAccessResponse_organizationNodes,
    listOrganizationPortfolioAccessResponse_nextPageToken,
    listOrganizationPortfolioAccessResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListOrganizationPortfolioAccess' smart constructor.
data ListOrganizationPortfolioAccess = ListOrganizationPortfolioAccess'
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
    -- | The portfolio identifier. For example, @port-2abcdext3y5fk@.
    portfolioId :: Core.Text,
    -- | The organization node type that will be returned in the output.
    --
    -- -   @ORGANIZATION@ - Organization that has access to the portfolio.
    --
    -- -   @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the
    --     portfolio within your organization.
    --
    -- -   @ACCOUNT@ - Account that has access to the portfolio within your
    --     organization.
    organizationNodeType :: OrganizationNodeType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOrganizationPortfolioAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listOrganizationPortfolioAccess_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listOrganizationPortfolioAccess_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listOrganizationPortfolioAccess_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'listOrganizationPortfolioAccess_portfolioId' - The portfolio identifier. For example, @port-2abcdext3y5fk@.
--
-- 'organizationNodeType', 'listOrganizationPortfolioAccess_organizationNodeType' - The organization node type that will be returned in the output.
--
-- -   @ORGANIZATION@ - Organization that has access to the portfolio.
--
-- -   @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the
--     portfolio within your organization.
--
-- -   @ACCOUNT@ - Account that has access to the portfolio within your
--     organization.
newListOrganizationPortfolioAccess ::
  -- | 'portfolioId'
  Core.Text ->
  -- | 'organizationNodeType'
  OrganizationNodeType ->
  ListOrganizationPortfolioAccess
newListOrganizationPortfolioAccess
  pPortfolioId_
  pOrganizationNodeType_ =
    ListOrganizationPortfolioAccess'
      { pageSize =
          Core.Nothing,
        pageToken = Core.Nothing,
        acceptLanguage = Core.Nothing,
        portfolioId = pPortfolioId_,
        organizationNodeType =
          pOrganizationNodeType_
      }

-- | The maximum number of items to return with this call.
listOrganizationPortfolioAccess_pageSize :: Lens.Lens' ListOrganizationPortfolioAccess (Core.Maybe Core.Natural)
listOrganizationPortfolioAccess_pageSize = Lens.lens (\ListOrganizationPortfolioAccess' {pageSize} -> pageSize) (\s@ListOrganizationPortfolioAccess' {} a -> s {pageSize = a} :: ListOrganizationPortfolioAccess)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listOrganizationPortfolioAccess_pageToken :: Lens.Lens' ListOrganizationPortfolioAccess (Core.Maybe Core.Text)
listOrganizationPortfolioAccess_pageToken = Lens.lens (\ListOrganizationPortfolioAccess' {pageToken} -> pageToken) (\s@ListOrganizationPortfolioAccess' {} a -> s {pageToken = a} :: ListOrganizationPortfolioAccess)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listOrganizationPortfolioAccess_acceptLanguage :: Lens.Lens' ListOrganizationPortfolioAccess (Core.Maybe Core.Text)
listOrganizationPortfolioAccess_acceptLanguage = Lens.lens (\ListOrganizationPortfolioAccess' {acceptLanguage} -> acceptLanguage) (\s@ListOrganizationPortfolioAccess' {} a -> s {acceptLanguage = a} :: ListOrganizationPortfolioAccess)

-- | The portfolio identifier. For example, @port-2abcdext3y5fk@.
listOrganizationPortfolioAccess_portfolioId :: Lens.Lens' ListOrganizationPortfolioAccess Core.Text
listOrganizationPortfolioAccess_portfolioId = Lens.lens (\ListOrganizationPortfolioAccess' {portfolioId} -> portfolioId) (\s@ListOrganizationPortfolioAccess' {} a -> s {portfolioId = a} :: ListOrganizationPortfolioAccess)

-- | The organization node type that will be returned in the output.
--
-- -   @ORGANIZATION@ - Organization that has access to the portfolio.
--
-- -   @ORGANIZATIONAL_UNIT@ - Organizational unit that has access to the
--     portfolio within your organization.
--
-- -   @ACCOUNT@ - Account that has access to the portfolio within your
--     organization.
listOrganizationPortfolioAccess_organizationNodeType :: Lens.Lens' ListOrganizationPortfolioAccess OrganizationNodeType
listOrganizationPortfolioAccess_organizationNodeType = Lens.lens (\ListOrganizationPortfolioAccess' {organizationNodeType} -> organizationNodeType) (\s@ListOrganizationPortfolioAccess' {} a -> s {organizationNodeType = a} :: ListOrganizationPortfolioAccess)

instance
  Core.AWSPager
    ListOrganizationPortfolioAccess
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOrganizationPortfolioAccessResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOrganizationPortfolioAccessResponse_organizationNodes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOrganizationPortfolioAccess_pageToken
          Lens..~ rs
          Lens.^? listOrganizationPortfolioAccessResponse_nextPageToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListOrganizationPortfolioAccess
  where
  type
    AWSResponse ListOrganizationPortfolioAccess =
      ListOrganizationPortfolioAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationPortfolioAccessResponse'
            Core.<$> (x Core..?> "OrganizationNodes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListOrganizationPortfolioAccess

instance Core.NFData ListOrganizationPortfolioAccess

instance
  Core.ToHeaders
    ListOrganizationPortfolioAccess
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListOrganizationPortfolioAccess" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOrganizationPortfolioAccess where
  toJSON ListOrganizationPortfolioAccess' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId),
            Core.Just
              ( "OrganizationNodeType"
                  Core..= organizationNodeType
              )
          ]
      )

instance Core.ToPath ListOrganizationPortfolioAccess where
  toPath = Core.const "/"

instance Core.ToQuery ListOrganizationPortfolioAccess where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListOrganizationPortfolioAccessResponse' smart constructor.
data ListOrganizationPortfolioAccessResponse = ListOrganizationPortfolioAccessResponse'
  { -- | Displays information about the organization nodes.
    organizationNodes :: Core.Maybe [OrganizationNode],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOrganizationPortfolioAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationNodes', 'listOrganizationPortfolioAccessResponse_organizationNodes' - Displays information about the organization nodes.
--
-- 'nextPageToken', 'listOrganizationPortfolioAccessResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'listOrganizationPortfolioAccessResponse_httpStatus' - The response's http status code.
newListOrganizationPortfolioAccessResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOrganizationPortfolioAccessResponse
newListOrganizationPortfolioAccessResponse
  pHttpStatus_ =
    ListOrganizationPortfolioAccessResponse'
      { organizationNodes =
          Core.Nothing,
        nextPageToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Displays information about the organization nodes.
listOrganizationPortfolioAccessResponse_organizationNodes :: Lens.Lens' ListOrganizationPortfolioAccessResponse (Core.Maybe [OrganizationNode])
listOrganizationPortfolioAccessResponse_organizationNodes = Lens.lens (\ListOrganizationPortfolioAccessResponse' {organizationNodes} -> organizationNodes) (\s@ListOrganizationPortfolioAccessResponse' {} a -> s {organizationNodes = a} :: ListOrganizationPortfolioAccessResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listOrganizationPortfolioAccessResponse_nextPageToken :: Lens.Lens' ListOrganizationPortfolioAccessResponse (Core.Maybe Core.Text)
listOrganizationPortfolioAccessResponse_nextPageToken = Lens.lens (\ListOrganizationPortfolioAccessResponse' {nextPageToken} -> nextPageToken) (\s@ListOrganizationPortfolioAccessResponse' {} a -> s {nextPageToken = a} :: ListOrganizationPortfolioAccessResponse)

-- | The response's http status code.
listOrganizationPortfolioAccessResponse_httpStatus :: Lens.Lens' ListOrganizationPortfolioAccessResponse Core.Int
listOrganizationPortfolioAccessResponse_httpStatus = Lens.lens (\ListOrganizationPortfolioAccessResponse' {httpStatus} -> httpStatus) (\s@ListOrganizationPortfolioAccessResponse' {} a -> s {httpStatus = a} :: ListOrganizationPortfolioAccessResponse)

instance
  Core.NFData
    ListOrganizationPortfolioAccessResponse
