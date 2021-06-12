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
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolioShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares the specified portfolio with the specified account or
-- organization node. Shares to an organization node can only be created by
-- the management account of an organization or by a delegated
-- administrator. You can share portfolios to an organization, an
-- organizational unit, or a specific account.
--
-- Note that if a delegated admin is de-registered, they can no longer
-- create portfolio shares.
--
-- @AWSOrganizationsAccess@ must be enabled in order to create a portfolio
-- share to an organization node.
--
-- You can\'t share a shared resource, including portfolios that contain a
-- shared product.
--
-- If the portfolio share with the specified account or organization node
-- already exists, this action will have no effect and will not return an
-- error. To update an existing share, you must use the
-- @ UpdatePortfolioShare@ API instead.
module Network.AWS.ServiceCatalog.CreatePortfolioShare
  ( -- * Creating a Request
    CreatePortfolioShare (..),
    newCreatePortfolioShare,

    -- * Request Lenses
    createPortfolioShare_shareTagOptions,
    createPortfolioShare_accountId,
    createPortfolioShare_organizationNode,
    createPortfolioShare_acceptLanguage,
    createPortfolioShare_portfolioId,

    -- * Destructuring the Response
    CreatePortfolioShareResponse (..),
    newCreatePortfolioShareResponse,

    -- * Response Lenses
    createPortfolioShareResponse_portfolioShareToken,
    createPortfolioShareResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreatePortfolioShare' smart constructor.
data CreatePortfolioShare = CreatePortfolioShare'
  { -- | Enables or disables @TagOptions @ sharing when creating the portfolio
    -- share. If this flag is not provided, TagOptions sharing is disabled.
    shareTagOptions :: Core.Maybe Core.Bool,
    -- | The AWS account ID. For example, @123456789012@.
    accountId :: Core.Maybe Core.Text,
    -- | The organization node to whom you are going to share. If
    -- @OrganizationNode@ is passed in, @PortfolioShare@ will be created for
    -- the node an ListOrganizationPortfolioAccessd its children (when
    -- applies), and a @PortfolioShareToken@ will be returned in the output in
    -- order for the administrator to monitor the status of the
    -- @PortfolioShare@ creation process.
    organizationNode :: Core.Maybe OrganizationNode,
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
-- Create a value of 'CreatePortfolioShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareTagOptions', 'createPortfolioShare_shareTagOptions' - Enables or disables @TagOptions @ sharing when creating the portfolio
-- share. If this flag is not provided, TagOptions sharing is disabled.
--
-- 'accountId', 'createPortfolioShare_accountId' - The AWS account ID. For example, @123456789012@.
--
-- 'organizationNode', 'createPortfolioShare_organizationNode' - The organization node to whom you are going to share. If
-- @OrganizationNode@ is passed in, @PortfolioShare@ will be created for
-- the node an ListOrganizationPortfolioAccessd its children (when
-- applies), and a @PortfolioShareToken@ will be returned in the output in
-- order for the administrator to monitor the status of the
-- @PortfolioShare@ creation process.
--
-- 'acceptLanguage', 'createPortfolioShare_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'createPortfolioShare_portfolioId' - The portfolio identifier.
newCreatePortfolioShare ::
  -- | 'portfolioId'
  Core.Text ->
  CreatePortfolioShare
newCreatePortfolioShare pPortfolioId_ =
  CreatePortfolioShare'
    { shareTagOptions =
        Core.Nothing,
      accountId = Core.Nothing,
      organizationNode = Core.Nothing,
      acceptLanguage = Core.Nothing,
      portfolioId = pPortfolioId_
    }

-- | Enables or disables @TagOptions @ sharing when creating the portfolio
-- share. If this flag is not provided, TagOptions sharing is disabled.
createPortfolioShare_shareTagOptions :: Lens.Lens' CreatePortfolioShare (Core.Maybe Core.Bool)
createPortfolioShare_shareTagOptions = Lens.lens (\CreatePortfolioShare' {shareTagOptions} -> shareTagOptions) (\s@CreatePortfolioShare' {} a -> s {shareTagOptions = a} :: CreatePortfolioShare)

-- | The AWS account ID. For example, @123456789012@.
createPortfolioShare_accountId :: Lens.Lens' CreatePortfolioShare (Core.Maybe Core.Text)
createPortfolioShare_accountId = Lens.lens (\CreatePortfolioShare' {accountId} -> accountId) (\s@CreatePortfolioShare' {} a -> s {accountId = a} :: CreatePortfolioShare)

-- | The organization node to whom you are going to share. If
-- @OrganizationNode@ is passed in, @PortfolioShare@ will be created for
-- the node an ListOrganizationPortfolioAccessd its children (when
-- applies), and a @PortfolioShareToken@ will be returned in the output in
-- order for the administrator to monitor the status of the
-- @PortfolioShare@ creation process.
createPortfolioShare_organizationNode :: Lens.Lens' CreatePortfolioShare (Core.Maybe OrganizationNode)
createPortfolioShare_organizationNode = Lens.lens (\CreatePortfolioShare' {organizationNode} -> organizationNode) (\s@CreatePortfolioShare' {} a -> s {organizationNode = a} :: CreatePortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createPortfolioShare_acceptLanguage :: Lens.Lens' CreatePortfolioShare (Core.Maybe Core.Text)
createPortfolioShare_acceptLanguage = Lens.lens (\CreatePortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@CreatePortfolioShare' {} a -> s {acceptLanguage = a} :: CreatePortfolioShare)

-- | The portfolio identifier.
createPortfolioShare_portfolioId :: Lens.Lens' CreatePortfolioShare Core.Text
createPortfolioShare_portfolioId = Lens.lens (\CreatePortfolioShare' {portfolioId} -> portfolioId) (\s@CreatePortfolioShare' {} a -> s {portfolioId = a} :: CreatePortfolioShare)

instance Core.AWSRequest CreatePortfolioShare where
  type
    AWSResponse CreatePortfolioShare =
      CreatePortfolioShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePortfolioShareResponse'
            Core.<$> (x Core..?> "PortfolioShareToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePortfolioShare

instance Core.NFData CreatePortfolioShare

instance Core.ToHeaders CreatePortfolioShare where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CreatePortfolioShare" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePortfolioShare where
  toJSON CreatePortfolioShare' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ShareTagOptions" Core..=)
              Core.<$> shareTagOptions,
            ("AccountId" Core..=) Core.<$> accountId,
            ("OrganizationNode" Core..=)
              Core.<$> organizationNode,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath CreatePortfolioShare where
  toPath = Core.const "/"

instance Core.ToQuery CreatePortfolioShare where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePortfolioShareResponse' smart constructor.
data CreatePortfolioShareResponse = CreatePortfolioShareResponse'
  { -- | The portfolio shares a unique identifier that only returns if the
    -- portfolio is shared to an organization node.
    portfolioShareToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePortfolioShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioShareToken', 'createPortfolioShareResponse_portfolioShareToken' - The portfolio shares a unique identifier that only returns if the
-- portfolio is shared to an organization node.
--
-- 'httpStatus', 'createPortfolioShareResponse_httpStatus' - The response's http status code.
newCreatePortfolioShareResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePortfolioShareResponse
newCreatePortfolioShareResponse pHttpStatus_ =
  CreatePortfolioShareResponse'
    { portfolioShareToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The portfolio shares a unique identifier that only returns if the
-- portfolio is shared to an organization node.
createPortfolioShareResponse_portfolioShareToken :: Lens.Lens' CreatePortfolioShareResponse (Core.Maybe Core.Text)
createPortfolioShareResponse_portfolioShareToken = Lens.lens (\CreatePortfolioShareResponse' {portfolioShareToken} -> portfolioShareToken) (\s@CreatePortfolioShareResponse' {} a -> s {portfolioShareToken = a} :: CreatePortfolioShareResponse)

-- | The response's http status code.
createPortfolioShareResponse_httpStatus :: Lens.Lens' CreatePortfolioShareResponse Core.Int
createPortfolioShareResponse_httpStatus = Lens.lens (\CreatePortfolioShareResponse' {httpStatus} -> httpStatus) (\s@CreatePortfolioShareResponse' {} a -> s {httpStatus = a} :: CreatePortfolioShareResponse)

instance Core.NFData CreatePortfolioShareResponse
