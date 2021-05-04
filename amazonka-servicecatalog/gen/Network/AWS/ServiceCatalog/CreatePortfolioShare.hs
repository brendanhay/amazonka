{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreatePortfolioShare' smart constructor.
data CreatePortfolioShare = CreatePortfolioShare'
  { -- | Enables or disables @TagOptions @ sharing when creating the portfolio
    -- share. If this flag is not provided, TagOptions sharing is disabled.
    shareTagOptions :: Prelude.Maybe Prelude.Bool,
    -- | The AWS account ID. For example, @123456789012@.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The organization node to whom you are going to share. If
    -- @OrganizationNode@ is passed in, @PortfolioShare@ will be created for
    -- the node an ListOrganizationPortfolioAccessd its children (when
    -- applies), and a @PortfolioShareToken@ will be returned in the output in
    -- order for the administrator to monitor the status of the
    -- @PortfolioShare@ creation process.
    organizationNode :: Prelude.Maybe OrganizationNode,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreatePortfolioShare
newCreatePortfolioShare pPortfolioId_ =
  CreatePortfolioShare'
    { shareTagOptions =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      organizationNode = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | Enables or disables @TagOptions @ sharing when creating the portfolio
-- share. If this flag is not provided, TagOptions sharing is disabled.
createPortfolioShare_shareTagOptions :: Lens.Lens' CreatePortfolioShare (Prelude.Maybe Prelude.Bool)
createPortfolioShare_shareTagOptions = Lens.lens (\CreatePortfolioShare' {shareTagOptions} -> shareTagOptions) (\s@CreatePortfolioShare' {} a -> s {shareTagOptions = a} :: CreatePortfolioShare)

-- | The AWS account ID. For example, @123456789012@.
createPortfolioShare_accountId :: Lens.Lens' CreatePortfolioShare (Prelude.Maybe Prelude.Text)
createPortfolioShare_accountId = Lens.lens (\CreatePortfolioShare' {accountId} -> accountId) (\s@CreatePortfolioShare' {} a -> s {accountId = a} :: CreatePortfolioShare)

-- | The organization node to whom you are going to share. If
-- @OrganizationNode@ is passed in, @PortfolioShare@ will be created for
-- the node an ListOrganizationPortfolioAccessd its children (when
-- applies), and a @PortfolioShareToken@ will be returned in the output in
-- order for the administrator to monitor the status of the
-- @PortfolioShare@ creation process.
createPortfolioShare_organizationNode :: Lens.Lens' CreatePortfolioShare (Prelude.Maybe OrganizationNode)
createPortfolioShare_organizationNode = Lens.lens (\CreatePortfolioShare' {organizationNode} -> organizationNode) (\s@CreatePortfolioShare' {} a -> s {organizationNode = a} :: CreatePortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createPortfolioShare_acceptLanguage :: Lens.Lens' CreatePortfolioShare (Prelude.Maybe Prelude.Text)
createPortfolioShare_acceptLanguage = Lens.lens (\CreatePortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@CreatePortfolioShare' {} a -> s {acceptLanguage = a} :: CreatePortfolioShare)

-- | The portfolio identifier.
createPortfolioShare_portfolioId :: Lens.Lens' CreatePortfolioShare Prelude.Text
createPortfolioShare_portfolioId = Lens.lens (\CreatePortfolioShare' {portfolioId} -> portfolioId) (\s@CreatePortfolioShare' {} a -> s {portfolioId = a} :: CreatePortfolioShare)

instance Prelude.AWSRequest CreatePortfolioShare where
  type
    Rs CreatePortfolioShare =
      CreatePortfolioShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePortfolioShareResponse'
            Prelude.<$> (x Prelude..?> "PortfolioShareToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePortfolioShare

instance Prelude.NFData CreatePortfolioShare

instance Prelude.ToHeaders CreatePortfolioShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.CreatePortfolioShare" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePortfolioShare where
  toJSON CreatePortfolioShare' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ShareTagOptions" Prelude..=)
              Prelude.<$> shareTagOptions,
            ("AccountId" Prelude..=) Prelude.<$> accountId,
            ("OrganizationNode" Prelude..=)
              Prelude.<$> organizationNode,
            ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PortfolioId" Prelude..= portfolioId)
          ]
      )

instance Prelude.ToPath CreatePortfolioShare where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreatePortfolioShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePortfolioShareResponse' smart constructor.
data CreatePortfolioShareResponse = CreatePortfolioShareResponse'
  { -- | The portfolio shares a unique identifier that only returns if the
    -- portfolio is shared to an organization node.
    portfolioShareToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreatePortfolioShareResponse
newCreatePortfolioShareResponse pHttpStatus_ =
  CreatePortfolioShareResponse'
    { portfolioShareToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The portfolio shares a unique identifier that only returns if the
-- portfolio is shared to an organization node.
createPortfolioShareResponse_portfolioShareToken :: Lens.Lens' CreatePortfolioShareResponse (Prelude.Maybe Prelude.Text)
createPortfolioShareResponse_portfolioShareToken = Lens.lens (\CreatePortfolioShareResponse' {portfolioShareToken} -> portfolioShareToken) (\s@CreatePortfolioShareResponse' {} a -> s {portfolioShareToken = a} :: CreatePortfolioShareResponse)

-- | The response's http status code.
createPortfolioShareResponse_httpStatus :: Lens.Lens' CreatePortfolioShareResponse Prelude.Int
createPortfolioShareResponse_httpStatus = Lens.lens (\CreatePortfolioShareResponse' {httpStatus} -> httpStatus) (\s@CreatePortfolioShareResponse' {} a -> s {httpStatus = a} :: CreatePortfolioShareResponse)

instance Prelude.NFData CreatePortfolioShareResponse
