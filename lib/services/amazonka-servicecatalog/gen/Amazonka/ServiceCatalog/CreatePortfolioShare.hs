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
-- Module      : Amazonka.ServiceCatalog.CreatePortfolioShare
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ServiceCatalog.CreatePortfolioShare
  ( -- * Creating a Request
    CreatePortfolioShare (..),
    newCreatePortfolioShare,

    -- * Request Lenses
    createPortfolioShare_accountId,
    createPortfolioShare_organizationNode,
    createPortfolioShare_acceptLanguage,
    createPortfolioShare_shareTagOptions,
    createPortfolioShare_portfolioId,

    -- * Destructuring the Response
    CreatePortfolioShareResponse (..),
    newCreatePortfolioShareResponse,

    -- * Response Lenses
    createPortfolioShareResponse_portfolioShareToken,
    createPortfolioShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newCreatePortfolioShare' smart constructor.
data CreatePortfolioShare = CreatePortfolioShare'
  { -- | The Amazon Web Services account ID. For example, @123456789012@.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The organization node to whom you are going to share. When you pass
    -- @OrganizationNode@, it creates @PortfolioShare@ for all of the Amazon
    -- Web Services accounts that are associated to the @OrganizationNode@. The
    -- output returns a @PortfolioShareToken@, which enables the administrator
    -- to monitor the status of the @PortfolioShare@ creation process.
    organizationNode :: Prelude.Maybe OrganizationNode,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables @TagOptions @ sharing when creating the portfolio
    -- share. If this flag is not provided, TagOptions sharing is disabled.
    shareTagOptions :: Prelude.Maybe Prelude.Bool,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePortfolioShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'createPortfolioShare_accountId' - The Amazon Web Services account ID. For example, @123456789012@.
--
-- 'organizationNode', 'createPortfolioShare_organizationNode' - The organization node to whom you are going to share. When you pass
-- @OrganizationNode@, it creates @PortfolioShare@ for all of the Amazon
-- Web Services accounts that are associated to the @OrganizationNode@. The
-- output returns a @PortfolioShareToken@, which enables the administrator
-- to monitor the status of the @PortfolioShare@ creation process.
--
-- 'acceptLanguage', 'createPortfolioShare_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'shareTagOptions', 'createPortfolioShare_shareTagOptions' - Enables or disables @TagOptions @ sharing when creating the portfolio
-- share. If this flag is not provided, TagOptions sharing is disabled.
--
-- 'portfolioId', 'createPortfolioShare_portfolioId' - The portfolio identifier.
newCreatePortfolioShare ::
  -- | 'portfolioId'
  Prelude.Text ->
  CreatePortfolioShare
newCreatePortfolioShare pPortfolioId_ =
  CreatePortfolioShare'
    { accountId = Prelude.Nothing,
      organizationNode = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      shareTagOptions = Prelude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The Amazon Web Services account ID. For example, @123456789012@.
createPortfolioShare_accountId :: Lens.Lens' CreatePortfolioShare (Prelude.Maybe Prelude.Text)
createPortfolioShare_accountId = Lens.lens (\CreatePortfolioShare' {accountId} -> accountId) (\s@CreatePortfolioShare' {} a -> s {accountId = a} :: CreatePortfolioShare)

-- | The organization node to whom you are going to share. When you pass
-- @OrganizationNode@, it creates @PortfolioShare@ for all of the Amazon
-- Web Services accounts that are associated to the @OrganizationNode@. The
-- output returns a @PortfolioShareToken@, which enables the administrator
-- to monitor the status of the @PortfolioShare@ creation process.
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

-- | Enables or disables @TagOptions @ sharing when creating the portfolio
-- share. If this flag is not provided, TagOptions sharing is disabled.
createPortfolioShare_shareTagOptions :: Lens.Lens' CreatePortfolioShare (Prelude.Maybe Prelude.Bool)
createPortfolioShare_shareTagOptions = Lens.lens (\CreatePortfolioShare' {shareTagOptions} -> shareTagOptions) (\s@CreatePortfolioShare' {} a -> s {shareTagOptions = a} :: CreatePortfolioShare)

-- | The portfolio identifier.
createPortfolioShare_portfolioId :: Lens.Lens' CreatePortfolioShare Prelude.Text
createPortfolioShare_portfolioId = Lens.lens (\CreatePortfolioShare' {portfolioId} -> portfolioId) (\s@CreatePortfolioShare' {} a -> s {portfolioId = a} :: CreatePortfolioShare)

instance Core.AWSRequest CreatePortfolioShare where
  type
    AWSResponse CreatePortfolioShare =
      CreatePortfolioShareResponse
  service _ = defaultService
  request srv = Request.postJSON srv
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePortfolioShareResponse'
            Prelude.<$> (x Core..?> "PortfolioShareToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePortfolioShare where
  hashWithSalt _salt CreatePortfolioShare' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` organizationNode
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` shareTagOptions
      `Prelude.hashWithSalt` portfolioId

instance Prelude.NFData CreatePortfolioShare where
  rnf CreatePortfolioShare' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf organizationNode
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf shareTagOptions
      `Prelude.seq` Prelude.rnf portfolioId

instance Core.ToHeaders CreatePortfolioShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CreatePortfolioShare" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePortfolioShare where
  toJSON CreatePortfolioShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccountId" Core..=) Prelude.<$> accountId,
            ("OrganizationNode" Core..=)
              Prelude.<$> organizationNode,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            ("ShareTagOptions" Core..=)
              Prelude.<$> shareTagOptions,
            Prelude.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath CreatePortfolioShare where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePortfolioShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePortfolioShareResponse' smart constructor.
data CreatePortfolioShareResponse = CreatePortfolioShareResponse'
  { -- | The portfolio shares a unique identifier that only returns if the
    -- portfolio is shared to an organization node.
    portfolioShareToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreatePortfolioShareResponse where
  rnf CreatePortfolioShareResponse' {..} =
    Prelude.rnf portfolioShareToken
      `Prelude.seq` Prelude.rnf httpStatus
