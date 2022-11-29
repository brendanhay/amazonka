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
-- Module      : Amazonka.ServiceCatalog.DeletePortfolioShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops sharing the specified portfolio with the specified account or
-- organization node. Shares to an organization node can only be deleted by
-- the management account of an organization or by a delegated
-- administrator.
--
-- Note that if a delegated admin is de-registered, portfolio shares
-- created from that account are removed.
module Amazonka.ServiceCatalog.DeletePortfolioShare
  ( -- * Creating a Request
    DeletePortfolioShare (..),
    newDeletePortfolioShare,

    -- * Request Lenses
    deletePortfolioShare_accountId,
    deletePortfolioShare_organizationNode,
    deletePortfolioShare_acceptLanguage,
    deletePortfolioShare_portfolioId,

    -- * Destructuring the Response
    DeletePortfolioShareResponse (..),
    newDeletePortfolioShareResponse,

    -- * Response Lenses
    deletePortfolioShareResponse_portfolioShareToken,
    deletePortfolioShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDeletePortfolioShare' smart constructor.
data DeletePortfolioShare = DeletePortfolioShare'
  { -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The organization node to whom you are going to stop sharing.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePortfolioShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deletePortfolioShare_accountId' - The Amazon Web Services account ID.
--
-- 'organizationNode', 'deletePortfolioShare_organizationNode' - The organization node to whom you are going to stop sharing.
--
-- 'acceptLanguage', 'deletePortfolioShare_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'deletePortfolioShare_portfolioId' - The portfolio identifier.
newDeletePortfolioShare ::
  -- | 'portfolioId'
  Prelude.Text ->
  DeletePortfolioShare
newDeletePortfolioShare pPortfolioId_ =
  DeletePortfolioShare'
    { accountId = Prelude.Nothing,
      organizationNode = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The Amazon Web Services account ID.
deletePortfolioShare_accountId :: Lens.Lens' DeletePortfolioShare (Prelude.Maybe Prelude.Text)
deletePortfolioShare_accountId = Lens.lens (\DeletePortfolioShare' {accountId} -> accountId) (\s@DeletePortfolioShare' {} a -> s {accountId = a} :: DeletePortfolioShare)

-- | The organization node to whom you are going to stop sharing.
deletePortfolioShare_organizationNode :: Lens.Lens' DeletePortfolioShare (Prelude.Maybe OrganizationNode)
deletePortfolioShare_organizationNode = Lens.lens (\DeletePortfolioShare' {organizationNode} -> organizationNode) (\s@DeletePortfolioShare' {} a -> s {organizationNode = a} :: DeletePortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deletePortfolioShare_acceptLanguage :: Lens.Lens' DeletePortfolioShare (Prelude.Maybe Prelude.Text)
deletePortfolioShare_acceptLanguage = Lens.lens (\DeletePortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@DeletePortfolioShare' {} a -> s {acceptLanguage = a} :: DeletePortfolioShare)

-- | The portfolio identifier.
deletePortfolioShare_portfolioId :: Lens.Lens' DeletePortfolioShare Prelude.Text
deletePortfolioShare_portfolioId = Lens.lens (\DeletePortfolioShare' {portfolioId} -> portfolioId) (\s@DeletePortfolioShare' {} a -> s {portfolioId = a} :: DeletePortfolioShare)

instance Core.AWSRequest DeletePortfolioShare where
  type
    AWSResponse DeletePortfolioShare =
      DeletePortfolioShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePortfolioShareResponse'
            Prelude.<$> (x Core..?> "PortfolioShareToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePortfolioShare where
  hashWithSalt _salt DeletePortfolioShare' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` organizationNode
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` portfolioId

instance Prelude.NFData DeletePortfolioShare where
  rnf DeletePortfolioShare' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf organizationNode
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf portfolioId

instance Core.ToHeaders DeletePortfolioShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DeletePortfolioShare" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeletePortfolioShare where
  toJSON DeletePortfolioShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccountId" Core..=) Prelude.<$> accountId,
            ("OrganizationNode" Core..=)
              Prelude.<$> organizationNode,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath DeletePortfolioShare where
  toPath = Prelude.const "/"

instance Core.ToQuery DeletePortfolioShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePortfolioShareResponse' smart constructor.
data DeletePortfolioShareResponse = DeletePortfolioShareResponse'
  { -- | The portfolio share unique identifier. This will only be returned if
    -- delete is made to an organization node.
    portfolioShareToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePortfolioShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioShareToken', 'deletePortfolioShareResponse_portfolioShareToken' - The portfolio share unique identifier. This will only be returned if
-- delete is made to an organization node.
--
-- 'httpStatus', 'deletePortfolioShareResponse_httpStatus' - The response's http status code.
newDeletePortfolioShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePortfolioShareResponse
newDeletePortfolioShareResponse pHttpStatus_ =
  DeletePortfolioShareResponse'
    { portfolioShareToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The portfolio share unique identifier. This will only be returned if
-- delete is made to an organization node.
deletePortfolioShareResponse_portfolioShareToken :: Lens.Lens' DeletePortfolioShareResponse (Prelude.Maybe Prelude.Text)
deletePortfolioShareResponse_portfolioShareToken = Lens.lens (\DeletePortfolioShareResponse' {portfolioShareToken} -> portfolioShareToken) (\s@DeletePortfolioShareResponse' {} a -> s {portfolioShareToken = a} :: DeletePortfolioShareResponse)

-- | The response's http status code.
deletePortfolioShareResponse_httpStatus :: Lens.Lens' DeletePortfolioShareResponse Prelude.Int
deletePortfolioShareResponse_httpStatus = Lens.lens (\DeletePortfolioShareResponse' {httpStatus} -> httpStatus) (\s@DeletePortfolioShareResponse' {} a -> s {httpStatus = a} :: DeletePortfolioShareResponse)

instance Prelude.NFData DeletePortfolioShareResponse where
  rnf DeletePortfolioShareResponse' {..} =
    Prelude.rnf portfolioShareToken
      `Prelude.seq` Prelude.rnf httpStatus
