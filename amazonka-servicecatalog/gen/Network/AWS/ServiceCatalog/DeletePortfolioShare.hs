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
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolioShare
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ServiceCatalog.DeletePortfolioShare
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDeletePortfolioShare' smart constructor.
data DeletePortfolioShare = DeletePortfolioShare'
  { -- | The AWS account ID.
    accountId :: Core.Maybe Core.Text,
    -- | The organization node to whom you are going to stop sharing.
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
-- Create a value of 'DeletePortfolioShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deletePortfolioShare_accountId' - The AWS account ID.
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
  Core.Text ->
  DeletePortfolioShare
newDeletePortfolioShare pPortfolioId_ =
  DeletePortfolioShare'
    { accountId = Core.Nothing,
      organizationNode = Core.Nothing,
      acceptLanguage = Core.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The AWS account ID.
deletePortfolioShare_accountId :: Lens.Lens' DeletePortfolioShare (Core.Maybe Core.Text)
deletePortfolioShare_accountId = Lens.lens (\DeletePortfolioShare' {accountId} -> accountId) (\s@DeletePortfolioShare' {} a -> s {accountId = a} :: DeletePortfolioShare)

-- | The organization node to whom you are going to stop sharing.
deletePortfolioShare_organizationNode :: Lens.Lens' DeletePortfolioShare (Core.Maybe OrganizationNode)
deletePortfolioShare_organizationNode = Lens.lens (\DeletePortfolioShare' {organizationNode} -> organizationNode) (\s@DeletePortfolioShare' {} a -> s {organizationNode = a} :: DeletePortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deletePortfolioShare_acceptLanguage :: Lens.Lens' DeletePortfolioShare (Core.Maybe Core.Text)
deletePortfolioShare_acceptLanguage = Lens.lens (\DeletePortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@DeletePortfolioShare' {} a -> s {acceptLanguage = a} :: DeletePortfolioShare)

-- | The portfolio identifier.
deletePortfolioShare_portfolioId :: Lens.Lens' DeletePortfolioShare Core.Text
deletePortfolioShare_portfolioId = Lens.lens (\DeletePortfolioShare' {portfolioId} -> portfolioId) (\s@DeletePortfolioShare' {} a -> s {portfolioId = a} :: DeletePortfolioShare)

instance Core.AWSRequest DeletePortfolioShare where
  type
    AWSResponse DeletePortfolioShare =
      DeletePortfolioShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePortfolioShareResponse'
            Core.<$> (x Core..?> "PortfolioShareToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeletePortfolioShare

instance Core.NFData DeletePortfolioShare

instance Core.ToHeaders DeletePortfolioShare where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DeletePortfolioShare" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePortfolioShare where
  toJSON DeletePortfolioShare' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountId" Core..=) Core.<$> accountId,
            ("OrganizationNode" Core..=)
              Core.<$> organizationNode,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath DeletePortfolioShare where
  toPath = Core.const "/"

instance Core.ToQuery DeletePortfolioShare where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePortfolioShareResponse' smart constructor.
data DeletePortfolioShareResponse = DeletePortfolioShareResponse'
  { -- | The portfolio share unique identifier. This will only be returned if
    -- delete is made to an organization node.
    portfolioShareToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeletePortfolioShareResponse
newDeletePortfolioShareResponse pHttpStatus_ =
  DeletePortfolioShareResponse'
    { portfolioShareToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The portfolio share unique identifier. This will only be returned if
-- delete is made to an organization node.
deletePortfolioShareResponse_portfolioShareToken :: Lens.Lens' DeletePortfolioShareResponse (Core.Maybe Core.Text)
deletePortfolioShareResponse_portfolioShareToken = Lens.lens (\DeletePortfolioShareResponse' {portfolioShareToken} -> portfolioShareToken) (\s@DeletePortfolioShareResponse' {} a -> s {portfolioShareToken = a} :: DeletePortfolioShareResponse)

-- | The response's http status code.
deletePortfolioShareResponse_httpStatus :: Lens.Lens' DeletePortfolioShareResponse Core.Int
deletePortfolioShareResponse_httpStatus = Lens.lens (\DeletePortfolioShareResponse' {httpStatus} -> httpStatus) (\s@DeletePortfolioShareResponse' {} a -> s {httpStatus = a} :: DeletePortfolioShareResponse)

instance Core.NFData DeletePortfolioShareResponse
