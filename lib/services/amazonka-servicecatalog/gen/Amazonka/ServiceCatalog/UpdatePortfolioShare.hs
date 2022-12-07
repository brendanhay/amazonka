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
-- Module      : Amazonka.ServiceCatalog.UpdatePortfolioShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified portfolio share. You can use this API to enable or
-- disable @TagOptions@ sharing or Principal sharing for an existing
-- portfolio share.
--
-- The portfolio share cannot be updated if the @CreatePortfolioShare@
-- operation is @IN_PROGRESS@, as the share is not available to recipient
-- entities. In this case, you must wait for the portfolio share to be
-- COMPLETED.
--
-- You must provide the @accountId@ or organization node in the input, but
-- not both.
--
-- If the portfolio is shared to both an external account and an
-- organization node, and both shares need to be updated, you must invoke
-- @UpdatePortfolioShare@ separately for each share type.
--
-- This API cannot be used for removing the portfolio share. You must use
-- @DeletePortfolioShare@ API for that action.
--
-- When you associate a principal with portfolio, a potential privilege
-- escalation path may occur when that portfolio is then shared with other
-- accounts. For a user in a recipient account who is /not/ an Service
-- Catalog Admin, but still has the ability to create Principals
-- (Users\/Groups\/Roles), that user could create a role that matches a
-- principal name association for the portfolio. Although this user may not
-- know which principal names are associated through Service Catalog, they
-- may be able to guess the user. If this potential escalation path is a
-- concern, then Service Catalog recommends using @PrincipalType@ as @IAM@.
-- With this configuration, the @PrincipalARN@ must already exist in the
-- recipient account before it can be associated.
module Amazonka.ServiceCatalog.UpdatePortfolioShare
  ( -- * Creating a Request
    UpdatePortfolioShare (..),
    newUpdatePortfolioShare,

    -- * Request Lenses
    updatePortfolioShare_accountId,
    updatePortfolioShare_organizationNode,
    updatePortfolioShare_sharePrincipals,
    updatePortfolioShare_acceptLanguage,
    updatePortfolioShare_shareTagOptions,
    updatePortfolioShare_portfolioId,

    -- * Destructuring the Response
    UpdatePortfolioShareResponse (..),
    newUpdatePortfolioShareResponse,

    -- * Response Lenses
    updatePortfolioShareResponse_status,
    updatePortfolioShareResponse_portfolioShareToken,
    updatePortfolioShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newUpdatePortfolioShare' smart constructor.
data UpdatePortfolioShare = UpdatePortfolioShare'
  { -- | The Amazon Web Services account Id of the recipient account. This field
    -- is required when updating an external account to account type share.
    accountId :: Prelude.Maybe Prelude.Text,
    organizationNode :: Prelude.Maybe OrganizationNode,
    -- | A flag to enables or disables @Principals@ sharing in the portfolio. If
    -- this field is not provided, the current state of the @Principals@
    -- sharing on the portfolio share will not be modified.
    sharePrincipals :: Prelude.Maybe Prelude.Bool,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables @TagOptions@ sharing for the portfolio share. If
    -- this field is not provided, the current state of TagOptions sharing on
    -- the portfolio share will not be modified.
    shareTagOptions :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier of the portfolio for which the share will be
    -- updated.
    portfolioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePortfolioShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'updatePortfolioShare_accountId' - The Amazon Web Services account Id of the recipient account. This field
-- is required when updating an external account to account type share.
--
-- 'organizationNode', 'updatePortfolioShare_organizationNode' - Undocumented member.
--
-- 'sharePrincipals', 'updatePortfolioShare_sharePrincipals' - A flag to enables or disables @Principals@ sharing in the portfolio. If
-- this field is not provided, the current state of the @Principals@
-- sharing on the portfolio share will not be modified.
--
-- 'acceptLanguage', 'updatePortfolioShare_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'shareTagOptions', 'updatePortfolioShare_shareTagOptions' - Enables or disables @TagOptions@ sharing for the portfolio share. If
-- this field is not provided, the current state of TagOptions sharing on
-- the portfolio share will not be modified.
--
-- 'portfolioId', 'updatePortfolioShare_portfolioId' - The unique identifier of the portfolio for which the share will be
-- updated.
newUpdatePortfolioShare ::
  -- | 'portfolioId'
  Prelude.Text ->
  UpdatePortfolioShare
newUpdatePortfolioShare pPortfolioId_ =
  UpdatePortfolioShare'
    { accountId = Prelude.Nothing,
      organizationNode = Prelude.Nothing,
      sharePrincipals = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      shareTagOptions = Prelude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The Amazon Web Services account Id of the recipient account. This field
-- is required when updating an external account to account type share.
updatePortfolioShare_accountId :: Lens.Lens' UpdatePortfolioShare (Prelude.Maybe Prelude.Text)
updatePortfolioShare_accountId = Lens.lens (\UpdatePortfolioShare' {accountId} -> accountId) (\s@UpdatePortfolioShare' {} a -> s {accountId = a} :: UpdatePortfolioShare)

-- | Undocumented member.
updatePortfolioShare_organizationNode :: Lens.Lens' UpdatePortfolioShare (Prelude.Maybe OrganizationNode)
updatePortfolioShare_organizationNode = Lens.lens (\UpdatePortfolioShare' {organizationNode} -> organizationNode) (\s@UpdatePortfolioShare' {} a -> s {organizationNode = a} :: UpdatePortfolioShare)

-- | A flag to enables or disables @Principals@ sharing in the portfolio. If
-- this field is not provided, the current state of the @Principals@
-- sharing on the portfolio share will not be modified.
updatePortfolioShare_sharePrincipals :: Lens.Lens' UpdatePortfolioShare (Prelude.Maybe Prelude.Bool)
updatePortfolioShare_sharePrincipals = Lens.lens (\UpdatePortfolioShare' {sharePrincipals} -> sharePrincipals) (\s@UpdatePortfolioShare' {} a -> s {sharePrincipals = a} :: UpdatePortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updatePortfolioShare_acceptLanguage :: Lens.Lens' UpdatePortfolioShare (Prelude.Maybe Prelude.Text)
updatePortfolioShare_acceptLanguage = Lens.lens (\UpdatePortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@UpdatePortfolioShare' {} a -> s {acceptLanguage = a} :: UpdatePortfolioShare)

-- | Enables or disables @TagOptions@ sharing for the portfolio share. If
-- this field is not provided, the current state of TagOptions sharing on
-- the portfolio share will not be modified.
updatePortfolioShare_shareTagOptions :: Lens.Lens' UpdatePortfolioShare (Prelude.Maybe Prelude.Bool)
updatePortfolioShare_shareTagOptions = Lens.lens (\UpdatePortfolioShare' {shareTagOptions} -> shareTagOptions) (\s@UpdatePortfolioShare' {} a -> s {shareTagOptions = a} :: UpdatePortfolioShare)

-- | The unique identifier of the portfolio for which the share will be
-- updated.
updatePortfolioShare_portfolioId :: Lens.Lens' UpdatePortfolioShare Prelude.Text
updatePortfolioShare_portfolioId = Lens.lens (\UpdatePortfolioShare' {portfolioId} -> portfolioId) (\s@UpdatePortfolioShare' {} a -> s {portfolioId = a} :: UpdatePortfolioShare)

instance Core.AWSRequest UpdatePortfolioShare where
  type
    AWSResponse UpdatePortfolioShare =
      UpdatePortfolioShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePortfolioShareResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "PortfolioShareToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePortfolioShare where
  hashWithSalt _salt UpdatePortfolioShare' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` organizationNode
      `Prelude.hashWithSalt` sharePrincipals
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` shareTagOptions
      `Prelude.hashWithSalt` portfolioId

instance Prelude.NFData UpdatePortfolioShare where
  rnf UpdatePortfolioShare' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf organizationNode
      `Prelude.seq` Prelude.rnf sharePrincipals
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf shareTagOptions
      `Prelude.seq` Prelude.rnf portfolioId

instance Data.ToHeaders UpdatePortfolioShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.UpdatePortfolioShare" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePortfolioShare where
  toJSON UpdatePortfolioShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("OrganizationNode" Data..=)
              Prelude.<$> organizationNode,
            ("SharePrincipals" Data..=)
              Prelude.<$> sharePrincipals,
            ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("ShareTagOptions" Data..=)
              Prelude.<$> shareTagOptions,
            Prelude.Just ("PortfolioId" Data..= portfolioId)
          ]
      )

instance Data.ToPath UpdatePortfolioShare where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePortfolioShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePortfolioShareResponse' smart constructor.
data UpdatePortfolioShareResponse = UpdatePortfolioShareResponse'
  { -- | The status of @UpdatePortfolioShare@ operation. You can also obtain the
    -- operation status using @DescribePortfolioShareStatus@ API.
    status :: Prelude.Maybe ShareStatus,
    -- | The token that tracks the status of the @UpdatePortfolioShare@ operation
    -- for external account to account or organizational type sharing.
    portfolioShareToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePortfolioShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updatePortfolioShareResponse_status' - The status of @UpdatePortfolioShare@ operation. You can also obtain the
-- operation status using @DescribePortfolioShareStatus@ API.
--
-- 'portfolioShareToken', 'updatePortfolioShareResponse_portfolioShareToken' - The token that tracks the status of the @UpdatePortfolioShare@ operation
-- for external account to account or organizational type sharing.
--
-- 'httpStatus', 'updatePortfolioShareResponse_httpStatus' - The response's http status code.
newUpdatePortfolioShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePortfolioShareResponse
newUpdatePortfolioShareResponse pHttpStatus_ =
  UpdatePortfolioShareResponse'
    { status =
        Prelude.Nothing,
      portfolioShareToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of @UpdatePortfolioShare@ operation. You can also obtain the
-- operation status using @DescribePortfolioShareStatus@ API.
updatePortfolioShareResponse_status :: Lens.Lens' UpdatePortfolioShareResponse (Prelude.Maybe ShareStatus)
updatePortfolioShareResponse_status = Lens.lens (\UpdatePortfolioShareResponse' {status} -> status) (\s@UpdatePortfolioShareResponse' {} a -> s {status = a} :: UpdatePortfolioShareResponse)

-- | The token that tracks the status of the @UpdatePortfolioShare@ operation
-- for external account to account or organizational type sharing.
updatePortfolioShareResponse_portfolioShareToken :: Lens.Lens' UpdatePortfolioShareResponse (Prelude.Maybe Prelude.Text)
updatePortfolioShareResponse_portfolioShareToken = Lens.lens (\UpdatePortfolioShareResponse' {portfolioShareToken} -> portfolioShareToken) (\s@UpdatePortfolioShareResponse' {} a -> s {portfolioShareToken = a} :: UpdatePortfolioShareResponse)

-- | The response's http status code.
updatePortfolioShareResponse_httpStatus :: Lens.Lens' UpdatePortfolioShareResponse Prelude.Int
updatePortfolioShareResponse_httpStatus = Lens.lens (\UpdatePortfolioShareResponse' {httpStatus} -> httpStatus) (\s@UpdatePortfolioShareResponse' {} a -> s {httpStatus = a} :: UpdatePortfolioShareResponse)

instance Prelude.NFData UpdatePortfolioShareResponse where
  rnf UpdatePortfolioShareResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf portfolioShareToken
      `Prelude.seq` Prelude.rnf httpStatus
