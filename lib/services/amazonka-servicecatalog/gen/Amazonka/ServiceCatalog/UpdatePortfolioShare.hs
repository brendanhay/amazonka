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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified portfolio share. You can use this API to enable or
-- disable TagOptions sharing for an existing portfolio share.
--
-- The portfolio share cannot be updated if the @ CreatePortfolioShare@
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
module Amazonka.ServiceCatalog.UpdatePortfolioShare
  ( -- * Creating a Request
    UpdatePortfolioShare (..),
    newUpdatePortfolioShare,

    -- * Request Lenses
    updatePortfolioShare_accountId,
    updatePortfolioShare_organizationNode,
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newUpdatePortfolioShare' smart constructor.
data UpdatePortfolioShare = UpdatePortfolioShare'
  { -- | The AWS Account Id of the recipient account. This field is required when
    -- updating an external account to account type share.
    accountId :: Prelude.Maybe Prelude.Text,
    organizationNode :: Prelude.Maybe OrganizationNode,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | A flag to enable or disable TagOptions sharing for the portfolio share.
    -- If this field is not provided, the current state of TagOptions sharing
    -- on the portfolio share will not be modified.
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
-- 'accountId', 'updatePortfolioShare_accountId' - The AWS Account Id of the recipient account. This field is required when
-- updating an external account to account type share.
--
-- 'organizationNode', 'updatePortfolioShare_organizationNode' - Undocumented member.
--
-- 'acceptLanguage', 'updatePortfolioShare_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'shareTagOptions', 'updatePortfolioShare_shareTagOptions' - A flag to enable or disable TagOptions sharing for the portfolio share.
-- If this field is not provided, the current state of TagOptions sharing
-- on the portfolio share will not be modified.
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
      acceptLanguage = Prelude.Nothing,
      shareTagOptions = Prelude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The AWS Account Id of the recipient account. This field is required when
-- updating an external account to account type share.
updatePortfolioShare_accountId :: Lens.Lens' UpdatePortfolioShare (Prelude.Maybe Prelude.Text)
updatePortfolioShare_accountId = Lens.lens (\UpdatePortfolioShare' {accountId} -> accountId) (\s@UpdatePortfolioShare' {} a -> s {accountId = a} :: UpdatePortfolioShare)

-- | Undocumented member.
updatePortfolioShare_organizationNode :: Lens.Lens' UpdatePortfolioShare (Prelude.Maybe OrganizationNode)
updatePortfolioShare_organizationNode = Lens.lens (\UpdatePortfolioShare' {organizationNode} -> organizationNode) (\s@UpdatePortfolioShare' {} a -> s {organizationNode = a} :: UpdatePortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updatePortfolioShare_acceptLanguage :: Lens.Lens' UpdatePortfolioShare (Prelude.Maybe Prelude.Text)
updatePortfolioShare_acceptLanguage = Lens.lens (\UpdatePortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@UpdatePortfolioShare' {} a -> s {acceptLanguage = a} :: UpdatePortfolioShare)

-- | A flag to enable or disable TagOptions sharing for the portfolio share.
-- If this field is not provided, the current state of TagOptions sharing
-- on the portfolio share will not be modified.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePortfolioShareResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "PortfolioShareToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePortfolioShare where
  hashWithSalt _salt UpdatePortfolioShare' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` organizationNode
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` shareTagOptions
      `Prelude.hashWithSalt` portfolioId

instance Prelude.NFData UpdatePortfolioShare where
  rnf UpdatePortfolioShare' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf organizationNode
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf shareTagOptions
      `Prelude.seq` Prelude.rnf portfolioId

instance Core.ToHeaders UpdatePortfolioShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdatePortfolioShare" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdatePortfolioShare where
  toJSON UpdatePortfolioShare' {..} =
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

instance Core.ToPath UpdatePortfolioShare where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdatePortfolioShare where
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
