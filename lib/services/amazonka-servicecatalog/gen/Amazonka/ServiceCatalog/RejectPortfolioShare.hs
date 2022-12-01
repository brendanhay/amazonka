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
-- Module      : Amazonka.ServiceCatalog.RejectPortfolioShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an offer to share the specified portfolio.
module Amazonka.ServiceCatalog.RejectPortfolioShare
  ( -- * Creating a Request
    RejectPortfolioShare (..),
    newRejectPortfolioShare,

    -- * Request Lenses
    rejectPortfolioShare_portfolioShareType,
    rejectPortfolioShare_acceptLanguage,
    rejectPortfolioShare_portfolioId,

    -- * Destructuring the Response
    RejectPortfolioShareResponse (..),
    newRejectPortfolioShareResponse,

    -- * Response Lenses
    rejectPortfolioShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newRejectPortfolioShare' smart constructor.
data RejectPortfolioShare = RejectPortfolioShare'
  { -- | The type of shared portfolios to reject. The default is to reject
    -- imported portfolios.
    --
    -- -   @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management
    --     account of your organization.
    --
    -- -   @IMPORTED@ - Reject imported portfolios.
    --
    -- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
    --     ResourceNotFoundException.)
    --
    -- For example,
    -- @aws servicecatalog reject-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
    portfolioShareType :: Prelude.Maybe PortfolioShareType,
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
-- Create a value of 'RejectPortfolioShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioShareType', 'rejectPortfolioShare_portfolioShareType' - The type of shared portfolios to reject. The default is to reject
-- imported portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management
--     account of your organization.
--
-- -   @IMPORTED@ - Reject imported portfolios.
--
-- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
--     ResourceNotFoundException.)
--
-- For example,
-- @aws servicecatalog reject-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
--
-- 'acceptLanguage', 'rejectPortfolioShare_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'rejectPortfolioShare_portfolioId' - The portfolio identifier.
newRejectPortfolioShare ::
  -- | 'portfolioId'
  Prelude.Text ->
  RejectPortfolioShare
newRejectPortfolioShare pPortfolioId_ =
  RejectPortfolioShare'
    { portfolioShareType =
        Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The type of shared portfolios to reject. The default is to reject
-- imported portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management
--     account of your organization.
--
-- -   @IMPORTED@ - Reject imported portfolios.
--
-- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
--     ResourceNotFoundException.)
--
-- For example,
-- @aws servicecatalog reject-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
rejectPortfolioShare_portfolioShareType :: Lens.Lens' RejectPortfolioShare (Prelude.Maybe PortfolioShareType)
rejectPortfolioShare_portfolioShareType = Lens.lens (\RejectPortfolioShare' {portfolioShareType} -> portfolioShareType) (\s@RejectPortfolioShare' {} a -> s {portfolioShareType = a} :: RejectPortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
rejectPortfolioShare_acceptLanguage :: Lens.Lens' RejectPortfolioShare (Prelude.Maybe Prelude.Text)
rejectPortfolioShare_acceptLanguage = Lens.lens (\RejectPortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@RejectPortfolioShare' {} a -> s {acceptLanguage = a} :: RejectPortfolioShare)

-- | The portfolio identifier.
rejectPortfolioShare_portfolioId :: Lens.Lens' RejectPortfolioShare Prelude.Text
rejectPortfolioShare_portfolioId = Lens.lens (\RejectPortfolioShare' {portfolioId} -> portfolioId) (\s@RejectPortfolioShare' {} a -> s {portfolioId = a} :: RejectPortfolioShare)

instance Core.AWSRequest RejectPortfolioShare where
  type
    AWSResponse RejectPortfolioShare =
      RejectPortfolioShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectPortfolioShareResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectPortfolioShare where
  hashWithSalt _salt RejectPortfolioShare' {..} =
    _salt `Prelude.hashWithSalt` portfolioShareType
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` portfolioId

instance Prelude.NFData RejectPortfolioShare where
  rnf RejectPortfolioShare' {..} =
    Prelude.rnf portfolioShareType
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf portfolioId

instance Core.ToHeaders RejectPortfolioShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.RejectPortfolioShare" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RejectPortfolioShare where
  toJSON RejectPortfolioShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PortfolioShareType" Core..=)
              Prelude.<$> portfolioShareType,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath RejectPortfolioShare where
  toPath = Prelude.const "/"

instance Core.ToQuery RejectPortfolioShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectPortfolioShareResponse' smart constructor.
data RejectPortfolioShareResponse = RejectPortfolioShareResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectPortfolioShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectPortfolioShareResponse_httpStatus' - The response's http status code.
newRejectPortfolioShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectPortfolioShareResponse
newRejectPortfolioShareResponse pHttpStatus_ =
  RejectPortfolioShareResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectPortfolioShareResponse_httpStatus :: Lens.Lens' RejectPortfolioShareResponse Prelude.Int
rejectPortfolioShareResponse_httpStatus = Lens.lens (\RejectPortfolioShareResponse' {httpStatus} -> httpStatus) (\s@RejectPortfolioShareResponse' {} a -> s {httpStatus = a} :: RejectPortfolioShareResponse)

instance Prelude.NFData RejectPortfolioShareResponse where
  rnf RejectPortfolioShareResponse' {..} =
    Prelude.rnf httpStatus
