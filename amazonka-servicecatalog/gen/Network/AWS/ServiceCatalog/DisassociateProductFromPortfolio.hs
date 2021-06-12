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
-- Module      : Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified product from the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
  ( -- * Creating a Request
    DisassociateProductFromPortfolio (..),
    newDisassociateProductFromPortfolio,

    -- * Request Lenses
    disassociateProductFromPortfolio_acceptLanguage,
    disassociateProductFromPortfolio_productId,
    disassociateProductFromPortfolio_portfolioId,

    -- * Destructuring the Response
    DisassociateProductFromPortfolioResponse (..),
    newDisassociateProductFromPortfolioResponse,

    -- * Response Lenses
    disassociateProductFromPortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDisassociateProductFromPortfolio' smart constructor.
data DisassociateProductFromPortfolio = DisassociateProductFromPortfolio'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Text,
    -- | The portfolio identifier.
    portfolioId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateProductFromPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'disassociateProductFromPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'disassociateProductFromPortfolio_productId' - The product identifier.
--
-- 'portfolioId', 'disassociateProductFromPortfolio_portfolioId' - The portfolio identifier.
newDisassociateProductFromPortfolio ::
  -- | 'productId'
  Core.Text ->
  -- | 'portfolioId'
  Core.Text ->
  DisassociateProductFromPortfolio
newDisassociateProductFromPortfolio
  pProductId_
  pPortfolioId_ =
    DisassociateProductFromPortfolio'
      { acceptLanguage =
          Core.Nothing,
        productId = pProductId_,
        portfolioId = pPortfolioId_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
disassociateProductFromPortfolio_acceptLanguage :: Lens.Lens' DisassociateProductFromPortfolio (Core.Maybe Core.Text)
disassociateProductFromPortfolio_acceptLanguage = Lens.lens (\DisassociateProductFromPortfolio' {acceptLanguage} -> acceptLanguage) (\s@DisassociateProductFromPortfolio' {} a -> s {acceptLanguage = a} :: DisassociateProductFromPortfolio)

-- | The product identifier.
disassociateProductFromPortfolio_productId :: Lens.Lens' DisassociateProductFromPortfolio Core.Text
disassociateProductFromPortfolio_productId = Lens.lens (\DisassociateProductFromPortfolio' {productId} -> productId) (\s@DisassociateProductFromPortfolio' {} a -> s {productId = a} :: DisassociateProductFromPortfolio)

-- | The portfolio identifier.
disassociateProductFromPortfolio_portfolioId :: Lens.Lens' DisassociateProductFromPortfolio Core.Text
disassociateProductFromPortfolio_portfolioId = Lens.lens (\DisassociateProductFromPortfolio' {portfolioId} -> portfolioId) (\s@DisassociateProductFromPortfolio' {} a -> s {portfolioId = a} :: DisassociateProductFromPortfolio)

instance
  Core.AWSRequest
    DisassociateProductFromPortfolio
  where
  type
    AWSResponse DisassociateProductFromPortfolio =
      DisassociateProductFromPortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateProductFromPortfolioResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociateProductFromPortfolio

instance Core.NFData DisassociateProductFromPortfolio

instance
  Core.ToHeaders
    DisassociateProductFromPortfolio
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DisassociateProductFromPortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateProductFromPortfolio where
  toJSON DisassociateProductFromPortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("ProductId" Core..= productId),
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath DisassociateProductFromPortfolio where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisassociateProductFromPortfolio
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateProductFromPortfolioResponse' smart constructor.
data DisassociateProductFromPortfolioResponse = DisassociateProductFromPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateProductFromPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateProductFromPortfolioResponse_httpStatus' - The response's http status code.
newDisassociateProductFromPortfolioResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateProductFromPortfolioResponse
newDisassociateProductFromPortfolioResponse
  pHttpStatus_ =
    DisassociateProductFromPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateProductFromPortfolioResponse_httpStatus :: Lens.Lens' DisassociateProductFromPortfolioResponse Core.Int
disassociateProductFromPortfolioResponse_httpStatus = Lens.lens (\DisassociateProductFromPortfolioResponse' {httpStatus} -> httpStatus) (\s@DisassociateProductFromPortfolioResponse' {} a -> s {httpStatus = a} :: DisassociateProductFromPortfolioResponse)

instance
  Core.NFData
    DisassociateProductFromPortfolioResponse
