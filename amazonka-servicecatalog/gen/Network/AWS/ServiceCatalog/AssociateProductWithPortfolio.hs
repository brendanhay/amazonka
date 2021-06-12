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
-- Module      : Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified product with the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
  ( -- * Creating a Request
    AssociateProductWithPortfolio (..),
    newAssociateProductWithPortfolio,

    -- * Request Lenses
    associateProductWithPortfolio_sourcePortfolioId,
    associateProductWithPortfolio_acceptLanguage,
    associateProductWithPortfolio_productId,
    associateProductWithPortfolio_portfolioId,

    -- * Destructuring the Response
    AssociateProductWithPortfolioResponse (..),
    newAssociateProductWithPortfolioResponse,

    -- * Response Lenses
    associateProductWithPortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newAssociateProductWithPortfolio' smart constructor.
data AssociateProductWithPortfolio = AssociateProductWithPortfolio'
  { -- | The identifier of the source portfolio.
    sourcePortfolioId :: Core.Maybe Core.Text,
    -- | The language code.
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
-- Create a value of 'AssociateProductWithPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePortfolioId', 'associateProductWithPortfolio_sourcePortfolioId' - The identifier of the source portfolio.
--
-- 'acceptLanguage', 'associateProductWithPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'associateProductWithPortfolio_productId' - The product identifier.
--
-- 'portfolioId', 'associateProductWithPortfolio_portfolioId' - The portfolio identifier.
newAssociateProductWithPortfolio ::
  -- | 'productId'
  Core.Text ->
  -- | 'portfolioId'
  Core.Text ->
  AssociateProductWithPortfolio
newAssociateProductWithPortfolio
  pProductId_
  pPortfolioId_ =
    AssociateProductWithPortfolio'
      { sourcePortfolioId =
          Core.Nothing,
        acceptLanguage = Core.Nothing,
        productId = pProductId_,
        portfolioId = pPortfolioId_
      }

-- | The identifier of the source portfolio.
associateProductWithPortfolio_sourcePortfolioId :: Lens.Lens' AssociateProductWithPortfolio (Core.Maybe Core.Text)
associateProductWithPortfolio_sourcePortfolioId = Lens.lens (\AssociateProductWithPortfolio' {sourcePortfolioId} -> sourcePortfolioId) (\s@AssociateProductWithPortfolio' {} a -> s {sourcePortfolioId = a} :: AssociateProductWithPortfolio)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
associateProductWithPortfolio_acceptLanguage :: Lens.Lens' AssociateProductWithPortfolio (Core.Maybe Core.Text)
associateProductWithPortfolio_acceptLanguage = Lens.lens (\AssociateProductWithPortfolio' {acceptLanguage} -> acceptLanguage) (\s@AssociateProductWithPortfolio' {} a -> s {acceptLanguage = a} :: AssociateProductWithPortfolio)

-- | The product identifier.
associateProductWithPortfolio_productId :: Lens.Lens' AssociateProductWithPortfolio Core.Text
associateProductWithPortfolio_productId = Lens.lens (\AssociateProductWithPortfolio' {productId} -> productId) (\s@AssociateProductWithPortfolio' {} a -> s {productId = a} :: AssociateProductWithPortfolio)

-- | The portfolio identifier.
associateProductWithPortfolio_portfolioId :: Lens.Lens' AssociateProductWithPortfolio Core.Text
associateProductWithPortfolio_portfolioId = Lens.lens (\AssociateProductWithPortfolio' {portfolioId} -> portfolioId) (\s@AssociateProductWithPortfolio' {} a -> s {portfolioId = a} :: AssociateProductWithPortfolio)

instance
  Core.AWSRequest
    AssociateProductWithPortfolio
  where
  type
    AWSResponse AssociateProductWithPortfolio =
      AssociateProductWithPortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateProductWithPortfolioResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateProductWithPortfolio

instance Core.NFData AssociateProductWithPortfolio

instance Core.ToHeaders AssociateProductWithPortfolio where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.AssociateProductWithPortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateProductWithPortfolio where
  toJSON AssociateProductWithPortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SourcePortfolioId" Core..=)
              Core.<$> sourcePortfolioId,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("ProductId" Core..= productId),
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath AssociateProductWithPortfolio where
  toPath = Core.const "/"

instance Core.ToQuery AssociateProductWithPortfolio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateProductWithPortfolioResponse' smart constructor.
data AssociateProductWithPortfolioResponse = AssociateProductWithPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateProductWithPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateProductWithPortfolioResponse_httpStatus' - The response's http status code.
newAssociateProductWithPortfolioResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateProductWithPortfolioResponse
newAssociateProductWithPortfolioResponse pHttpStatus_ =
  AssociateProductWithPortfolioResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateProductWithPortfolioResponse_httpStatus :: Lens.Lens' AssociateProductWithPortfolioResponse Core.Int
associateProductWithPortfolioResponse_httpStatus = Lens.lens (\AssociateProductWithPortfolioResponse' {httpStatus} -> httpStatus) (\s@AssociateProductWithPortfolioResponse' {} a -> s {httpStatus = a} :: AssociateProductWithPortfolioResponse)

instance
  Core.NFData
    AssociateProductWithPortfolioResponse
