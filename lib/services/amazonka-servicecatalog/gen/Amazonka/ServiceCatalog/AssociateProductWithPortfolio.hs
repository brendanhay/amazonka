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
-- Module      : Amazonka.ServiceCatalog.AssociateProductWithPortfolio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified product with the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Amazonka.ServiceCatalog.AssociateProductWithPortfolio
  ( -- * Creating a Request
    AssociateProductWithPortfolio (..),
    newAssociateProductWithPortfolio,

    -- * Request Lenses
    associateProductWithPortfolio_acceptLanguage,
    associateProductWithPortfolio_sourcePortfolioId,
    associateProductWithPortfolio_productId,
    associateProductWithPortfolio_portfolioId,

    -- * Destructuring the Response
    AssociateProductWithPortfolioResponse (..),
    newAssociateProductWithPortfolioResponse,

    -- * Response Lenses
    associateProductWithPortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newAssociateProductWithPortfolio' smart constructor.
data AssociateProductWithPortfolio = AssociateProductWithPortfolio'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the source portfolio.
    sourcePortfolioId :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateProductWithPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'associateProductWithPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'sourcePortfolioId', 'associateProductWithPortfolio_sourcePortfolioId' - The identifier of the source portfolio.
--
-- 'productId', 'associateProductWithPortfolio_productId' - The product identifier.
--
-- 'portfolioId', 'associateProductWithPortfolio_portfolioId' - The portfolio identifier.
newAssociateProductWithPortfolio ::
  -- | 'productId'
  Prelude.Text ->
  -- | 'portfolioId'
  Prelude.Text ->
  AssociateProductWithPortfolio
newAssociateProductWithPortfolio
  pProductId_
  pPortfolioId_ =
    AssociateProductWithPortfolio'
      { acceptLanguage =
          Prelude.Nothing,
        sourcePortfolioId = Prelude.Nothing,
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
associateProductWithPortfolio_acceptLanguage :: Lens.Lens' AssociateProductWithPortfolio (Prelude.Maybe Prelude.Text)
associateProductWithPortfolio_acceptLanguage = Lens.lens (\AssociateProductWithPortfolio' {acceptLanguage} -> acceptLanguage) (\s@AssociateProductWithPortfolio' {} a -> s {acceptLanguage = a} :: AssociateProductWithPortfolio)

-- | The identifier of the source portfolio.
associateProductWithPortfolio_sourcePortfolioId :: Lens.Lens' AssociateProductWithPortfolio (Prelude.Maybe Prelude.Text)
associateProductWithPortfolio_sourcePortfolioId = Lens.lens (\AssociateProductWithPortfolio' {sourcePortfolioId} -> sourcePortfolioId) (\s@AssociateProductWithPortfolio' {} a -> s {sourcePortfolioId = a} :: AssociateProductWithPortfolio)

-- | The product identifier.
associateProductWithPortfolio_productId :: Lens.Lens' AssociateProductWithPortfolio Prelude.Text
associateProductWithPortfolio_productId = Lens.lens (\AssociateProductWithPortfolio' {productId} -> productId) (\s@AssociateProductWithPortfolio' {} a -> s {productId = a} :: AssociateProductWithPortfolio)

-- | The portfolio identifier.
associateProductWithPortfolio_portfolioId :: Lens.Lens' AssociateProductWithPortfolio Prelude.Text
associateProductWithPortfolio_portfolioId = Lens.lens (\AssociateProductWithPortfolio' {portfolioId} -> portfolioId) (\s@AssociateProductWithPortfolio' {} a -> s {portfolioId = a} :: AssociateProductWithPortfolio)

instance
  Core.AWSRequest
    AssociateProductWithPortfolio
  where
  type
    AWSResponse AssociateProductWithPortfolio =
      AssociateProductWithPortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateProductWithPortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateProductWithPortfolio
  where
  hashWithSalt _salt AssociateProductWithPortfolio' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` sourcePortfolioId
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` portfolioId

instance Prelude.NFData AssociateProductWithPortfolio where
  rnf AssociateProductWithPortfolio' {..} =
    Prelude.rnf acceptLanguage `Prelude.seq`
      Prelude.rnf sourcePortfolioId `Prelude.seq`
        Prelude.rnf productId `Prelude.seq`
          Prelude.rnf portfolioId

instance Data.ToHeaders AssociateProductWithPortfolio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.AssociateProductWithPortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateProductWithPortfolio where
  toJSON AssociateProductWithPortfolio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("SourcePortfolioId" Data..=)
              Prelude.<$> sourcePortfolioId,
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just ("PortfolioId" Data..= portfolioId)
          ]
      )

instance Data.ToPath AssociateProductWithPortfolio where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateProductWithPortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateProductWithPortfolioResponse' smart constructor.
data AssociateProductWithPortfolioResponse = AssociateProductWithPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateProductWithPortfolioResponse
newAssociateProductWithPortfolioResponse pHttpStatus_ =
  AssociateProductWithPortfolioResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateProductWithPortfolioResponse_httpStatus :: Lens.Lens' AssociateProductWithPortfolioResponse Prelude.Int
associateProductWithPortfolioResponse_httpStatus = Lens.lens (\AssociateProductWithPortfolioResponse' {httpStatus} -> httpStatus) (\s@AssociateProductWithPortfolioResponse' {} a -> s {httpStatus = a} :: AssociateProductWithPortfolioResponse)

instance
  Prelude.NFData
    AssociateProductWithPortfolioResponse
  where
  rnf AssociateProductWithPortfolioResponse' {..} =
    Prelude.rnf httpStatus
