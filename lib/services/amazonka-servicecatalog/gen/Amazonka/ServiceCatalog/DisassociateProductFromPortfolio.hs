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
-- Module      : Amazonka.ServiceCatalog.DisassociateProductFromPortfolio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified product from the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Amazonka.ServiceCatalog.DisassociateProductFromPortfolio
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDisassociateProductFromPortfolio' smart constructor.
data DisassociateProductFromPortfolio = DisassociateProductFromPortfolio'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'portfolioId'
  Prelude.Text ->
  DisassociateProductFromPortfolio
newDisassociateProductFromPortfolio
  pProductId_
  pPortfolioId_ =
    DisassociateProductFromPortfolio'
      { acceptLanguage =
          Prelude.Nothing,
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
disassociateProductFromPortfolio_acceptLanguage :: Lens.Lens' DisassociateProductFromPortfolio (Prelude.Maybe Prelude.Text)
disassociateProductFromPortfolio_acceptLanguage = Lens.lens (\DisassociateProductFromPortfolio' {acceptLanguage} -> acceptLanguage) (\s@DisassociateProductFromPortfolio' {} a -> s {acceptLanguage = a} :: DisassociateProductFromPortfolio)

-- | The product identifier.
disassociateProductFromPortfolio_productId :: Lens.Lens' DisassociateProductFromPortfolio Prelude.Text
disassociateProductFromPortfolio_productId = Lens.lens (\DisassociateProductFromPortfolio' {productId} -> productId) (\s@DisassociateProductFromPortfolio' {} a -> s {productId = a} :: DisassociateProductFromPortfolio)

-- | The portfolio identifier.
disassociateProductFromPortfolio_portfolioId :: Lens.Lens' DisassociateProductFromPortfolio Prelude.Text
disassociateProductFromPortfolio_portfolioId = Lens.lens (\DisassociateProductFromPortfolio' {portfolioId} -> portfolioId) (\s@DisassociateProductFromPortfolio' {} a -> s {portfolioId = a} :: DisassociateProductFromPortfolio)

instance
  Core.AWSRequest
    DisassociateProductFromPortfolio
  where
  type
    AWSResponse DisassociateProductFromPortfolio =
      DisassociateProductFromPortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateProductFromPortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateProductFromPortfolio
  where
  hashWithSalt
    _salt
    DisassociateProductFromPortfolio' {..} =
      _salt
        `Prelude.hashWithSalt` acceptLanguage
        `Prelude.hashWithSalt` productId
        `Prelude.hashWithSalt` portfolioId

instance
  Prelude.NFData
    DisassociateProductFromPortfolio
  where
  rnf DisassociateProductFromPortfolio' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf portfolioId

instance
  Data.ToHeaders
    DisassociateProductFromPortfolio
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DisassociateProductFromPortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateProductFromPortfolio where
  toJSON DisassociateProductFromPortfolio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just ("PortfolioId" Data..= portfolioId)
          ]
      )

instance Data.ToPath DisassociateProductFromPortfolio where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateProductFromPortfolio
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateProductFromPortfolioResponse' smart constructor.
data DisassociateProductFromPortfolioResponse = DisassociateProductFromPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateProductFromPortfolioResponse
newDisassociateProductFromPortfolioResponse
  pHttpStatus_ =
    DisassociateProductFromPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateProductFromPortfolioResponse_httpStatus :: Lens.Lens' DisassociateProductFromPortfolioResponse Prelude.Int
disassociateProductFromPortfolioResponse_httpStatus = Lens.lens (\DisassociateProductFromPortfolioResponse' {httpStatus} -> httpStatus) (\s@DisassociateProductFromPortfolioResponse' {} a -> s {httpStatus = a} :: DisassociateProductFromPortfolioResponse)

instance
  Prelude.NFData
    DisassociateProductFromPortfolioResponse
  where
  rnf DisassociateProductFromPortfolioResponse' {..} =
    Prelude.rnf httpStatus
