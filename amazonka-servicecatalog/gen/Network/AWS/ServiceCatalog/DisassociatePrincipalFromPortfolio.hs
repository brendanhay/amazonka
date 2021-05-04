{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a previously associated principal ARN from a specified
-- portfolio.
module Network.AWS.ServiceCatalog.DisassociatePrincipalFromPortfolio
  ( -- * Creating a Request
    DisassociatePrincipalFromPortfolio (..),
    newDisassociatePrincipalFromPortfolio,

    -- * Request Lenses
    disassociatePrincipalFromPortfolio_acceptLanguage,
    disassociatePrincipalFromPortfolio_portfolioId,
    disassociatePrincipalFromPortfolio_principalARN,

    -- * Destructuring the Response
    DisassociatePrincipalFromPortfolioResponse (..),
    newDisassociatePrincipalFromPortfolioResponse,

    -- * Response Lenses
    disassociatePrincipalFromPortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDisassociatePrincipalFromPortfolio' smart constructor.
data DisassociatePrincipalFromPortfolio = DisassociatePrincipalFromPortfolio'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text,
    -- | The ARN of the principal (IAM user, role, or group).
    principalARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePrincipalFromPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'disassociatePrincipalFromPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'disassociatePrincipalFromPortfolio_portfolioId' - The portfolio identifier.
--
-- 'principalARN', 'disassociatePrincipalFromPortfolio_principalARN' - The ARN of the principal (IAM user, role, or group).
newDisassociatePrincipalFromPortfolio ::
  -- | 'portfolioId'
  Prelude.Text ->
  -- | 'principalARN'
  Prelude.Text ->
  DisassociatePrincipalFromPortfolio
newDisassociatePrincipalFromPortfolio
  pPortfolioId_
  pPrincipalARN_ =
    DisassociatePrincipalFromPortfolio'
      { acceptLanguage =
          Prelude.Nothing,
        portfolioId = pPortfolioId_,
        principalARN = pPrincipalARN_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
disassociatePrincipalFromPortfolio_acceptLanguage :: Lens.Lens' DisassociatePrincipalFromPortfolio (Prelude.Maybe Prelude.Text)
disassociatePrincipalFromPortfolio_acceptLanguage = Lens.lens (\DisassociatePrincipalFromPortfolio' {acceptLanguage} -> acceptLanguage) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {acceptLanguage = a} :: DisassociatePrincipalFromPortfolio)

-- | The portfolio identifier.
disassociatePrincipalFromPortfolio_portfolioId :: Lens.Lens' DisassociatePrincipalFromPortfolio Prelude.Text
disassociatePrincipalFromPortfolio_portfolioId = Lens.lens (\DisassociatePrincipalFromPortfolio' {portfolioId} -> portfolioId) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {portfolioId = a} :: DisassociatePrincipalFromPortfolio)

-- | The ARN of the principal (IAM user, role, or group).
disassociatePrincipalFromPortfolio_principalARN :: Lens.Lens' DisassociatePrincipalFromPortfolio Prelude.Text
disassociatePrincipalFromPortfolio_principalARN = Lens.lens (\DisassociatePrincipalFromPortfolio' {principalARN} -> principalARN) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {principalARN = a} :: DisassociatePrincipalFromPortfolio)

instance
  Prelude.AWSRequest
    DisassociatePrincipalFromPortfolio
  where
  type
    Rs DisassociatePrincipalFromPortfolio =
      DisassociatePrincipalFromPortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociatePrincipalFromPortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociatePrincipalFromPortfolio

instance
  Prelude.NFData
    DisassociatePrincipalFromPortfolio

instance
  Prelude.ToHeaders
    DisassociatePrincipalFromPortfolio
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DisassociatePrincipalFromPortfolio" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DisassociatePrincipalFromPortfolio
  where
  toJSON DisassociatePrincipalFromPortfolio' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PortfolioId" Prelude..= portfolioId),
            Prelude.Just
              ("PrincipalARN" Prelude..= principalARN)
          ]
      )

instance
  Prelude.ToPath
    DisassociatePrincipalFromPortfolio
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociatePrincipalFromPortfolio
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociatePrincipalFromPortfolioResponse' smart constructor.
data DisassociatePrincipalFromPortfolioResponse = DisassociatePrincipalFromPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociatePrincipalFromPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociatePrincipalFromPortfolioResponse_httpStatus' - The response's http status code.
newDisassociatePrincipalFromPortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociatePrincipalFromPortfolioResponse
newDisassociatePrincipalFromPortfolioResponse
  pHttpStatus_ =
    DisassociatePrincipalFromPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociatePrincipalFromPortfolioResponse_httpStatus :: Lens.Lens' DisassociatePrincipalFromPortfolioResponse Prelude.Int
disassociatePrincipalFromPortfolioResponse_httpStatus = Lens.lens (\DisassociatePrincipalFromPortfolioResponse' {httpStatus} -> httpStatus) (\s@DisassociatePrincipalFromPortfolioResponse' {} a -> s {httpStatus = a} :: DisassociatePrincipalFromPortfolioResponse)

instance
  Prelude.NFData
    DisassociatePrincipalFromPortfolioResponse
