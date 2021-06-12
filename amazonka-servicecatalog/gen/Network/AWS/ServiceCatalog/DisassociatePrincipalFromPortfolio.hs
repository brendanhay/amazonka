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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The portfolio identifier.
    portfolioId :: Core.Text,
    -- | The ARN of the principal (IAM user, role, or group).
    principalARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'principalARN'
  Core.Text ->
  DisassociatePrincipalFromPortfolio
newDisassociatePrincipalFromPortfolio
  pPortfolioId_
  pPrincipalARN_ =
    DisassociatePrincipalFromPortfolio'
      { acceptLanguage =
          Core.Nothing,
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
disassociatePrincipalFromPortfolio_acceptLanguage :: Lens.Lens' DisassociatePrincipalFromPortfolio (Core.Maybe Core.Text)
disassociatePrincipalFromPortfolio_acceptLanguage = Lens.lens (\DisassociatePrincipalFromPortfolio' {acceptLanguage} -> acceptLanguage) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {acceptLanguage = a} :: DisassociatePrincipalFromPortfolio)

-- | The portfolio identifier.
disassociatePrincipalFromPortfolio_portfolioId :: Lens.Lens' DisassociatePrincipalFromPortfolio Core.Text
disassociatePrincipalFromPortfolio_portfolioId = Lens.lens (\DisassociatePrincipalFromPortfolio' {portfolioId} -> portfolioId) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {portfolioId = a} :: DisassociatePrincipalFromPortfolio)

-- | The ARN of the principal (IAM user, role, or group).
disassociatePrincipalFromPortfolio_principalARN :: Lens.Lens' DisassociatePrincipalFromPortfolio Core.Text
disassociatePrincipalFromPortfolio_principalARN = Lens.lens (\DisassociatePrincipalFromPortfolio' {principalARN} -> principalARN) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {principalARN = a} :: DisassociatePrincipalFromPortfolio)

instance
  Core.AWSRequest
    DisassociatePrincipalFromPortfolio
  where
  type
    AWSResponse DisassociatePrincipalFromPortfolio =
      DisassociatePrincipalFromPortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociatePrincipalFromPortfolioResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociatePrincipalFromPortfolio

instance
  Core.NFData
    DisassociatePrincipalFromPortfolio

instance
  Core.ToHeaders
    DisassociatePrincipalFromPortfolio
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DisassociatePrincipalFromPortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DisassociatePrincipalFromPortfolio
  where
  toJSON DisassociatePrincipalFromPortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId),
            Core.Just ("PrincipalARN" Core..= principalARN)
          ]
      )

instance
  Core.ToPath
    DisassociatePrincipalFromPortfolio
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisassociatePrincipalFromPortfolio
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociatePrincipalFromPortfolioResponse' smart constructor.
data DisassociatePrincipalFromPortfolioResponse = DisassociatePrincipalFromPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisassociatePrincipalFromPortfolioResponse
newDisassociatePrincipalFromPortfolioResponse
  pHttpStatus_ =
    DisassociatePrincipalFromPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociatePrincipalFromPortfolioResponse_httpStatus :: Lens.Lens' DisassociatePrincipalFromPortfolioResponse Core.Int
disassociatePrincipalFromPortfolioResponse_httpStatus = Lens.lens (\DisassociatePrincipalFromPortfolioResponse' {httpStatus} -> httpStatus) (\s@DisassociatePrincipalFromPortfolioResponse' {} a -> s {httpStatus = a} :: DisassociatePrincipalFromPortfolioResponse)

instance
  Core.NFData
    DisassociatePrincipalFromPortfolioResponse
