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
-- Module      : Amazonka.ServiceCatalog.DisassociatePrincipalFromPortfolio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a previously associated principal ARN from a specified
-- portfolio.
--
-- The @PrincipalType@ and @PrincipalARN@ must match the
-- @AssociatePrincipalWithPortfolio@ call request details. For example, to
-- disassociate an association created with a @PrincipalARN@ of
-- @PrincipalType@ IAM you must use the @PrincipalType@ IAM when calling
-- @DisassociatePrincipalFromPortfolio@.
--
-- For portfolios that have been shared with principal name sharing
-- enabled: after disassociating a principal, share recipient accounts will
-- no longer be able to provision products in this portfolio using a role
-- matching the name of the associated principal.
module Amazonka.ServiceCatalog.DisassociatePrincipalFromPortfolio
  ( -- * Creating a Request
    DisassociatePrincipalFromPortfolio (..),
    newDisassociatePrincipalFromPortfolio,

    -- * Request Lenses
    disassociatePrincipalFromPortfolio_acceptLanguage,
    disassociatePrincipalFromPortfolio_principalType,
    disassociatePrincipalFromPortfolio_portfolioId,
    disassociatePrincipalFromPortfolio_principalARN,

    -- * Destructuring the Response
    DisassociatePrincipalFromPortfolioResponse (..),
    newDisassociatePrincipalFromPortfolioResponse,

    -- * Response Lenses
    disassociatePrincipalFromPortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

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
    -- | The supported value is @IAM@ if you use a fully defined ARN, or
    -- @IAM_PATTERN@ if you use no @accountID@.
    principalType :: Prelude.Maybe PrincipalType,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text,
    -- | The ARN of the principal (IAM user, role, or group). This field allows
    -- an ARN with no @accountID@ if @PrincipalType@ is @IAM_PATTERN@.
    principalARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'principalType', 'disassociatePrincipalFromPortfolio_principalType' - The supported value is @IAM@ if you use a fully defined ARN, or
-- @IAM_PATTERN@ if you use no @accountID@.
--
-- 'portfolioId', 'disassociatePrincipalFromPortfolio_portfolioId' - The portfolio identifier.
--
-- 'principalARN', 'disassociatePrincipalFromPortfolio_principalARN' - The ARN of the principal (IAM user, role, or group). This field allows
-- an ARN with no @accountID@ if @PrincipalType@ is @IAM_PATTERN@.
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
        principalType = Prelude.Nothing,
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

-- | The supported value is @IAM@ if you use a fully defined ARN, or
-- @IAM_PATTERN@ if you use no @accountID@.
disassociatePrincipalFromPortfolio_principalType :: Lens.Lens' DisassociatePrincipalFromPortfolio (Prelude.Maybe PrincipalType)
disassociatePrincipalFromPortfolio_principalType = Lens.lens (\DisassociatePrincipalFromPortfolio' {principalType} -> principalType) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {principalType = a} :: DisassociatePrincipalFromPortfolio)

-- | The portfolio identifier.
disassociatePrincipalFromPortfolio_portfolioId :: Lens.Lens' DisassociatePrincipalFromPortfolio Prelude.Text
disassociatePrincipalFromPortfolio_portfolioId = Lens.lens (\DisassociatePrincipalFromPortfolio' {portfolioId} -> portfolioId) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {portfolioId = a} :: DisassociatePrincipalFromPortfolio)

-- | The ARN of the principal (IAM user, role, or group). This field allows
-- an ARN with no @accountID@ if @PrincipalType@ is @IAM_PATTERN@.
disassociatePrincipalFromPortfolio_principalARN :: Lens.Lens' DisassociatePrincipalFromPortfolio Prelude.Text
disassociatePrincipalFromPortfolio_principalARN = Lens.lens (\DisassociatePrincipalFromPortfolio' {principalARN} -> principalARN) (\s@DisassociatePrincipalFromPortfolio' {} a -> s {principalARN = a} :: DisassociatePrincipalFromPortfolio)

instance
  Core.AWSRequest
    DisassociatePrincipalFromPortfolio
  where
  type
    AWSResponse DisassociatePrincipalFromPortfolio =
      DisassociatePrincipalFromPortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociatePrincipalFromPortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociatePrincipalFromPortfolio
  where
  hashWithSalt
    _salt
    DisassociatePrincipalFromPortfolio' {..} =
      _salt `Prelude.hashWithSalt` acceptLanguage
        `Prelude.hashWithSalt` principalType
        `Prelude.hashWithSalt` portfolioId
        `Prelude.hashWithSalt` principalARN

instance
  Prelude.NFData
    DisassociatePrincipalFromPortfolio
  where
  rnf DisassociatePrincipalFromPortfolio' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf principalType
      `Prelude.seq` Prelude.rnf portfolioId
      `Prelude.seq` Prelude.rnf principalARN

instance
  Data.ToHeaders
    DisassociatePrincipalFromPortfolio
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DisassociatePrincipalFromPortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociatePrincipalFromPortfolio
  where
  toJSON DisassociatePrincipalFromPortfolio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("PrincipalType" Data..=) Prelude.<$> principalType,
            Prelude.Just ("PortfolioId" Data..= portfolioId),
            Prelude.Just ("PrincipalARN" Data..= principalARN)
          ]
      )

instance
  Data.ToPath
    DisassociatePrincipalFromPortfolio
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociatePrincipalFromPortfolio
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociatePrincipalFromPortfolioResponse' smart constructor.
data DisassociatePrincipalFromPortfolioResponse = DisassociatePrincipalFromPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisassociatePrincipalFromPortfolioResponse' {..} =
    Prelude.rnf httpStatus
