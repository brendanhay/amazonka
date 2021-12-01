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
-- Module      : Amazonka.ServiceCatalog.AssociatePrincipalWithPortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified principal ARN with the specified portfolio.
module Amazonka.ServiceCatalog.AssociatePrincipalWithPortfolio
  ( -- * Creating a Request
    AssociatePrincipalWithPortfolio (..),
    newAssociatePrincipalWithPortfolio,

    -- * Request Lenses
    associatePrincipalWithPortfolio_acceptLanguage,
    associatePrincipalWithPortfolio_portfolioId,
    associatePrincipalWithPortfolio_principalARN,
    associatePrincipalWithPortfolio_principalType,

    -- * Destructuring the Response
    AssociatePrincipalWithPortfolioResponse (..),
    newAssociatePrincipalWithPortfolioResponse,

    -- * Response Lenses
    associatePrincipalWithPortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newAssociatePrincipalWithPortfolio' smart constructor.
data AssociatePrincipalWithPortfolio = AssociatePrincipalWithPortfolio'
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
    principalARN :: Prelude.Text,
    -- | The principal type. The supported value is @IAM@.
    principalType :: PrincipalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePrincipalWithPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'associatePrincipalWithPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'associatePrincipalWithPortfolio_portfolioId' - The portfolio identifier.
--
-- 'principalARN', 'associatePrincipalWithPortfolio_principalARN' - The ARN of the principal (IAM user, role, or group).
--
-- 'principalType', 'associatePrincipalWithPortfolio_principalType' - The principal type. The supported value is @IAM@.
newAssociatePrincipalWithPortfolio ::
  -- | 'portfolioId'
  Prelude.Text ->
  -- | 'principalARN'
  Prelude.Text ->
  -- | 'principalType'
  PrincipalType ->
  AssociatePrincipalWithPortfolio
newAssociatePrincipalWithPortfolio
  pPortfolioId_
  pPrincipalARN_
  pPrincipalType_ =
    AssociatePrincipalWithPortfolio'
      { acceptLanguage =
          Prelude.Nothing,
        portfolioId = pPortfolioId_,
        principalARN = pPrincipalARN_,
        principalType = pPrincipalType_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
associatePrincipalWithPortfolio_acceptLanguage :: Lens.Lens' AssociatePrincipalWithPortfolio (Prelude.Maybe Prelude.Text)
associatePrincipalWithPortfolio_acceptLanguage = Lens.lens (\AssociatePrincipalWithPortfolio' {acceptLanguage} -> acceptLanguage) (\s@AssociatePrincipalWithPortfolio' {} a -> s {acceptLanguage = a} :: AssociatePrincipalWithPortfolio)

-- | The portfolio identifier.
associatePrincipalWithPortfolio_portfolioId :: Lens.Lens' AssociatePrincipalWithPortfolio Prelude.Text
associatePrincipalWithPortfolio_portfolioId = Lens.lens (\AssociatePrincipalWithPortfolio' {portfolioId} -> portfolioId) (\s@AssociatePrincipalWithPortfolio' {} a -> s {portfolioId = a} :: AssociatePrincipalWithPortfolio)

-- | The ARN of the principal (IAM user, role, or group).
associatePrincipalWithPortfolio_principalARN :: Lens.Lens' AssociatePrincipalWithPortfolio Prelude.Text
associatePrincipalWithPortfolio_principalARN = Lens.lens (\AssociatePrincipalWithPortfolio' {principalARN} -> principalARN) (\s@AssociatePrincipalWithPortfolio' {} a -> s {principalARN = a} :: AssociatePrincipalWithPortfolio)

-- | The principal type. The supported value is @IAM@.
associatePrincipalWithPortfolio_principalType :: Lens.Lens' AssociatePrincipalWithPortfolio PrincipalType
associatePrincipalWithPortfolio_principalType = Lens.lens (\AssociatePrincipalWithPortfolio' {principalType} -> principalType) (\s@AssociatePrincipalWithPortfolio' {} a -> s {principalType = a} :: AssociatePrincipalWithPortfolio)

instance
  Core.AWSRequest
    AssociatePrincipalWithPortfolio
  where
  type
    AWSResponse AssociatePrincipalWithPortfolio =
      AssociatePrincipalWithPortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociatePrincipalWithPortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociatePrincipalWithPortfolio
  where
  hashWithSalt
    salt'
    AssociatePrincipalWithPortfolio' {..} =
      salt' `Prelude.hashWithSalt` principalType
        `Prelude.hashWithSalt` principalARN
        `Prelude.hashWithSalt` portfolioId
        `Prelude.hashWithSalt` acceptLanguage

instance
  Prelude.NFData
    AssociatePrincipalWithPortfolio
  where
  rnf AssociatePrincipalWithPortfolio' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf principalType
      `Prelude.seq` Prelude.rnf principalARN
      `Prelude.seq` Prelude.rnf portfolioId

instance
  Core.ToHeaders
    AssociatePrincipalWithPortfolio
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.AssociatePrincipalWithPortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociatePrincipalWithPortfolio where
  toJSON AssociatePrincipalWithPortfolio' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PortfolioId" Core..= portfolioId),
            Prelude.Just ("PrincipalARN" Core..= principalARN),
            Prelude.Just
              ("PrincipalType" Core..= principalType)
          ]
      )

instance Core.ToPath AssociatePrincipalWithPortfolio where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociatePrincipalWithPortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociatePrincipalWithPortfolioResponse' smart constructor.
data AssociatePrincipalWithPortfolioResponse = AssociatePrincipalWithPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePrincipalWithPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associatePrincipalWithPortfolioResponse_httpStatus' - The response's http status code.
newAssociatePrincipalWithPortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociatePrincipalWithPortfolioResponse
newAssociatePrincipalWithPortfolioResponse
  pHttpStatus_ =
    AssociatePrincipalWithPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associatePrincipalWithPortfolioResponse_httpStatus :: Lens.Lens' AssociatePrincipalWithPortfolioResponse Prelude.Int
associatePrincipalWithPortfolioResponse_httpStatus = Lens.lens (\AssociatePrincipalWithPortfolioResponse' {httpStatus} -> httpStatus) (\s@AssociatePrincipalWithPortfolioResponse' {} a -> s {httpStatus = a} :: AssociatePrincipalWithPortfolioResponse)

instance
  Prelude.NFData
    AssociatePrincipalWithPortfolioResponse
  where
  rnf AssociatePrincipalWithPortfolioResponse' {..} =
    Prelude.rnf httpStatus
