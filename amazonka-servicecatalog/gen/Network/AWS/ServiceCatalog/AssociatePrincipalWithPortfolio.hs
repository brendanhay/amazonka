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
-- Module      : Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified principal ARN with the specified portfolio.
module Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newAssociatePrincipalWithPortfolio' smart constructor.
data AssociatePrincipalWithPortfolio = AssociatePrincipalWithPortfolio'
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
    principalARN :: Core.Text,
    -- | The principal type. The supported value is @IAM@.
    principalType :: PrincipalType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'principalARN'
  Core.Text ->
  -- | 'principalType'
  PrincipalType ->
  AssociatePrincipalWithPortfolio
newAssociatePrincipalWithPortfolio
  pPortfolioId_
  pPrincipalARN_
  pPrincipalType_ =
    AssociatePrincipalWithPortfolio'
      { acceptLanguage =
          Core.Nothing,
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
associatePrincipalWithPortfolio_acceptLanguage :: Lens.Lens' AssociatePrincipalWithPortfolio (Core.Maybe Core.Text)
associatePrincipalWithPortfolio_acceptLanguage = Lens.lens (\AssociatePrincipalWithPortfolio' {acceptLanguage} -> acceptLanguage) (\s@AssociatePrincipalWithPortfolio' {} a -> s {acceptLanguage = a} :: AssociatePrincipalWithPortfolio)

-- | The portfolio identifier.
associatePrincipalWithPortfolio_portfolioId :: Lens.Lens' AssociatePrincipalWithPortfolio Core.Text
associatePrincipalWithPortfolio_portfolioId = Lens.lens (\AssociatePrincipalWithPortfolio' {portfolioId} -> portfolioId) (\s@AssociatePrincipalWithPortfolio' {} a -> s {portfolioId = a} :: AssociatePrincipalWithPortfolio)

-- | The ARN of the principal (IAM user, role, or group).
associatePrincipalWithPortfolio_principalARN :: Lens.Lens' AssociatePrincipalWithPortfolio Core.Text
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
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AssociatePrincipalWithPortfolio

instance Core.NFData AssociatePrincipalWithPortfolio

instance
  Core.ToHeaders
    AssociatePrincipalWithPortfolio
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.AssociatePrincipalWithPortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociatePrincipalWithPortfolio where
  toJSON AssociatePrincipalWithPortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId),
            Core.Just ("PrincipalARN" Core..= principalARN),
            Core.Just ("PrincipalType" Core..= principalType)
          ]
      )

instance Core.ToPath AssociatePrincipalWithPortfolio where
  toPath = Core.const "/"

instance Core.ToQuery AssociatePrincipalWithPortfolio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociatePrincipalWithPortfolioResponse' smart constructor.
data AssociatePrincipalWithPortfolioResponse = AssociatePrincipalWithPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AssociatePrincipalWithPortfolioResponse
newAssociatePrincipalWithPortfolioResponse
  pHttpStatus_ =
    AssociatePrincipalWithPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associatePrincipalWithPortfolioResponse_httpStatus :: Lens.Lens' AssociatePrincipalWithPortfolioResponse Core.Int
associatePrincipalWithPortfolioResponse_httpStatus = Lens.lens (\AssociatePrincipalWithPortfolioResponse' {httpStatus} -> httpStatus) (\s@AssociatePrincipalWithPortfolioResponse' {} a -> s {httpStatus = a} :: AssociatePrincipalWithPortfolioResponse)

instance
  Core.NFData
    AssociatePrincipalWithPortfolioResponse
