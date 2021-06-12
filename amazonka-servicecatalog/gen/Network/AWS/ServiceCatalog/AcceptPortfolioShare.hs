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
-- Module      : Network.AWS.ServiceCatalog.AcceptPortfolioShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an offer to share the specified portfolio.
module Network.AWS.ServiceCatalog.AcceptPortfolioShare
  ( -- * Creating a Request
    AcceptPortfolioShare (..),
    newAcceptPortfolioShare,

    -- * Request Lenses
    acceptPortfolioShare_portfolioShareType,
    acceptPortfolioShare_acceptLanguage,
    acceptPortfolioShare_portfolioId,

    -- * Destructuring the Response
    AcceptPortfolioShareResponse (..),
    newAcceptPortfolioShareResponse,

    -- * Response Lenses
    acceptPortfolioShareResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newAcceptPortfolioShare' smart constructor.
data AcceptPortfolioShare = AcceptPortfolioShare'
  { -- | The type of shared portfolios to accept. The default is to accept
    -- imported portfolios.
    --
    -- -   @AWS_ORGANIZATIONS@ - Accept portfolios shared by the management
    --     account of your organization.
    --
    -- -   @IMPORTED@ - Accept imported portfolios.
    --
    -- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
    --     ResourceNotFoundException.)
    --
    -- For example,
    -- @aws servicecatalog accept-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
    portfolioShareType :: Core.Maybe PortfolioShareType,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The portfolio identifier.
    portfolioId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptPortfolioShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioShareType', 'acceptPortfolioShare_portfolioShareType' - The type of shared portfolios to accept. The default is to accept
-- imported portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - Accept portfolios shared by the management
--     account of your organization.
--
-- -   @IMPORTED@ - Accept imported portfolios.
--
-- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
--     ResourceNotFoundException.)
--
-- For example,
-- @aws servicecatalog accept-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
--
-- 'acceptLanguage', 'acceptPortfolioShare_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'acceptPortfolioShare_portfolioId' - The portfolio identifier.
newAcceptPortfolioShare ::
  -- | 'portfolioId'
  Core.Text ->
  AcceptPortfolioShare
newAcceptPortfolioShare pPortfolioId_ =
  AcceptPortfolioShare'
    { portfolioShareType =
        Core.Nothing,
      acceptLanguage = Core.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The type of shared portfolios to accept. The default is to accept
-- imported portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - Accept portfolios shared by the management
--     account of your organization.
--
-- -   @IMPORTED@ - Accept imported portfolios.
--
-- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
--     ResourceNotFoundException.)
--
-- For example,
-- @aws servicecatalog accept-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
acceptPortfolioShare_portfolioShareType :: Lens.Lens' AcceptPortfolioShare (Core.Maybe PortfolioShareType)
acceptPortfolioShare_portfolioShareType = Lens.lens (\AcceptPortfolioShare' {portfolioShareType} -> portfolioShareType) (\s@AcceptPortfolioShare' {} a -> s {portfolioShareType = a} :: AcceptPortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
acceptPortfolioShare_acceptLanguage :: Lens.Lens' AcceptPortfolioShare (Core.Maybe Core.Text)
acceptPortfolioShare_acceptLanguage = Lens.lens (\AcceptPortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@AcceptPortfolioShare' {} a -> s {acceptLanguage = a} :: AcceptPortfolioShare)

-- | The portfolio identifier.
acceptPortfolioShare_portfolioId :: Lens.Lens' AcceptPortfolioShare Core.Text
acceptPortfolioShare_portfolioId = Lens.lens (\AcceptPortfolioShare' {portfolioId} -> portfolioId) (\s@AcceptPortfolioShare' {} a -> s {portfolioId = a} :: AcceptPortfolioShare)

instance Core.AWSRequest AcceptPortfolioShare where
  type
    AWSResponse AcceptPortfolioShare =
      AcceptPortfolioShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptPortfolioShareResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcceptPortfolioShare

instance Core.NFData AcceptPortfolioShare

instance Core.ToHeaders AcceptPortfolioShare where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.AcceptPortfolioShare" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AcceptPortfolioShare where
  toJSON AcceptPortfolioShare' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PortfolioShareType" Core..=)
              Core.<$> portfolioShareType,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath AcceptPortfolioShare where
  toPath = Core.const "/"

instance Core.ToQuery AcceptPortfolioShare where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAcceptPortfolioShareResponse' smart constructor.
data AcceptPortfolioShareResponse = AcceptPortfolioShareResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptPortfolioShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptPortfolioShareResponse_httpStatus' - The response's http status code.
newAcceptPortfolioShareResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptPortfolioShareResponse
newAcceptPortfolioShareResponse pHttpStatus_ =
  AcceptPortfolioShareResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptPortfolioShareResponse_httpStatus :: Lens.Lens' AcceptPortfolioShareResponse Core.Int
acceptPortfolioShareResponse_httpStatus = Lens.lens (\AcceptPortfolioShareResponse' {httpStatus} -> httpStatus) (\s@AcceptPortfolioShareResponse' {} a -> s {httpStatus = a} :: AcceptPortfolioShareResponse)

instance Core.NFData AcceptPortfolioShareResponse
