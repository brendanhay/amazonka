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
-- Module      : Network.AWS.ServiceCatalog.RejectPortfolioShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an offer to share the specified portfolio.
module Network.AWS.ServiceCatalog.RejectPortfolioShare
  ( -- * Creating a Request
    RejectPortfolioShare (..),
    newRejectPortfolioShare,

    -- * Request Lenses
    rejectPortfolioShare_portfolioShareType,
    rejectPortfolioShare_acceptLanguage,
    rejectPortfolioShare_portfolioId,

    -- * Destructuring the Response
    RejectPortfolioShareResponse (..),
    newRejectPortfolioShareResponse,

    -- * Response Lenses
    rejectPortfolioShareResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newRejectPortfolioShare' smart constructor.
data RejectPortfolioShare = RejectPortfolioShare'
  { -- | The type of shared portfolios to reject. The default is to reject
    -- imported portfolios.
    --
    -- -   @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management
    --     account of your organization.
    --
    -- -   @IMPORTED@ - Reject imported portfolios.
    --
    -- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
    --     ResourceNotFoundException.)
    --
    -- For example,
    -- @aws servicecatalog reject-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
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
-- Create a value of 'RejectPortfolioShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioShareType', 'rejectPortfolioShare_portfolioShareType' - The type of shared portfolios to reject. The default is to reject
-- imported portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management
--     account of your organization.
--
-- -   @IMPORTED@ - Reject imported portfolios.
--
-- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
--     ResourceNotFoundException.)
--
-- For example,
-- @aws servicecatalog reject-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
--
-- 'acceptLanguage', 'rejectPortfolioShare_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'rejectPortfolioShare_portfolioId' - The portfolio identifier.
newRejectPortfolioShare ::
  -- | 'portfolioId'
  Core.Text ->
  RejectPortfolioShare
newRejectPortfolioShare pPortfolioId_ =
  RejectPortfolioShare'
    { portfolioShareType =
        Core.Nothing,
      acceptLanguage = Core.Nothing,
      portfolioId = pPortfolioId_
    }

-- | The type of shared portfolios to reject. The default is to reject
-- imported portfolios.
--
-- -   @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management
--     account of your organization.
--
-- -   @IMPORTED@ - Reject imported portfolios.
--
-- -   @AWS_SERVICECATALOG@ - Not supported. (Throws
--     ResourceNotFoundException.)
--
-- For example,
-- @aws servicecatalog reject-portfolio-share --portfolio-id \"port-2qwzkwxt3y5fk\" --portfolio-share-type AWS_ORGANIZATIONS@
rejectPortfolioShare_portfolioShareType :: Lens.Lens' RejectPortfolioShare (Core.Maybe PortfolioShareType)
rejectPortfolioShare_portfolioShareType = Lens.lens (\RejectPortfolioShare' {portfolioShareType} -> portfolioShareType) (\s@RejectPortfolioShare' {} a -> s {portfolioShareType = a} :: RejectPortfolioShare)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
rejectPortfolioShare_acceptLanguage :: Lens.Lens' RejectPortfolioShare (Core.Maybe Core.Text)
rejectPortfolioShare_acceptLanguage = Lens.lens (\RejectPortfolioShare' {acceptLanguage} -> acceptLanguage) (\s@RejectPortfolioShare' {} a -> s {acceptLanguage = a} :: RejectPortfolioShare)

-- | The portfolio identifier.
rejectPortfolioShare_portfolioId :: Lens.Lens' RejectPortfolioShare Core.Text
rejectPortfolioShare_portfolioId = Lens.lens (\RejectPortfolioShare' {portfolioId} -> portfolioId) (\s@RejectPortfolioShare' {} a -> s {portfolioId = a} :: RejectPortfolioShare)

instance Core.AWSRequest RejectPortfolioShare where
  type
    AWSResponse RejectPortfolioShare =
      RejectPortfolioShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectPortfolioShareResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RejectPortfolioShare

instance Core.NFData RejectPortfolioShare

instance Core.ToHeaders RejectPortfolioShare where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.RejectPortfolioShare" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RejectPortfolioShare where
  toJSON RejectPortfolioShare' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PortfolioShareType" Core..=)
              Core.<$> portfolioShareType,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("PortfolioId" Core..= portfolioId)
          ]
      )

instance Core.ToPath RejectPortfolioShare where
  toPath = Core.const "/"

instance Core.ToQuery RejectPortfolioShare where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRejectPortfolioShareResponse' smart constructor.
data RejectPortfolioShareResponse = RejectPortfolioShareResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RejectPortfolioShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectPortfolioShareResponse_httpStatus' - The response's http status code.
newRejectPortfolioShareResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RejectPortfolioShareResponse
newRejectPortfolioShareResponse pHttpStatus_ =
  RejectPortfolioShareResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectPortfolioShareResponse_httpStatus :: Lens.Lens' RejectPortfolioShareResponse Core.Int
rejectPortfolioShareResponse_httpStatus = Lens.lens (\RejectPortfolioShareResponse' {httpStatus} -> httpStatus) (\s@RejectPortfolioShareResponse' {} a -> s {httpStatus = a} :: RejectPortfolioShareResponse)

instance Core.NFData RejectPortfolioShareResponse
