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
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.CreatePortfolio
  ( -- * Creating a Request
    CreatePortfolio (..),
    newCreatePortfolio,

    -- * Request Lenses
    createPortfolio_tags,
    createPortfolio_description,
    createPortfolio_acceptLanguage,
    createPortfolio_displayName,
    createPortfolio_providerName,
    createPortfolio_idempotencyToken,

    -- * Destructuring the Response
    CreatePortfolioResponse (..),
    newCreatePortfolioResponse,

    -- * Response Lenses
    createPortfolioResponse_tags,
    createPortfolioResponse_portfolioDetail,
    createPortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreatePortfolio' smart constructor.
data CreatePortfolio = CreatePortfolio'
  { -- | One or more tags.
    tags :: Core.Maybe [Tag],
    -- | The description of the portfolio.
    description :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The name to use for display purposes.
    displayName :: Core.Text,
    -- | The name of the portfolio provider.
    providerName :: Core.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPortfolio_tags' - One or more tags.
--
-- 'description', 'createPortfolio_description' - The description of the portfolio.
--
-- 'acceptLanguage', 'createPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'displayName', 'createPortfolio_displayName' - The name to use for display purposes.
--
-- 'providerName', 'createPortfolio_providerName' - The name of the portfolio provider.
--
-- 'idempotencyToken', 'createPortfolio_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
newCreatePortfolio ::
  -- | 'displayName'
  Core.Text ->
  -- | 'providerName'
  Core.Text ->
  -- | 'idempotencyToken'
  Core.Text ->
  CreatePortfolio
newCreatePortfolio
  pDisplayName_
  pProviderName_
  pIdempotencyToken_ =
    CreatePortfolio'
      { tags = Core.Nothing,
        description = Core.Nothing,
        acceptLanguage = Core.Nothing,
        displayName = pDisplayName_,
        providerName = pProviderName_,
        idempotencyToken = pIdempotencyToken_
      }

-- | One or more tags.
createPortfolio_tags :: Lens.Lens' CreatePortfolio (Core.Maybe [Tag])
createPortfolio_tags = Lens.lens (\CreatePortfolio' {tags} -> tags) (\s@CreatePortfolio' {} a -> s {tags = a} :: CreatePortfolio) Core.. Lens.mapping Lens._Coerce

-- | The description of the portfolio.
createPortfolio_description :: Lens.Lens' CreatePortfolio (Core.Maybe Core.Text)
createPortfolio_description = Lens.lens (\CreatePortfolio' {description} -> description) (\s@CreatePortfolio' {} a -> s {description = a} :: CreatePortfolio)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createPortfolio_acceptLanguage :: Lens.Lens' CreatePortfolio (Core.Maybe Core.Text)
createPortfolio_acceptLanguage = Lens.lens (\CreatePortfolio' {acceptLanguage} -> acceptLanguage) (\s@CreatePortfolio' {} a -> s {acceptLanguage = a} :: CreatePortfolio)

-- | The name to use for display purposes.
createPortfolio_displayName :: Lens.Lens' CreatePortfolio Core.Text
createPortfolio_displayName = Lens.lens (\CreatePortfolio' {displayName} -> displayName) (\s@CreatePortfolio' {} a -> s {displayName = a} :: CreatePortfolio)

-- | The name of the portfolio provider.
createPortfolio_providerName :: Lens.Lens' CreatePortfolio Core.Text
createPortfolio_providerName = Lens.lens (\CreatePortfolio' {providerName} -> providerName) (\s@CreatePortfolio' {} a -> s {providerName = a} :: CreatePortfolio)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createPortfolio_idempotencyToken :: Lens.Lens' CreatePortfolio Core.Text
createPortfolio_idempotencyToken = Lens.lens (\CreatePortfolio' {idempotencyToken} -> idempotencyToken) (\s@CreatePortfolio' {} a -> s {idempotencyToken = a} :: CreatePortfolio)

instance Core.AWSRequest CreatePortfolio where
  type
    AWSResponse CreatePortfolio =
      CreatePortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePortfolioResponse'
            Core.<$> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "PortfolioDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePortfolio

instance Core.NFData CreatePortfolio

instance Core.ToHeaders CreatePortfolio where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CreatePortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePortfolio where
  toJSON CreatePortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("DisplayName" Core..= displayName),
            Core.Just ("ProviderName" Core..= providerName),
            Core.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreatePortfolio where
  toPath = Core.const "/"

instance Core.ToQuery CreatePortfolio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePortfolioResponse' smart constructor.
data CreatePortfolioResponse = CreatePortfolioResponse'
  { -- | Information about the tags associated with the portfolio.
    tags :: Core.Maybe [Tag],
    -- | Information about the portfolio.
    portfolioDetail :: Core.Maybe PortfolioDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPortfolioResponse_tags' - Information about the tags associated with the portfolio.
--
-- 'portfolioDetail', 'createPortfolioResponse_portfolioDetail' - Information about the portfolio.
--
-- 'httpStatus', 'createPortfolioResponse_httpStatus' - The response's http status code.
newCreatePortfolioResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePortfolioResponse
newCreatePortfolioResponse pHttpStatus_ =
  CreatePortfolioResponse'
    { tags = Core.Nothing,
      portfolioDetail = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags associated with the portfolio.
createPortfolioResponse_tags :: Lens.Lens' CreatePortfolioResponse (Core.Maybe [Tag])
createPortfolioResponse_tags = Lens.lens (\CreatePortfolioResponse' {tags} -> tags) (\s@CreatePortfolioResponse' {} a -> s {tags = a} :: CreatePortfolioResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the portfolio.
createPortfolioResponse_portfolioDetail :: Lens.Lens' CreatePortfolioResponse (Core.Maybe PortfolioDetail)
createPortfolioResponse_portfolioDetail = Lens.lens (\CreatePortfolioResponse' {portfolioDetail} -> portfolioDetail) (\s@CreatePortfolioResponse' {} a -> s {portfolioDetail = a} :: CreatePortfolioResponse)

-- | The response's http status code.
createPortfolioResponse_httpStatus :: Lens.Lens' CreatePortfolioResponse Core.Int
createPortfolioResponse_httpStatus = Lens.lens (\CreatePortfolioResponse' {httpStatus} -> httpStatus) (\s@CreatePortfolioResponse' {} a -> s {httpStatus = a} :: CreatePortfolioResponse)

instance Core.NFData CreatePortfolioResponse
