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
-- Module      : Network.AWS.ServiceCatalog.UpdatePortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified portfolio.
--
-- You cannot update a product that was shared with you.
module Network.AWS.ServiceCatalog.UpdatePortfolio
  ( -- * Creating a Request
    UpdatePortfolio (..),
    newUpdatePortfolio,

    -- * Request Lenses
    updatePortfolio_removeTags,
    updatePortfolio_providerName,
    updatePortfolio_addTags,
    updatePortfolio_description,
    updatePortfolio_displayName,
    updatePortfolio_acceptLanguage,
    updatePortfolio_id,

    -- * Destructuring the Response
    UpdatePortfolioResponse (..),
    newUpdatePortfolioResponse,

    -- * Response Lenses
    updatePortfolioResponse_tags,
    updatePortfolioResponse_portfolioDetail,
    updatePortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdatePortfolio' smart constructor.
data UpdatePortfolio = UpdatePortfolio'
  { -- | The tags to remove.
    removeTags :: Core.Maybe [Core.Text],
    -- | The updated name of the portfolio provider.
    providerName :: Core.Maybe Core.Text,
    -- | The tags to add.
    addTags :: Core.Maybe [Tag],
    -- | The updated description of the portfolio.
    description :: Core.Maybe Core.Text,
    -- | The name to use for display purposes.
    displayName :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The portfolio identifier.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeTags', 'updatePortfolio_removeTags' - The tags to remove.
--
-- 'providerName', 'updatePortfolio_providerName' - The updated name of the portfolio provider.
--
-- 'addTags', 'updatePortfolio_addTags' - The tags to add.
--
-- 'description', 'updatePortfolio_description' - The updated description of the portfolio.
--
-- 'displayName', 'updatePortfolio_displayName' - The name to use for display purposes.
--
-- 'acceptLanguage', 'updatePortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'updatePortfolio_id' - The portfolio identifier.
newUpdatePortfolio ::
  -- | 'id'
  Core.Text ->
  UpdatePortfolio
newUpdatePortfolio pId_ =
  UpdatePortfolio'
    { removeTags = Core.Nothing,
      providerName = Core.Nothing,
      addTags = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The tags to remove.
updatePortfolio_removeTags :: Lens.Lens' UpdatePortfolio (Core.Maybe [Core.Text])
updatePortfolio_removeTags = Lens.lens (\UpdatePortfolio' {removeTags} -> removeTags) (\s@UpdatePortfolio' {} a -> s {removeTags = a} :: UpdatePortfolio) Core.. Lens.mapping Lens._Coerce

-- | The updated name of the portfolio provider.
updatePortfolio_providerName :: Lens.Lens' UpdatePortfolio (Core.Maybe Core.Text)
updatePortfolio_providerName = Lens.lens (\UpdatePortfolio' {providerName} -> providerName) (\s@UpdatePortfolio' {} a -> s {providerName = a} :: UpdatePortfolio)

-- | The tags to add.
updatePortfolio_addTags :: Lens.Lens' UpdatePortfolio (Core.Maybe [Tag])
updatePortfolio_addTags = Lens.lens (\UpdatePortfolio' {addTags} -> addTags) (\s@UpdatePortfolio' {} a -> s {addTags = a} :: UpdatePortfolio) Core.. Lens.mapping Lens._Coerce

-- | The updated description of the portfolio.
updatePortfolio_description :: Lens.Lens' UpdatePortfolio (Core.Maybe Core.Text)
updatePortfolio_description = Lens.lens (\UpdatePortfolio' {description} -> description) (\s@UpdatePortfolio' {} a -> s {description = a} :: UpdatePortfolio)

-- | The name to use for display purposes.
updatePortfolio_displayName :: Lens.Lens' UpdatePortfolio (Core.Maybe Core.Text)
updatePortfolio_displayName = Lens.lens (\UpdatePortfolio' {displayName} -> displayName) (\s@UpdatePortfolio' {} a -> s {displayName = a} :: UpdatePortfolio)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updatePortfolio_acceptLanguage :: Lens.Lens' UpdatePortfolio (Core.Maybe Core.Text)
updatePortfolio_acceptLanguage = Lens.lens (\UpdatePortfolio' {acceptLanguage} -> acceptLanguage) (\s@UpdatePortfolio' {} a -> s {acceptLanguage = a} :: UpdatePortfolio)

-- | The portfolio identifier.
updatePortfolio_id :: Lens.Lens' UpdatePortfolio Core.Text
updatePortfolio_id = Lens.lens (\UpdatePortfolio' {id} -> id) (\s@UpdatePortfolio' {} a -> s {id = a} :: UpdatePortfolio)

instance Core.AWSRequest UpdatePortfolio where
  type
    AWSResponse UpdatePortfolio =
      UpdatePortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePortfolioResponse'
            Core.<$> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "PortfolioDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdatePortfolio

instance Core.NFData UpdatePortfolio

instance Core.ToHeaders UpdatePortfolio where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdatePortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdatePortfolio where
  toJSON UpdatePortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RemoveTags" Core..=) Core.<$> removeTags,
            ("ProviderName" Core..=) Core.<$> providerName,
            ("AddTags" Core..=) Core.<$> addTags,
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath UpdatePortfolio where
  toPath = Core.const "/"

instance Core.ToQuery UpdatePortfolio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePortfolioResponse' smart constructor.
data UpdatePortfolioResponse = UpdatePortfolioResponse'
  { -- | Information about the tags associated with the portfolio.
    tags :: Core.Maybe [Tag],
    -- | Information about the portfolio.
    portfolioDetail :: Core.Maybe PortfolioDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updatePortfolioResponse_tags' - Information about the tags associated with the portfolio.
--
-- 'portfolioDetail', 'updatePortfolioResponse_portfolioDetail' - Information about the portfolio.
--
-- 'httpStatus', 'updatePortfolioResponse_httpStatus' - The response's http status code.
newUpdatePortfolioResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdatePortfolioResponse
newUpdatePortfolioResponse pHttpStatus_ =
  UpdatePortfolioResponse'
    { tags = Core.Nothing,
      portfolioDetail = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags associated with the portfolio.
updatePortfolioResponse_tags :: Lens.Lens' UpdatePortfolioResponse (Core.Maybe [Tag])
updatePortfolioResponse_tags = Lens.lens (\UpdatePortfolioResponse' {tags} -> tags) (\s@UpdatePortfolioResponse' {} a -> s {tags = a} :: UpdatePortfolioResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the portfolio.
updatePortfolioResponse_portfolioDetail :: Lens.Lens' UpdatePortfolioResponse (Core.Maybe PortfolioDetail)
updatePortfolioResponse_portfolioDetail = Lens.lens (\UpdatePortfolioResponse' {portfolioDetail} -> portfolioDetail) (\s@UpdatePortfolioResponse' {} a -> s {portfolioDetail = a} :: UpdatePortfolioResponse)

-- | The response's http status code.
updatePortfolioResponse_httpStatus :: Lens.Lens' UpdatePortfolioResponse Core.Int
updatePortfolioResponse_httpStatus = Lens.lens (\UpdatePortfolioResponse' {httpStatus} -> httpStatus) (\s@UpdatePortfolioResponse' {} a -> s {httpStatus = a} :: UpdatePortfolioResponse)

instance Core.NFData UpdatePortfolioResponse
