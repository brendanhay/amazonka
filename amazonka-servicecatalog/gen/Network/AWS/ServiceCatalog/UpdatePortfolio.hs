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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdatePortfolio' smart constructor.
data UpdatePortfolio = UpdatePortfolio'
  { -- | The tags to remove.
    removeTags :: Prelude.Maybe [Prelude.Text],
    -- | The updated name of the portfolio provider.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The tags to add.
    addTags :: Prelude.Maybe [Tag],
    -- | The updated description of the portfolio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name to use for display purposes.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdatePortfolio
newUpdatePortfolio pId_ =
  UpdatePortfolio'
    { removeTags = Prelude.Nothing,
      providerName = Prelude.Nothing,
      addTags = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      id = pId_
    }

-- | The tags to remove.
updatePortfolio_removeTags :: Lens.Lens' UpdatePortfolio (Prelude.Maybe [Prelude.Text])
updatePortfolio_removeTags = Lens.lens (\UpdatePortfolio' {removeTags} -> removeTags) (\s@UpdatePortfolio' {} a -> s {removeTags = a} :: UpdatePortfolio) Prelude.. Lens.mapping Lens._Coerce

-- | The updated name of the portfolio provider.
updatePortfolio_providerName :: Lens.Lens' UpdatePortfolio (Prelude.Maybe Prelude.Text)
updatePortfolio_providerName = Lens.lens (\UpdatePortfolio' {providerName} -> providerName) (\s@UpdatePortfolio' {} a -> s {providerName = a} :: UpdatePortfolio)

-- | The tags to add.
updatePortfolio_addTags :: Lens.Lens' UpdatePortfolio (Prelude.Maybe [Tag])
updatePortfolio_addTags = Lens.lens (\UpdatePortfolio' {addTags} -> addTags) (\s@UpdatePortfolio' {} a -> s {addTags = a} :: UpdatePortfolio) Prelude.. Lens.mapping Lens._Coerce

-- | The updated description of the portfolio.
updatePortfolio_description :: Lens.Lens' UpdatePortfolio (Prelude.Maybe Prelude.Text)
updatePortfolio_description = Lens.lens (\UpdatePortfolio' {description} -> description) (\s@UpdatePortfolio' {} a -> s {description = a} :: UpdatePortfolio)

-- | The name to use for display purposes.
updatePortfolio_displayName :: Lens.Lens' UpdatePortfolio (Prelude.Maybe Prelude.Text)
updatePortfolio_displayName = Lens.lens (\UpdatePortfolio' {displayName} -> displayName) (\s@UpdatePortfolio' {} a -> s {displayName = a} :: UpdatePortfolio)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updatePortfolio_acceptLanguage :: Lens.Lens' UpdatePortfolio (Prelude.Maybe Prelude.Text)
updatePortfolio_acceptLanguage = Lens.lens (\UpdatePortfolio' {acceptLanguage} -> acceptLanguage) (\s@UpdatePortfolio' {} a -> s {acceptLanguage = a} :: UpdatePortfolio)

-- | The portfolio identifier.
updatePortfolio_id :: Lens.Lens' UpdatePortfolio Prelude.Text
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
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "PortfolioDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePortfolio

instance Prelude.NFData UpdatePortfolio

instance Core.ToHeaders UpdatePortfolio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdatePortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdatePortfolio where
  toJSON UpdatePortfolio' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RemoveTags" Core..=) Prelude.<$> removeTags,
            ("ProviderName" Core..=) Prelude.<$> providerName,
            ("AddTags" Core..=) Prelude.<$> addTags,
            ("Description" Core..=) Prelude.<$> description,
            ("DisplayName" Core..=) Prelude.<$> displayName,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath UpdatePortfolio where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdatePortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePortfolioResponse' smart constructor.
data UpdatePortfolioResponse = UpdatePortfolioResponse'
  { -- | Information about the tags associated with the portfolio.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the portfolio.
    portfolioDetail :: Prelude.Maybe PortfolioDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdatePortfolioResponse
newUpdatePortfolioResponse pHttpStatus_ =
  UpdatePortfolioResponse'
    { tags = Prelude.Nothing,
      portfolioDetail = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags associated with the portfolio.
updatePortfolioResponse_tags :: Lens.Lens' UpdatePortfolioResponse (Prelude.Maybe [Tag])
updatePortfolioResponse_tags = Lens.lens (\UpdatePortfolioResponse' {tags} -> tags) (\s@UpdatePortfolioResponse' {} a -> s {tags = a} :: UpdatePortfolioResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the portfolio.
updatePortfolioResponse_portfolioDetail :: Lens.Lens' UpdatePortfolioResponse (Prelude.Maybe PortfolioDetail)
updatePortfolioResponse_portfolioDetail = Lens.lens (\UpdatePortfolioResponse' {portfolioDetail} -> portfolioDetail) (\s@UpdatePortfolioResponse' {} a -> s {portfolioDetail = a} :: UpdatePortfolioResponse)

-- | The response's http status code.
updatePortfolioResponse_httpStatus :: Lens.Lens' UpdatePortfolioResponse Prelude.Int
updatePortfolioResponse_httpStatus = Lens.lens (\UpdatePortfolioResponse' {httpStatus} -> httpStatus) (\s@UpdatePortfolioResponse' {} a -> s {httpStatus = a} :: UpdatePortfolioResponse)

instance Prelude.NFData UpdatePortfolioResponse
