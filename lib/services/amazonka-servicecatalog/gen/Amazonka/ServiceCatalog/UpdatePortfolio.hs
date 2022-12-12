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
-- Module      : Amazonka.ServiceCatalog.UpdatePortfolio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified portfolio.
--
-- You cannot update a product that was shared with you.
module Amazonka.ServiceCatalog.UpdatePortfolio
  ( -- * Creating a Request
    UpdatePortfolio (..),
    newUpdatePortfolio,

    -- * Request Lenses
    updatePortfolio_acceptLanguage,
    updatePortfolio_addTags,
    updatePortfolio_description,
    updatePortfolio_displayName,
    updatePortfolio_providerName,
    updatePortfolio_removeTags,
    updatePortfolio_id,

    -- * Destructuring the Response
    UpdatePortfolioResponse (..),
    newUpdatePortfolioResponse,

    -- * Response Lenses
    updatePortfolioResponse_portfolioDetail,
    updatePortfolioResponse_tags,
    updatePortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newUpdatePortfolio' smart constructor.
data UpdatePortfolio = UpdatePortfolio'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The tags to add.
    addTags :: Prelude.Maybe [Tag],
    -- | The updated description of the portfolio.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name to use for display purposes.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The updated name of the portfolio provider.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The tags to remove.
    removeTags :: Prelude.Maybe [Prelude.Text],
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
-- 'acceptLanguage', 'updatePortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'addTags', 'updatePortfolio_addTags' - The tags to add.
--
-- 'description', 'updatePortfolio_description' - The updated description of the portfolio.
--
-- 'displayName', 'updatePortfolio_displayName' - The name to use for display purposes.
--
-- 'providerName', 'updatePortfolio_providerName' - The updated name of the portfolio provider.
--
-- 'removeTags', 'updatePortfolio_removeTags' - The tags to remove.
--
-- 'id', 'updatePortfolio_id' - The portfolio identifier.
newUpdatePortfolio ::
  -- | 'id'
  Prelude.Text ->
  UpdatePortfolio
newUpdatePortfolio pId_ =
  UpdatePortfolio'
    { acceptLanguage = Prelude.Nothing,
      addTags = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      providerName = Prelude.Nothing,
      removeTags = Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updatePortfolio_acceptLanguage :: Lens.Lens' UpdatePortfolio (Prelude.Maybe Prelude.Text)
updatePortfolio_acceptLanguage = Lens.lens (\UpdatePortfolio' {acceptLanguage} -> acceptLanguage) (\s@UpdatePortfolio' {} a -> s {acceptLanguage = a} :: UpdatePortfolio)

-- | The tags to add.
updatePortfolio_addTags :: Lens.Lens' UpdatePortfolio (Prelude.Maybe [Tag])
updatePortfolio_addTags = Lens.lens (\UpdatePortfolio' {addTags} -> addTags) (\s@UpdatePortfolio' {} a -> s {addTags = a} :: UpdatePortfolio) Prelude.. Lens.mapping Lens.coerced

-- | The updated description of the portfolio.
updatePortfolio_description :: Lens.Lens' UpdatePortfolio (Prelude.Maybe Prelude.Text)
updatePortfolio_description = Lens.lens (\UpdatePortfolio' {description} -> description) (\s@UpdatePortfolio' {} a -> s {description = a} :: UpdatePortfolio)

-- | The name to use for display purposes.
updatePortfolio_displayName :: Lens.Lens' UpdatePortfolio (Prelude.Maybe Prelude.Text)
updatePortfolio_displayName = Lens.lens (\UpdatePortfolio' {displayName} -> displayName) (\s@UpdatePortfolio' {} a -> s {displayName = a} :: UpdatePortfolio)

-- | The updated name of the portfolio provider.
updatePortfolio_providerName :: Lens.Lens' UpdatePortfolio (Prelude.Maybe Prelude.Text)
updatePortfolio_providerName = Lens.lens (\UpdatePortfolio' {providerName} -> providerName) (\s@UpdatePortfolio' {} a -> s {providerName = a} :: UpdatePortfolio)

-- | The tags to remove.
updatePortfolio_removeTags :: Lens.Lens' UpdatePortfolio (Prelude.Maybe [Prelude.Text])
updatePortfolio_removeTags = Lens.lens (\UpdatePortfolio' {removeTags} -> removeTags) (\s@UpdatePortfolio' {} a -> s {removeTags = a} :: UpdatePortfolio) Prelude.. Lens.mapping Lens.coerced

-- | The portfolio identifier.
updatePortfolio_id :: Lens.Lens' UpdatePortfolio Prelude.Text
updatePortfolio_id = Lens.lens (\UpdatePortfolio' {id} -> id) (\s@UpdatePortfolio' {} a -> s {id = a} :: UpdatePortfolio)

instance Core.AWSRequest UpdatePortfolio where
  type
    AWSResponse UpdatePortfolio =
      UpdatePortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePortfolioResponse'
            Prelude.<$> (x Data..?> "PortfolioDetail")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePortfolio where
  hashWithSalt _salt UpdatePortfolio' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` addTags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` removeTags
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdatePortfolio where
  rnf UpdatePortfolio' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf addTags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf removeTags
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdatePortfolio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.UpdatePortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePortfolio where
  toJSON UpdatePortfolio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("AddTags" Data..=) Prelude.<$> addTags,
            ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("ProviderName" Data..=) Prelude.<$> providerName,
            ("RemoveTags" Data..=) Prelude.<$> removeTags,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath UpdatePortfolio where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePortfolioResponse' smart constructor.
data UpdatePortfolioResponse = UpdatePortfolioResponse'
  { -- | Information about the portfolio.
    portfolioDetail :: Prelude.Maybe PortfolioDetail,
    -- | Information about the tags associated with the portfolio.
    tags :: Prelude.Maybe [Tag],
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
-- 'portfolioDetail', 'updatePortfolioResponse_portfolioDetail' - Information about the portfolio.
--
-- 'tags', 'updatePortfolioResponse_tags' - Information about the tags associated with the portfolio.
--
-- 'httpStatus', 'updatePortfolioResponse_httpStatus' - The response's http status code.
newUpdatePortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePortfolioResponse
newUpdatePortfolioResponse pHttpStatus_ =
  UpdatePortfolioResponse'
    { portfolioDetail =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the portfolio.
updatePortfolioResponse_portfolioDetail :: Lens.Lens' UpdatePortfolioResponse (Prelude.Maybe PortfolioDetail)
updatePortfolioResponse_portfolioDetail = Lens.lens (\UpdatePortfolioResponse' {portfolioDetail} -> portfolioDetail) (\s@UpdatePortfolioResponse' {} a -> s {portfolioDetail = a} :: UpdatePortfolioResponse)

-- | Information about the tags associated with the portfolio.
updatePortfolioResponse_tags :: Lens.Lens' UpdatePortfolioResponse (Prelude.Maybe [Tag])
updatePortfolioResponse_tags = Lens.lens (\UpdatePortfolioResponse' {tags} -> tags) (\s@UpdatePortfolioResponse' {} a -> s {tags = a} :: UpdatePortfolioResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updatePortfolioResponse_httpStatus :: Lens.Lens' UpdatePortfolioResponse Prelude.Int
updatePortfolioResponse_httpStatus = Lens.lens (\UpdatePortfolioResponse' {httpStatus} -> httpStatus) (\s@UpdatePortfolioResponse' {} a -> s {httpStatus = a} :: UpdatePortfolioResponse)

instance Prelude.NFData UpdatePortfolioResponse where
  rnf UpdatePortfolioResponse' {..} =
    Prelude.rnf portfolioDetail
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
