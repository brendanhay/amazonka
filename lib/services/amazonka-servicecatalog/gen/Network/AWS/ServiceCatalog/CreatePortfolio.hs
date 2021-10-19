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
    createPortfolio_acceptLanguage,
    createPortfolio_description,
    createPortfolio_tags,
    createPortfolio_displayName,
    createPortfolio_providerName,
    createPortfolio_idempotencyToken,

    -- * Destructuring the Response
    CreatePortfolioResponse (..),
    newCreatePortfolioResponse,

    -- * Response Lenses
    createPortfolioResponse_portfolioDetail,
    createPortfolioResponse_tags,
    createPortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreatePortfolio' smart constructor.
data CreatePortfolio = CreatePortfolio'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The description of the portfolio.
    description :: Prelude.Maybe Prelude.Text,
    -- | One or more tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name to use for display purposes.
    displayName :: Prelude.Text,
    -- | The name of the portfolio provider.
    providerName :: Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'createPortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'description', 'createPortfolio_description' - The description of the portfolio.
--
-- 'tags', 'createPortfolio_tags' - One or more tags.
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
  Prelude.Text ->
  -- | 'providerName'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreatePortfolio
newCreatePortfolio
  pDisplayName_
  pProviderName_
  pIdempotencyToken_ =
    CreatePortfolio'
      { acceptLanguage = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        displayName = pDisplayName_,
        providerName = pProviderName_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createPortfolio_acceptLanguage :: Lens.Lens' CreatePortfolio (Prelude.Maybe Prelude.Text)
createPortfolio_acceptLanguage = Lens.lens (\CreatePortfolio' {acceptLanguage} -> acceptLanguage) (\s@CreatePortfolio' {} a -> s {acceptLanguage = a} :: CreatePortfolio)

-- | The description of the portfolio.
createPortfolio_description :: Lens.Lens' CreatePortfolio (Prelude.Maybe Prelude.Text)
createPortfolio_description = Lens.lens (\CreatePortfolio' {description} -> description) (\s@CreatePortfolio' {} a -> s {description = a} :: CreatePortfolio)

-- | One or more tags.
createPortfolio_tags :: Lens.Lens' CreatePortfolio (Prelude.Maybe [Tag])
createPortfolio_tags = Lens.lens (\CreatePortfolio' {tags} -> tags) (\s@CreatePortfolio' {} a -> s {tags = a} :: CreatePortfolio) Prelude.. Lens.mapping Lens.coerced

-- | The name to use for display purposes.
createPortfolio_displayName :: Lens.Lens' CreatePortfolio Prelude.Text
createPortfolio_displayName = Lens.lens (\CreatePortfolio' {displayName} -> displayName) (\s@CreatePortfolio' {} a -> s {displayName = a} :: CreatePortfolio)

-- | The name of the portfolio provider.
createPortfolio_providerName :: Lens.Lens' CreatePortfolio Prelude.Text
createPortfolio_providerName = Lens.lens (\CreatePortfolio' {providerName} -> providerName) (\s@CreatePortfolio' {} a -> s {providerName = a} :: CreatePortfolio)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createPortfolio_idempotencyToken :: Lens.Lens' CreatePortfolio Prelude.Text
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
            Prelude.<$> (x Core..?> "PortfolioDetail")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePortfolio

instance Prelude.NFData CreatePortfolio

instance Core.ToHeaders CreatePortfolio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CreatePortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePortfolio where
  toJSON CreatePortfolio' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("DisplayName" Core..= displayName),
            Prelude.Just ("ProviderName" Core..= providerName),
            Prelude.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreatePortfolio where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePortfolioResponse' smart constructor.
data CreatePortfolioResponse = CreatePortfolioResponse'
  { -- | Information about the portfolio.
    portfolioDetail :: Prelude.Maybe PortfolioDetail,
    -- | Information about the tags associated with the portfolio.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portfolioDetail', 'createPortfolioResponse_portfolioDetail' - Information about the portfolio.
--
-- 'tags', 'createPortfolioResponse_tags' - Information about the tags associated with the portfolio.
--
-- 'httpStatus', 'createPortfolioResponse_httpStatus' - The response's http status code.
newCreatePortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePortfolioResponse
newCreatePortfolioResponse pHttpStatus_ =
  CreatePortfolioResponse'
    { portfolioDetail =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the portfolio.
createPortfolioResponse_portfolioDetail :: Lens.Lens' CreatePortfolioResponse (Prelude.Maybe PortfolioDetail)
createPortfolioResponse_portfolioDetail = Lens.lens (\CreatePortfolioResponse' {portfolioDetail} -> portfolioDetail) (\s@CreatePortfolioResponse' {} a -> s {portfolioDetail = a} :: CreatePortfolioResponse)

-- | Information about the tags associated with the portfolio.
createPortfolioResponse_tags :: Lens.Lens' CreatePortfolioResponse (Prelude.Maybe [Tag])
createPortfolioResponse_tags = Lens.lens (\CreatePortfolioResponse' {tags} -> tags) (\s@CreatePortfolioResponse' {} a -> s {tags = a} :: CreatePortfolioResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createPortfolioResponse_httpStatus :: Lens.Lens' CreatePortfolioResponse Prelude.Int
createPortfolioResponse_httpStatus = Lens.lens (\CreatePortfolioResponse' {httpStatus} -> httpStatus) (\s@CreatePortfolioResponse' {} a -> s {httpStatus = a} :: CreatePortfolioResponse)

instance Prelude.NFData CreatePortfolioResponse
