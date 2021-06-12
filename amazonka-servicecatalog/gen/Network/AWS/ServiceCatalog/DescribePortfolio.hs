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
-- Module      : Network.AWS.ServiceCatalog.DescribePortfolio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DescribePortfolio
  ( -- * Creating a Request
    DescribePortfolio (..),
    newDescribePortfolio,

    -- * Request Lenses
    describePortfolio_acceptLanguage,
    describePortfolio_id,

    -- * Destructuring the Response
    DescribePortfolioResponse (..),
    newDescribePortfolioResponse,

    -- * Response Lenses
    describePortfolioResponse_tags,
    describePortfolioResponse_budgets,
    describePortfolioResponse_portfolioDetail,
    describePortfolioResponse_tagOptions,
    describePortfolioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribePortfolio' smart constructor.
data DescribePortfolio = DescribePortfolio'
  { -- | The language code.
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
-- Create a value of 'DescribePortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describePortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'describePortfolio_id' - The portfolio identifier.
newDescribePortfolio ::
  -- | 'id'
  Core.Text ->
  DescribePortfolio
newDescribePortfolio pId_ =
  DescribePortfolio'
    { acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describePortfolio_acceptLanguage :: Lens.Lens' DescribePortfolio (Core.Maybe Core.Text)
describePortfolio_acceptLanguage = Lens.lens (\DescribePortfolio' {acceptLanguage} -> acceptLanguage) (\s@DescribePortfolio' {} a -> s {acceptLanguage = a} :: DescribePortfolio)

-- | The portfolio identifier.
describePortfolio_id :: Lens.Lens' DescribePortfolio Core.Text
describePortfolio_id = Lens.lens (\DescribePortfolio' {id} -> id) (\s@DescribePortfolio' {} a -> s {id = a} :: DescribePortfolio)

instance Core.AWSRequest DescribePortfolio where
  type
    AWSResponse DescribePortfolio =
      DescribePortfolioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePortfolioResponse'
            Core.<$> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Budgets" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "PortfolioDetail")
            Core.<*> (x Core..?> "TagOptions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePortfolio

instance Core.NFData DescribePortfolio

instance Core.ToHeaders DescribePortfolio where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribePortfolio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePortfolio where
  toJSON DescribePortfolio' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DescribePortfolio where
  toPath = Core.const "/"

instance Core.ToQuery DescribePortfolio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePortfolioResponse' smart constructor.
data DescribePortfolioResponse = DescribePortfolioResponse'
  { -- | Information about the tags associated with the portfolio.
    tags :: Core.Maybe [Tag],
    -- | Information about the associated budgets.
    budgets :: Core.Maybe [BudgetDetail],
    -- | Information about the portfolio.
    portfolioDetail :: Core.Maybe PortfolioDetail,
    -- | Information about the TagOptions associated with the portfolio.
    tagOptions :: Core.Maybe [TagOptionDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describePortfolioResponse_tags' - Information about the tags associated with the portfolio.
--
-- 'budgets', 'describePortfolioResponse_budgets' - Information about the associated budgets.
--
-- 'portfolioDetail', 'describePortfolioResponse_portfolioDetail' - Information about the portfolio.
--
-- 'tagOptions', 'describePortfolioResponse_tagOptions' - Information about the TagOptions associated with the portfolio.
--
-- 'httpStatus', 'describePortfolioResponse_httpStatus' - The response's http status code.
newDescribePortfolioResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePortfolioResponse
newDescribePortfolioResponse pHttpStatus_ =
  DescribePortfolioResponse'
    { tags = Core.Nothing,
      budgets = Core.Nothing,
      portfolioDetail = Core.Nothing,
      tagOptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags associated with the portfolio.
describePortfolioResponse_tags :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [Tag])
describePortfolioResponse_tags = Lens.lens (\DescribePortfolioResponse' {tags} -> tags) (\s@DescribePortfolioResponse' {} a -> s {tags = a} :: DescribePortfolioResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the associated budgets.
describePortfolioResponse_budgets :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [BudgetDetail])
describePortfolioResponse_budgets = Lens.lens (\DescribePortfolioResponse' {budgets} -> budgets) (\s@DescribePortfolioResponse' {} a -> s {budgets = a} :: DescribePortfolioResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the portfolio.
describePortfolioResponse_portfolioDetail :: Lens.Lens' DescribePortfolioResponse (Core.Maybe PortfolioDetail)
describePortfolioResponse_portfolioDetail = Lens.lens (\DescribePortfolioResponse' {portfolioDetail} -> portfolioDetail) (\s@DescribePortfolioResponse' {} a -> s {portfolioDetail = a} :: DescribePortfolioResponse)

-- | Information about the TagOptions associated with the portfolio.
describePortfolioResponse_tagOptions :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [TagOptionDetail])
describePortfolioResponse_tagOptions = Lens.lens (\DescribePortfolioResponse' {tagOptions} -> tagOptions) (\s@DescribePortfolioResponse' {} a -> s {tagOptions = a} :: DescribePortfolioResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePortfolioResponse_httpStatus :: Lens.Lens' DescribePortfolioResponse Core.Int
describePortfolioResponse_httpStatus = Lens.lens (\DescribePortfolioResponse' {httpStatus} -> httpStatus) (\s@DescribePortfolioResponse' {} a -> s {httpStatus = a} :: DescribePortfolioResponse)

instance Core.NFData DescribePortfolioResponse
