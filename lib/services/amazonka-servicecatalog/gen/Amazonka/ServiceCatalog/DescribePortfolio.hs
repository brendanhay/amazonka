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
-- Module      : Amazonka.ServiceCatalog.DescribePortfolio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Amazonka.ServiceCatalog.DescribePortfolio
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
    describePortfolioResponse_portfolioDetail,
    describePortfolioResponse_budgets,
    describePortfolioResponse_tagOptions,
    describePortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribePortfolio' smart constructor.
data DescribePortfolio = DescribePortfolio'
  { -- | The language code.
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
  Prelude.Text ->
  DescribePortfolio
newDescribePortfolio pId_ =
  DescribePortfolio'
    { acceptLanguage =
        Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describePortfolio_acceptLanguage :: Lens.Lens' DescribePortfolio (Prelude.Maybe Prelude.Text)
describePortfolio_acceptLanguage = Lens.lens (\DescribePortfolio' {acceptLanguage} -> acceptLanguage) (\s@DescribePortfolio' {} a -> s {acceptLanguage = a} :: DescribePortfolio)

-- | The portfolio identifier.
describePortfolio_id :: Lens.Lens' DescribePortfolio Prelude.Text
describePortfolio_id = Lens.lens (\DescribePortfolio' {id} -> id) (\s@DescribePortfolio' {} a -> s {id = a} :: DescribePortfolio)

instance Core.AWSRequest DescribePortfolio where
  type
    AWSResponse DescribePortfolio =
      DescribePortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePortfolioResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "PortfolioDetail")
            Prelude.<*> (x Core..?> "Budgets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "TagOptions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePortfolio where
  hashWithSalt _salt DescribePortfolio' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` id

instance Prelude.NFData DescribePortfolio where
  rnf DescribePortfolio' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders DescribePortfolio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribePortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePortfolio where
  toJSON DescribePortfolio' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DescribePortfolio where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePortfolioResponse' smart constructor.
data DescribePortfolioResponse = DescribePortfolioResponse'
  { -- | Information about the tags associated with the portfolio.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the portfolio.
    portfolioDetail :: Prelude.Maybe PortfolioDetail,
    -- | Information about the associated budgets.
    budgets :: Prelude.Maybe [BudgetDetail],
    -- | Information about the TagOptions associated with the portfolio.
    tagOptions :: Prelude.Maybe [TagOptionDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'portfolioDetail', 'describePortfolioResponse_portfolioDetail' - Information about the portfolio.
--
-- 'budgets', 'describePortfolioResponse_budgets' - Information about the associated budgets.
--
-- 'tagOptions', 'describePortfolioResponse_tagOptions' - Information about the TagOptions associated with the portfolio.
--
-- 'httpStatus', 'describePortfolioResponse_httpStatus' - The response's http status code.
newDescribePortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePortfolioResponse
newDescribePortfolioResponse pHttpStatus_ =
  DescribePortfolioResponse'
    { tags = Prelude.Nothing,
      portfolioDetail = Prelude.Nothing,
      budgets = Prelude.Nothing,
      tagOptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags associated with the portfolio.
describePortfolioResponse_tags :: Lens.Lens' DescribePortfolioResponse (Prelude.Maybe [Tag])
describePortfolioResponse_tags = Lens.lens (\DescribePortfolioResponse' {tags} -> tags) (\s@DescribePortfolioResponse' {} a -> s {tags = a} :: DescribePortfolioResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the portfolio.
describePortfolioResponse_portfolioDetail :: Lens.Lens' DescribePortfolioResponse (Prelude.Maybe PortfolioDetail)
describePortfolioResponse_portfolioDetail = Lens.lens (\DescribePortfolioResponse' {portfolioDetail} -> portfolioDetail) (\s@DescribePortfolioResponse' {} a -> s {portfolioDetail = a} :: DescribePortfolioResponse)

-- | Information about the associated budgets.
describePortfolioResponse_budgets :: Lens.Lens' DescribePortfolioResponse (Prelude.Maybe [BudgetDetail])
describePortfolioResponse_budgets = Lens.lens (\DescribePortfolioResponse' {budgets} -> budgets) (\s@DescribePortfolioResponse' {} a -> s {budgets = a} :: DescribePortfolioResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the TagOptions associated with the portfolio.
describePortfolioResponse_tagOptions :: Lens.Lens' DescribePortfolioResponse (Prelude.Maybe [TagOptionDetail])
describePortfolioResponse_tagOptions = Lens.lens (\DescribePortfolioResponse' {tagOptions} -> tagOptions) (\s@DescribePortfolioResponse' {} a -> s {tagOptions = a} :: DescribePortfolioResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePortfolioResponse_httpStatus :: Lens.Lens' DescribePortfolioResponse Prelude.Int
describePortfolioResponse_httpStatus = Lens.lens (\DescribePortfolioResponse' {httpStatus} -> httpStatus) (\s@DescribePortfolioResponse' {} a -> s {httpStatus = a} :: DescribePortfolioResponse)

instance Prelude.NFData DescribePortfolioResponse where
  rnf DescribePortfolioResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf portfolioDetail
      `Prelude.seq` Prelude.rnf budgets
      `Prelude.seq` Prelude.rnf tagOptions
      `Prelude.seq` Prelude.rnf httpStatus
