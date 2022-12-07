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
-- Module      : Amazonka.ServiceCatalog.DescribeProductAsAdmin
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product. This operation is run with
-- administrator access.
module Amazonka.ServiceCatalog.DescribeProductAsAdmin
  ( -- * Creating a Request
    DescribeProductAsAdmin (..),
    newDescribeProductAsAdmin,

    -- * Request Lenses
    describeProductAsAdmin_name,
    describeProductAsAdmin_id,
    describeProductAsAdmin_sourcePortfolioId,
    describeProductAsAdmin_acceptLanguage,

    -- * Destructuring the Response
    DescribeProductAsAdminResponse (..),
    newDescribeProductAsAdminResponse,

    -- * Response Lenses
    describeProductAsAdminResponse_tags,
    describeProductAsAdminResponse_provisioningArtifactSummaries,
    describeProductAsAdminResponse_budgets,
    describeProductAsAdminResponse_tagOptions,
    describeProductAsAdminResponse_productViewDetail,
    describeProductAsAdminResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeProductAsAdmin' smart constructor.
data DescribeProductAsAdmin = DescribeProductAsAdmin'
  { -- | The product name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the shared portfolio that the specified product
    -- is associated with.
    --
    -- You can provide this parameter to retrieve the shared TagOptions
    -- associated with the product. If this parameter is provided and if
    -- TagOptions sharing is enabled in the portfolio share, the API returns
    -- both local and shared TagOptions associated with the product. Otherwise
    -- only local TagOptions will be returned.
    sourcePortfolioId :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProductAsAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeProductAsAdmin_name' - The product name.
--
-- 'id', 'describeProductAsAdmin_id' - The product identifier.
--
-- 'sourcePortfolioId', 'describeProductAsAdmin_sourcePortfolioId' - The unique identifier of the shared portfolio that the specified product
-- is associated with.
--
-- You can provide this parameter to retrieve the shared TagOptions
-- associated with the product. If this parameter is provided and if
-- TagOptions sharing is enabled in the portfolio share, the API returns
-- both local and shared TagOptions associated with the product. Otherwise
-- only local TagOptions will be returned.
--
-- 'acceptLanguage', 'describeProductAsAdmin_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newDescribeProductAsAdmin ::
  DescribeProductAsAdmin
newDescribeProductAsAdmin =
  DescribeProductAsAdmin'
    { name = Prelude.Nothing,
      id = Prelude.Nothing,
      sourcePortfolioId = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing
    }

-- | The product name.
describeProductAsAdmin_name :: Lens.Lens' DescribeProductAsAdmin (Prelude.Maybe Prelude.Text)
describeProductAsAdmin_name = Lens.lens (\DescribeProductAsAdmin' {name} -> name) (\s@DescribeProductAsAdmin' {} a -> s {name = a} :: DescribeProductAsAdmin)

-- | The product identifier.
describeProductAsAdmin_id :: Lens.Lens' DescribeProductAsAdmin (Prelude.Maybe Prelude.Text)
describeProductAsAdmin_id = Lens.lens (\DescribeProductAsAdmin' {id} -> id) (\s@DescribeProductAsAdmin' {} a -> s {id = a} :: DescribeProductAsAdmin)

-- | The unique identifier of the shared portfolio that the specified product
-- is associated with.
--
-- You can provide this parameter to retrieve the shared TagOptions
-- associated with the product. If this parameter is provided and if
-- TagOptions sharing is enabled in the portfolio share, the API returns
-- both local and shared TagOptions associated with the product. Otherwise
-- only local TagOptions will be returned.
describeProductAsAdmin_sourcePortfolioId :: Lens.Lens' DescribeProductAsAdmin (Prelude.Maybe Prelude.Text)
describeProductAsAdmin_sourcePortfolioId = Lens.lens (\DescribeProductAsAdmin' {sourcePortfolioId} -> sourcePortfolioId) (\s@DescribeProductAsAdmin' {} a -> s {sourcePortfolioId = a} :: DescribeProductAsAdmin)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProductAsAdmin_acceptLanguage :: Lens.Lens' DescribeProductAsAdmin (Prelude.Maybe Prelude.Text)
describeProductAsAdmin_acceptLanguage = Lens.lens (\DescribeProductAsAdmin' {acceptLanguage} -> acceptLanguage) (\s@DescribeProductAsAdmin' {} a -> s {acceptLanguage = a} :: DescribeProductAsAdmin)

instance Core.AWSRequest DescribeProductAsAdmin where
  type
    AWSResponse DescribeProductAsAdmin =
      DescribeProductAsAdminResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductAsAdminResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "ProvisioningArtifactSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Budgets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TagOptions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ProductViewDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProductAsAdmin where
  hashWithSalt _salt DescribeProductAsAdmin' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sourcePortfolioId
      `Prelude.hashWithSalt` acceptLanguage

instance Prelude.NFData DescribeProductAsAdmin where
  rnf DescribeProductAsAdmin' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sourcePortfolioId
      `Prelude.seq` Prelude.rnf acceptLanguage

instance Data.ToHeaders DescribeProductAsAdmin where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeProductAsAdmin" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProductAsAdmin where
  toJSON DescribeProductAsAdmin' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Id" Data..=) Prelude.<$> id,
            ("SourcePortfolioId" Data..=)
              Prelude.<$> sourcePortfolioId,
            ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage
          ]
      )

instance Data.ToPath DescribeProductAsAdmin where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProductAsAdmin where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProductAsAdminResponse' smart constructor.
data DescribeProductAsAdminResponse = DescribeProductAsAdminResponse'
  { -- | Information about the tags associated with the product.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the provisioning artifacts (also known as versions)
    -- for the specified product.
    provisioningArtifactSummaries :: Prelude.Maybe [ProvisioningArtifactSummary],
    -- | Information about the associated budgets.
    budgets :: Prelude.Maybe [BudgetDetail],
    -- | Information about the TagOptions associated with the product.
    tagOptions :: Prelude.Maybe [TagOptionDetail],
    -- | Information about the product view.
    productViewDetail :: Prelude.Maybe ProductViewDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProductAsAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeProductAsAdminResponse_tags' - Information about the tags associated with the product.
--
-- 'provisioningArtifactSummaries', 'describeProductAsAdminResponse_provisioningArtifactSummaries' - Information about the provisioning artifacts (also known as versions)
-- for the specified product.
--
-- 'budgets', 'describeProductAsAdminResponse_budgets' - Information about the associated budgets.
--
-- 'tagOptions', 'describeProductAsAdminResponse_tagOptions' - Information about the TagOptions associated with the product.
--
-- 'productViewDetail', 'describeProductAsAdminResponse_productViewDetail' - Information about the product view.
--
-- 'httpStatus', 'describeProductAsAdminResponse_httpStatus' - The response's http status code.
newDescribeProductAsAdminResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProductAsAdminResponse
newDescribeProductAsAdminResponse pHttpStatus_ =
  DescribeProductAsAdminResponse'
    { tags =
        Prelude.Nothing,
      provisioningArtifactSummaries =
        Prelude.Nothing,
      budgets = Prelude.Nothing,
      tagOptions = Prelude.Nothing,
      productViewDetail = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the tags associated with the product.
describeProductAsAdminResponse_tags :: Lens.Lens' DescribeProductAsAdminResponse (Prelude.Maybe [Tag])
describeProductAsAdminResponse_tags = Lens.lens (\DescribeProductAsAdminResponse' {tags} -> tags) (\s@DescribeProductAsAdminResponse' {} a -> s {tags = a} :: DescribeProductAsAdminResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the provisioning artifacts (also known as versions)
-- for the specified product.
describeProductAsAdminResponse_provisioningArtifactSummaries :: Lens.Lens' DescribeProductAsAdminResponse (Prelude.Maybe [ProvisioningArtifactSummary])
describeProductAsAdminResponse_provisioningArtifactSummaries = Lens.lens (\DescribeProductAsAdminResponse' {provisioningArtifactSummaries} -> provisioningArtifactSummaries) (\s@DescribeProductAsAdminResponse' {} a -> s {provisioningArtifactSummaries = a} :: DescribeProductAsAdminResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the associated budgets.
describeProductAsAdminResponse_budgets :: Lens.Lens' DescribeProductAsAdminResponse (Prelude.Maybe [BudgetDetail])
describeProductAsAdminResponse_budgets = Lens.lens (\DescribeProductAsAdminResponse' {budgets} -> budgets) (\s@DescribeProductAsAdminResponse' {} a -> s {budgets = a} :: DescribeProductAsAdminResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the TagOptions associated with the product.
describeProductAsAdminResponse_tagOptions :: Lens.Lens' DescribeProductAsAdminResponse (Prelude.Maybe [TagOptionDetail])
describeProductAsAdminResponse_tagOptions = Lens.lens (\DescribeProductAsAdminResponse' {tagOptions} -> tagOptions) (\s@DescribeProductAsAdminResponse' {} a -> s {tagOptions = a} :: DescribeProductAsAdminResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the product view.
describeProductAsAdminResponse_productViewDetail :: Lens.Lens' DescribeProductAsAdminResponse (Prelude.Maybe ProductViewDetail)
describeProductAsAdminResponse_productViewDetail = Lens.lens (\DescribeProductAsAdminResponse' {productViewDetail} -> productViewDetail) (\s@DescribeProductAsAdminResponse' {} a -> s {productViewDetail = a} :: DescribeProductAsAdminResponse)

-- | The response's http status code.
describeProductAsAdminResponse_httpStatus :: Lens.Lens' DescribeProductAsAdminResponse Prelude.Int
describeProductAsAdminResponse_httpStatus = Lens.lens (\DescribeProductAsAdminResponse' {httpStatus} -> httpStatus) (\s@DescribeProductAsAdminResponse' {} a -> s {httpStatus = a} :: DescribeProductAsAdminResponse)

instance
  Prelude.NFData
    DescribeProductAsAdminResponse
  where
  rnf DescribeProductAsAdminResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf provisioningArtifactSummaries
      `Prelude.seq` Prelude.rnf budgets
      `Prelude.seq` Prelude.rnf tagOptions
      `Prelude.seq` Prelude.rnf productViewDetail
      `Prelude.seq` Prelude.rnf httpStatus
