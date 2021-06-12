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
-- Module      : Network.AWS.ServiceCatalog.DescribeProductAsAdmin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product. This operation is run with
-- administrator access.
module Network.AWS.ServiceCatalog.DescribeProductAsAdmin
  ( -- * Creating a Request
    DescribeProductAsAdmin (..),
    newDescribeProductAsAdmin,

    -- * Request Lenses
    describeProductAsAdmin_id,
    describeProductAsAdmin_name,
    describeProductAsAdmin_sourcePortfolioId,
    describeProductAsAdmin_acceptLanguage,

    -- * Destructuring the Response
    DescribeProductAsAdminResponse (..),
    newDescribeProductAsAdminResponse,

    -- * Response Lenses
    describeProductAsAdminResponse_productViewDetail,
    describeProductAsAdminResponse_tags,
    describeProductAsAdminResponse_budgets,
    describeProductAsAdminResponse_provisioningArtifactSummaries,
    describeProductAsAdminResponse_tagOptions,
    describeProductAsAdminResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeProductAsAdmin' smart constructor.
data DescribeProductAsAdmin = DescribeProductAsAdmin'
  { -- | The product identifier.
    id :: Core.Maybe Core.Text,
    -- | The product name.
    name :: Core.Maybe Core.Text,
    -- | The unique identifier of the shared portfolio that the specified product
    -- is associated with.
    --
    -- You can provide this parameter to retrieve the shared TagOptions
    -- associated with the product. If this parameter is provided and if
    -- TagOptions sharing is enabled in the portfolio share, the API returns
    -- both local and shared TagOptions associated with the product. Otherwise
    -- only local TagOptions will be returned.
    sourcePortfolioId :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProductAsAdmin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeProductAsAdmin_id' - The product identifier.
--
-- 'name', 'describeProductAsAdmin_name' - The product name.
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
    { id = Core.Nothing,
      name = Core.Nothing,
      sourcePortfolioId = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The product identifier.
describeProductAsAdmin_id :: Lens.Lens' DescribeProductAsAdmin (Core.Maybe Core.Text)
describeProductAsAdmin_id = Lens.lens (\DescribeProductAsAdmin' {id} -> id) (\s@DescribeProductAsAdmin' {} a -> s {id = a} :: DescribeProductAsAdmin)

-- | The product name.
describeProductAsAdmin_name :: Lens.Lens' DescribeProductAsAdmin (Core.Maybe Core.Text)
describeProductAsAdmin_name = Lens.lens (\DescribeProductAsAdmin' {name} -> name) (\s@DescribeProductAsAdmin' {} a -> s {name = a} :: DescribeProductAsAdmin)

-- | The unique identifier of the shared portfolio that the specified product
-- is associated with.
--
-- You can provide this parameter to retrieve the shared TagOptions
-- associated with the product. If this parameter is provided and if
-- TagOptions sharing is enabled in the portfolio share, the API returns
-- both local and shared TagOptions associated with the product. Otherwise
-- only local TagOptions will be returned.
describeProductAsAdmin_sourcePortfolioId :: Lens.Lens' DescribeProductAsAdmin (Core.Maybe Core.Text)
describeProductAsAdmin_sourcePortfolioId = Lens.lens (\DescribeProductAsAdmin' {sourcePortfolioId} -> sourcePortfolioId) (\s@DescribeProductAsAdmin' {} a -> s {sourcePortfolioId = a} :: DescribeProductAsAdmin)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProductAsAdmin_acceptLanguage :: Lens.Lens' DescribeProductAsAdmin (Core.Maybe Core.Text)
describeProductAsAdmin_acceptLanguage = Lens.lens (\DescribeProductAsAdmin' {acceptLanguage} -> acceptLanguage) (\s@DescribeProductAsAdmin' {} a -> s {acceptLanguage = a} :: DescribeProductAsAdmin)

instance Core.AWSRequest DescribeProductAsAdmin where
  type
    AWSResponse DescribeProductAsAdmin =
      DescribeProductAsAdminResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductAsAdminResponse'
            Core.<$> (x Core..?> "ProductViewDetail")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Budgets" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "ProvisioningArtifactSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "TagOptions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProductAsAdmin

instance Core.NFData DescribeProductAsAdmin

instance Core.ToHeaders DescribeProductAsAdmin where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProductAsAdmin" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProductAsAdmin where
  toJSON DescribeProductAsAdmin' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Id" Core..=) Core.<$> id,
            ("Name" Core..=) Core.<$> name,
            ("SourcePortfolioId" Core..=)
              Core.<$> sourcePortfolioId,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath DescribeProductAsAdmin where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProductAsAdmin where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProductAsAdminResponse' smart constructor.
data DescribeProductAsAdminResponse = DescribeProductAsAdminResponse'
  { -- | Information about the product view.
    productViewDetail :: Core.Maybe ProductViewDetail,
    -- | Information about the tags associated with the product.
    tags :: Core.Maybe [Tag],
    -- | Information about the associated budgets.
    budgets :: Core.Maybe [BudgetDetail],
    -- | Information about the provisioning artifacts (also known as versions)
    -- for the specified product.
    provisioningArtifactSummaries :: Core.Maybe [ProvisioningArtifactSummary],
    -- | Information about the TagOptions associated with the product.
    tagOptions :: Core.Maybe [TagOptionDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProductAsAdminResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productViewDetail', 'describeProductAsAdminResponse_productViewDetail' - Information about the product view.
--
-- 'tags', 'describeProductAsAdminResponse_tags' - Information about the tags associated with the product.
--
-- 'budgets', 'describeProductAsAdminResponse_budgets' - Information about the associated budgets.
--
-- 'provisioningArtifactSummaries', 'describeProductAsAdminResponse_provisioningArtifactSummaries' - Information about the provisioning artifacts (also known as versions)
-- for the specified product.
--
-- 'tagOptions', 'describeProductAsAdminResponse_tagOptions' - Information about the TagOptions associated with the product.
--
-- 'httpStatus', 'describeProductAsAdminResponse_httpStatus' - The response's http status code.
newDescribeProductAsAdminResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProductAsAdminResponse
newDescribeProductAsAdminResponse pHttpStatus_ =
  DescribeProductAsAdminResponse'
    { productViewDetail =
        Core.Nothing,
      tags = Core.Nothing,
      budgets = Core.Nothing,
      provisioningArtifactSummaries =
        Core.Nothing,
      tagOptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the product view.
describeProductAsAdminResponse_productViewDetail :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe ProductViewDetail)
describeProductAsAdminResponse_productViewDetail = Lens.lens (\DescribeProductAsAdminResponse' {productViewDetail} -> productViewDetail) (\s@DescribeProductAsAdminResponse' {} a -> s {productViewDetail = a} :: DescribeProductAsAdminResponse)

-- | Information about the tags associated with the product.
describeProductAsAdminResponse_tags :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe [Tag])
describeProductAsAdminResponse_tags = Lens.lens (\DescribeProductAsAdminResponse' {tags} -> tags) (\s@DescribeProductAsAdminResponse' {} a -> s {tags = a} :: DescribeProductAsAdminResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the associated budgets.
describeProductAsAdminResponse_budgets :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe [BudgetDetail])
describeProductAsAdminResponse_budgets = Lens.lens (\DescribeProductAsAdminResponse' {budgets} -> budgets) (\s@DescribeProductAsAdminResponse' {} a -> s {budgets = a} :: DescribeProductAsAdminResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the provisioning artifacts (also known as versions)
-- for the specified product.
describeProductAsAdminResponse_provisioningArtifactSummaries :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe [ProvisioningArtifactSummary])
describeProductAsAdminResponse_provisioningArtifactSummaries = Lens.lens (\DescribeProductAsAdminResponse' {provisioningArtifactSummaries} -> provisioningArtifactSummaries) (\s@DescribeProductAsAdminResponse' {} a -> s {provisioningArtifactSummaries = a} :: DescribeProductAsAdminResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the TagOptions associated with the product.
describeProductAsAdminResponse_tagOptions :: Lens.Lens' DescribeProductAsAdminResponse (Core.Maybe [TagOptionDetail])
describeProductAsAdminResponse_tagOptions = Lens.lens (\DescribeProductAsAdminResponse' {tagOptions} -> tagOptions) (\s@DescribeProductAsAdminResponse' {} a -> s {tagOptions = a} :: DescribeProductAsAdminResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeProductAsAdminResponse_httpStatus :: Lens.Lens' DescribeProductAsAdminResponse Core.Int
describeProductAsAdminResponse_httpStatus = Lens.lens (\DescribeProductAsAdminResponse' {httpStatus} -> httpStatus) (\s@DescribeProductAsAdminResponse' {} a -> s {httpStatus = a} :: DescribeProductAsAdminResponse)

instance Core.NFData DescribeProductAsAdminResponse
