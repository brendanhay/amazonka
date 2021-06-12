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
-- Module      : Network.AWS.ServiceCatalog.DescribeProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
module Network.AWS.ServiceCatalog.DescribeProduct
  ( -- * Creating a Request
    DescribeProduct (..),
    newDescribeProduct,

    -- * Request Lenses
    describeProduct_id,
    describeProduct_name,
    describeProduct_acceptLanguage,

    -- * Destructuring the Response
    DescribeProductResponse (..),
    newDescribeProductResponse,

    -- * Response Lenses
    describeProductResponse_provisioningArtifacts,
    describeProductResponse_launchPaths,
    describeProductResponse_productViewSummary,
    describeProductResponse_budgets,
    describeProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeProduct' smart constructor.
data DescribeProduct = DescribeProduct'
  { -- | The product identifier.
    id :: Core.Maybe Core.Text,
    -- | The product name.
    name :: Core.Maybe Core.Text,
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
-- Create a value of 'DescribeProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeProduct_id' - The product identifier.
--
-- 'name', 'describeProduct_name' - The product name.
--
-- 'acceptLanguage', 'describeProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newDescribeProduct ::
  DescribeProduct
newDescribeProduct =
  DescribeProduct'
    { id = Core.Nothing,
      name = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The product identifier.
describeProduct_id :: Lens.Lens' DescribeProduct (Core.Maybe Core.Text)
describeProduct_id = Lens.lens (\DescribeProduct' {id} -> id) (\s@DescribeProduct' {} a -> s {id = a} :: DescribeProduct)

-- | The product name.
describeProduct_name :: Lens.Lens' DescribeProduct (Core.Maybe Core.Text)
describeProduct_name = Lens.lens (\DescribeProduct' {name} -> name) (\s@DescribeProduct' {} a -> s {name = a} :: DescribeProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProduct_acceptLanguage :: Lens.Lens' DescribeProduct (Core.Maybe Core.Text)
describeProduct_acceptLanguage = Lens.lens (\DescribeProduct' {acceptLanguage} -> acceptLanguage) (\s@DescribeProduct' {} a -> s {acceptLanguage = a} :: DescribeProduct)

instance Core.AWSRequest DescribeProduct where
  type
    AWSResponse DescribeProduct =
      DescribeProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductResponse'
            Core.<$> ( x Core..?> "ProvisioningArtifacts"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "LaunchPaths" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ProductViewSummary")
            Core.<*> (x Core..?> "Budgets" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProduct

instance Core.NFData DescribeProduct

instance Core.ToHeaders DescribeProduct where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProduct" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProduct where
  toJSON DescribeProduct' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Id" Core..=) Core.<$> id,
            ("Name" Core..=) Core.<$> name,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath DescribeProduct where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProduct where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProductResponse' smart constructor.
data DescribeProductResponse = DescribeProductResponse'
  { -- | Information about the provisioning artifacts for the specified product.
    provisioningArtifacts :: Core.Maybe [ProvisioningArtifact],
    -- | Information about the associated launch paths.
    launchPaths :: Core.Maybe [LaunchPath],
    -- | Summary information about the product view.
    productViewSummary :: Core.Maybe ProductViewSummary,
    -- | Information about the associated budgets.
    budgets :: Core.Maybe [BudgetDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifacts', 'describeProductResponse_provisioningArtifacts' - Information about the provisioning artifacts for the specified product.
--
-- 'launchPaths', 'describeProductResponse_launchPaths' - Information about the associated launch paths.
--
-- 'productViewSummary', 'describeProductResponse_productViewSummary' - Summary information about the product view.
--
-- 'budgets', 'describeProductResponse_budgets' - Information about the associated budgets.
--
-- 'httpStatus', 'describeProductResponse_httpStatus' - The response's http status code.
newDescribeProductResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProductResponse
newDescribeProductResponse pHttpStatus_ =
  DescribeProductResponse'
    { provisioningArtifacts =
        Core.Nothing,
      launchPaths = Core.Nothing,
      productViewSummary = Core.Nothing,
      budgets = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the provisioning artifacts for the specified product.
describeProductResponse_provisioningArtifacts :: Lens.Lens' DescribeProductResponse (Core.Maybe [ProvisioningArtifact])
describeProductResponse_provisioningArtifacts = Lens.lens (\DescribeProductResponse' {provisioningArtifacts} -> provisioningArtifacts) (\s@DescribeProductResponse' {} a -> s {provisioningArtifacts = a} :: DescribeProductResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the associated launch paths.
describeProductResponse_launchPaths :: Lens.Lens' DescribeProductResponse (Core.Maybe [LaunchPath])
describeProductResponse_launchPaths = Lens.lens (\DescribeProductResponse' {launchPaths} -> launchPaths) (\s@DescribeProductResponse' {} a -> s {launchPaths = a} :: DescribeProductResponse) Core.. Lens.mapping Lens._Coerce

-- | Summary information about the product view.
describeProductResponse_productViewSummary :: Lens.Lens' DescribeProductResponse (Core.Maybe ProductViewSummary)
describeProductResponse_productViewSummary = Lens.lens (\DescribeProductResponse' {productViewSummary} -> productViewSummary) (\s@DescribeProductResponse' {} a -> s {productViewSummary = a} :: DescribeProductResponse)

-- | Information about the associated budgets.
describeProductResponse_budgets :: Lens.Lens' DescribeProductResponse (Core.Maybe [BudgetDetail])
describeProductResponse_budgets = Lens.lens (\DescribeProductResponse' {budgets} -> budgets) (\s@DescribeProductResponse' {} a -> s {budgets = a} :: DescribeProductResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeProductResponse_httpStatus :: Lens.Lens' DescribeProductResponse Core.Int
describeProductResponse_httpStatus = Lens.lens (\DescribeProductResponse' {httpStatus} -> httpStatus) (\s@DescribeProductResponse' {} a -> s {httpStatus = a} :: DescribeProductResponse)

instance Core.NFData DescribeProductResponse
