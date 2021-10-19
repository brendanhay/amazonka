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
    describeProduct_name,
    describeProduct_acceptLanguage,
    describeProduct_id,

    -- * Destructuring the Response
    DescribeProductResponse (..),
    newDescribeProductResponse,

    -- * Response Lenses
    describeProductResponse_productViewSummary,
    describeProductResponse_provisioningArtifacts,
    describeProductResponse_launchPaths,
    describeProductResponse_budgets,
    describeProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeProduct' smart constructor.
data DescribeProduct = DescribeProduct'
  { -- | The product name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'id', 'describeProduct_id' - The product identifier.
newDescribeProduct ::
  DescribeProduct
newDescribeProduct =
  DescribeProduct'
    { name = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The product name.
describeProduct_name :: Lens.Lens' DescribeProduct (Prelude.Maybe Prelude.Text)
describeProduct_name = Lens.lens (\DescribeProduct' {name} -> name) (\s@DescribeProduct' {} a -> s {name = a} :: DescribeProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProduct_acceptLanguage :: Lens.Lens' DescribeProduct (Prelude.Maybe Prelude.Text)
describeProduct_acceptLanguage = Lens.lens (\DescribeProduct' {acceptLanguage} -> acceptLanguage) (\s@DescribeProduct' {} a -> s {acceptLanguage = a} :: DescribeProduct)

-- | The product identifier.
describeProduct_id :: Lens.Lens' DescribeProduct (Prelude.Maybe Prelude.Text)
describeProduct_id = Lens.lens (\DescribeProduct' {id} -> id) (\s@DescribeProduct' {} a -> s {id = a} :: DescribeProduct)

instance Core.AWSRequest DescribeProduct where
  type
    AWSResponse DescribeProduct =
      DescribeProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductResponse'
            Prelude.<$> (x Core..?> "ProductViewSummary")
            Prelude.<*> ( x Core..?> "ProvisioningArtifacts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "LaunchPaths" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Budgets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProduct

instance Prelude.NFData DescribeProduct

instance Core.ToHeaders DescribeProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProduct where
  toJSON DescribeProduct' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            ("Id" Core..=) Prelude.<$> id
          ]
      )

instance Core.ToPath DescribeProduct where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProductResponse' smart constructor.
data DescribeProductResponse = DescribeProductResponse'
  { -- | Summary information about the product view.
    productViewSummary :: Prelude.Maybe ProductViewSummary,
    -- | Information about the provisioning artifacts for the specified product.
    provisioningArtifacts :: Prelude.Maybe [ProvisioningArtifact],
    -- | Information about the associated launch paths.
    launchPaths :: Prelude.Maybe [LaunchPath],
    -- | Information about the associated budgets.
    budgets :: Prelude.Maybe [BudgetDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productViewSummary', 'describeProductResponse_productViewSummary' - Summary information about the product view.
--
-- 'provisioningArtifacts', 'describeProductResponse_provisioningArtifacts' - Information about the provisioning artifacts for the specified product.
--
-- 'launchPaths', 'describeProductResponse_launchPaths' - Information about the associated launch paths.
--
-- 'budgets', 'describeProductResponse_budgets' - Information about the associated budgets.
--
-- 'httpStatus', 'describeProductResponse_httpStatus' - The response's http status code.
newDescribeProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProductResponse
newDescribeProductResponse pHttpStatus_ =
  DescribeProductResponse'
    { productViewSummary =
        Prelude.Nothing,
      provisioningArtifacts = Prelude.Nothing,
      launchPaths = Prelude.Nothing,
      budgets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information about the product view.
describeProductResponse_productViewSummary :: Lens.Lens' DescribeProductResponse (Prelude.Maybe ProductViewSummary)
describeProductResponse_productViewSummary = Lens.lens (\DescribeProductResponse' {productViewSummary} -> productViewSummary) (\s@DescribeProductResponse' {} a -> s {productViewSummary = a} :: DescribeProductResponse)

-- | Information about the provisioning artifacts for the specified product.
describeProductResponse_provisioningArtifacts :: Lens.Lens' DescribeProductResponse (Prelude.Maybe [ProvisioningArtifact])
describeProductResponse_provisioningArtifacts = Lens.lens (\DescribeProductResponse' {provisioningArtifacts} -> provisioningArtifacts) (\s@DescribeProductResponse' {} a -> s {provisioningArtifacts = a} :: DescribeProductResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the associated launch paths.
describeProductResponse_launchPaths :: Lens.Lens' DescribeProductResponse (Prelude.Maybe [LaunchPath])
describeProductResponse_launchPaths = Lens.lens (\DescribeProductResponse' {launchPaths} -> launchPaths) (\s@DescribeProductResponse' {} a -> s {launchPaths = a} :: DescribeProductResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the associated budgets.
describeProductResponse_budgets :: Lens.Lens' DescribeProductResponse (Prelude.Maybe [BudgetDetail])
describeProductResponse_budgets = Lens.lens (\DescribeProductResponse' {budgets} -> budgets) (\s@DescribeProductResponse' {} a -> s {budgets = a} :: DescribeProductResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeProductResponse_httpStatus :: Lens.Lens' DescribeProductResponse Prelude.Int
describeProductResponse_httpStatus = Lens.lens (\DescribeProductResponse' {httpStatus} -> httpStatus) (\s@DescribeProductResponse' {} a -> s {httpStatus = a} :: DescribeProductResponse)

instance Prelude.NFData DescribeProductResponse
