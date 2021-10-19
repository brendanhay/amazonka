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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioned product.
module Network.AWS.ServiceCatalog.DescribeProvisionedProduct
  ( -- * Creating a Request
    DescribeProvisionedProduct (..),
    newDescribeProvisionedProduct,

    -- * Request Lenses
    describeProvisionedProduct_name,
    describeProvisionedProduct_acceptLanguage,
    describeProvisionedProduct_id,

    -- * Destructuring the Response
    DescribeProvisionedProductResponse (..),
    newDescribeProvisionedProductResponse,

    -- * Response Lenses
    describeProvisionedProductResponse_provisionedProductDetail,
    describeProvisionedProductResponse_cloudWatchDashboards,
    describeProvisionedProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | DescribeProvisionedProductAPI input structure. AcceptLanguage -
-- [Optional] The language code for localization. Id - [Optional] The
-- provisioned product identifier. Name - [Optional] Another provisioned
-- product identifier. Customers must provide either Id or Name.
--
-- /See:/ 'newDescribeProvisionedProduct' smart constructor.
data DescribeProvisionedProduct = DescribeProvisionedProduct'
  { -- | The name of the provisioned product. You must provide the name or ID,
    -- but not both.
    --
    -- If you do not provide a name or ID, or you provide both name and ID, an
    -- @InvalidParametersException@ will occur.
    name :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The provisioned product identifier. You must provide the name or ID, but
    -- not both.
    --
    -- If you do not provide a name or ID, or you provide both name and ID, an
    -- @InvalidParametersException@ will occur.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisionedProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeProvisionedProduct_name' - The name of the provisioned product. You must provide the name or ID,
-- but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
--
-- 'acceptLanguage', 'describeProvisionedProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'describeProvisionedProduct_id' - The provisioned product identifier. You must provide the name or ID, but
-- not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
newDescribeProvisionedProduct ::
  DescribeProvisionedProduct
newDescribeProvisionedProduct =
  DescribeProvisionedProduct'
    { name = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The name of the provisioned product. You must provide the name or ID,
-- but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
describeProvisionedProduct_name :: Lens.Lens' DescribeProvisionedProduct (Prelude.Maybe Prelude.Text)
describeProvisionedProduct_name = Lens.lens (\DescribeProvisionedProduct' {name} -> name) (\s@DescribeProvisionedProduct' {} a -> s {name = a} :: DescribeProvisionedProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisionedProduct_acceptLanguage :: Lens.Lens' DescribeProvisionedProduct (Prelude.Maybe Prelude.Text)
describeProvisionedProduct_acceptLanguage = Lens.lens (\DescribeProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisionedProduct' {} a -> s {acceptLanguage = a} :: DescribeProvisionedProduct)

-- | The provisioned product identifier. You must provide the name or ID, but
-- not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
describeProvisionedProduct_id :: Lens.Lens' DescribeProvisionedProduct (Prelude.Maybe Prelude.Text)
describeProvisionedProduct_id = Lens.lens (\DescribeProvisionedProduct' {id} -> id) (\s@DescribeProvisionedProduct' {} a -> s {id = a} :: DescribeProvisionedProduct)

instance Core.AWSRequest DescribeProvisionedProduct where
  type
    AWSResponse DescribeProvisionedProduct =
      DescribeProvisionedProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductResponse'
            Prelude.<$> (x Core..?> "ProvisionedProductDetail")
            Prelude.<*> ( x Core..?> "CloudWatchDashboards"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProvisionedProduct

instance Prelude.NFData DescribeProvisionedProduct

instance Core.ToHeaders DescribeProvisionedProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProvisionedProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProvisionedProduct where
  toJSON DescribeProvisionedProduct' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            ("Id" Core..=) Prelude.<$> id
          ]
      )

instance Core.ToPath DescribeProvisionedProduct where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProvisionedProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisionedProductResponse' smart constructor.
data DescribeProvisionedProductResponse = DescribeProvisionedProductResponse'
  { -- | Information about the provisioned product.
    provisionedProductDetail :: Prelude.Maybe ProvisionedProductDetail,
    -- | Any CloudWatch dashboards that were created when provisioning the
    -- product.
    cloudWatchDashboards :: Prelude.Maybe [CloudWatchDashboard],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisionedProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedProductDetail', 'describeProvisionedProductResponse_provisionedProductDetail' - Information about the provisioned product.
--
-- 'cloudWatchDashboards', 'describeProvisionedProductResponse_cloudWatchDashboards' - Any CloudWatch dashboards that were created when provisioning the
-- product.
--
-- 'httpStatus', 'describeProvisionedProductResponse_httpStatus' - The response's http status code.
newDescribeProvisionedProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisionedProductResponse
newDescribeProvisionedProductResponse pHttpStatus_ =
  DescribeProvisionedProductResponse'
    { provisionedProductDetail =
        Prelude.Nothing,
      cloudWatchDashboards = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the provisioned product.
describeProvisionedProductResponse_provisionedProductDetail :: Lens.Lens' DescribeProvisionedProductResponse (Prelude.Maybe ProvisionedProductDetail)
describeProvisionedProductResponse_provisionedProductDetail = Lens.lens (\DescribeProvisionedProductResponse' {provisionedProductDetail} -> provisionedProductDetail) (\s@DescribeProvisionedProductResponse' {} a -> s {provisionedProductDetail = a} :: DescribeProvisionedProductResponse)

-- | Any CloudWatch dashboards that were created when provisioning the
-- product.
describeProvisionedProductResponse_cloudWatchDashboards :: Lens.Lens' DescribeProvisionedProductResponse (Prelude.Maybe [CloudWatchDashboard])
describeProvisionedProductResponse_cloudWatchDashboards = Lens.lens (\DescribeProvisionedProductResponse' {cloudWatchDashboards} -> cloudWatchDashboards) (\s@DescribeProvisionedProductResponse' {} a -> s {cloudWatchDashboards = a} :: DescribeProvisionedProductResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeProvisionedProductResponse_httpStatus :: Lens.Lens' DescribeProvisionedProductResponse Prelude.Int
describeProvisionedProductResponse_httpStatus = Lens.lens (\DescribeProvisionedProductResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisionedProductResponse' {} a -> s {httpStatus = a} :: DescribeProvisionedProductResponse)

instance
  Prelude.NFData
    DescribeProvisionedProductResponse
