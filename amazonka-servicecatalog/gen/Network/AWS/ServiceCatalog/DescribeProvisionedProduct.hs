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
    describeProvisionedProduct_id,
    describeProvisionedProduct_name,
    describeProvisionedProduct_acceptLanguage,

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
  { -- | The provisioned product identifier. You must provide the name or ID, but
    -- not both.
    --
    -- If you do not provide a name or ID, or you provide both name and ID, an
    -- @InvalidParametersException@ will occur.
    id :: Core.Maybe Core.Text,
    -- | The name of the provisioned product. You must provide the name or ID,
    -- but not both.
    --
    -- If you do not provide a name or ID, or you provide both name and ID, an
    -- @InvalidParametersException@ will occur.
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
-- Create a value of 'DescribeProvisionedProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeProvisionedProduct_id' - The provisioned product identifier. You must provide the name or ID, but
-- not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
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
newDescribeProvisionedProduct ::
  DescribeProvisionedProduct
newDescribeProvisionedProduct =
  DescribeProvisionedProduct'
    { id = Core.Nothing,
      name = Core.Nothing,
      acceptLanguage = Core.Nothing
    }

-- | The provisioned product identifier. You must provide the name or ID, but
-- not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
describeProvisionedProduct_id :: Lens.Lens' DescribeProvisionedProduct (Core.Maybe Core.Text)
describeProvisionedProduct_id = Lens.lens (\DescribeProvisionedProduct' {id} -> id) (\s@DescribeProvisionedProduct' {} a -> s {id = a} :: DescribeProvisionedProduct)

-- | The name of the provisioned product. You must provide the name or ID,
-- but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
describeProvisionedProduct_name :: Lens.Lens' DescribeProvisionedProduct (Core.Maybe Core.Text)
describeProvisionedProduct_name = Lens.lens (\DescribeProvisionedProduct' {name} -> name) (\s@DescribeProvisionedProduct' {} a -> s {name = a} :: DescribeProvisionedProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisionedProduct_acceptLanguage :: Lens.Lens' DescribeProvisionedProduct (Core.Maybe Core.Text)
describeProvisionedProduct_acceptLanguage = Lens.lens (\DescribeProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisionedProduct' {} a -> s {acceptLanguage = a} :: DescribeProvisionedProduct)

instance Core.AWSRequest DescribeProvisionedProduct where
  type
    AWSResponse DescribeProvisionedProduct =
      DescribeProvisionedProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductResponse'
            Core.<$> (x Core..?> "ProvisionedProductDetail")
            Core.<*> ( x Core..?> "CloudWatchDashboards"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProvisionedProduct

instance Core.NFData DescribeProvisionedProduct

instance Core.ToHeaders DescribeProvisionedProduct where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProvisionedProduct" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProvisionedProduct where
  toJSON DescribeProvisionedProduct' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Id" Core..=) Core.<$> id,
            ("Name" Core..=) Core.<$> name,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.ToPath DescribeProvisionedProduct where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProvisionedProduct where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProvisionedProductResponse' smart constructor.
data DescribeProvisionedProductResponse = DescribeProvisionedProductResponse'
  { -- | Information about the provisioned product.
    provisionedProductDetail :: Core.Maybe ProvisionedProductDetail,
    -- | Any CloudWatch dashboards that were created when provisioning the
    -- product.
    cloudWatchDashboards :: Core.Maybe [CloudWatchDashboard],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeProvisionedProductResponse
newDescribeProvisionedProductResponse pHttpStatus_ =
  DescribeProvisionedProductResponse'
    { provisionedProductDetail =
        Core.Nothing,
      cloudWatchDashboards = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the provisioned product.
describeProvisionedProductResponse_provisionedProductDetail :: Lens.Lens' DescribeProvisionedProductResponse (Core.Maybe ProvisionedProductDetail)
describeProvisionedProductResponse_provisionedProductDetail = Lens.lens (\DescribeProvisionedProductResponse' {provisionedProductDetail} -> provisionedProductDetail) (\s@DescribeProvisionedProductResponse' {} a -> s {provisionedProductDetail = a} :: DescribeProvisionedProductResponse)

-- | Any CloudWatch dashboards that were created when provisioning the
-- product.
describeProvisionedProductResponse_cloudWatchDashboards :: Lens.Lens' DescribeProvisionedProductResponse (Core.Maybe [CloudWatchDashboard])
describeProvisionedProductResponse_cloudWatchDashboards = Lens.lens (\DescribeProvisionedProductResponse' {cloudWatchDashboards} -> cloudWatchDashboards) (\s@DescribeProvisionedProductResponse' {} a -> s {cloudWatchDashboards = a} :: DescribeProvisionedProductResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeProvisionedProductResponse_httpStatus :: Lens.Lens' DescribeProvisionedProductResponse Core.Int
describeProvisionedProductResponse_httpStatus = Lens.lens (\DescribeProvisionedProductResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisionedProductResponse' {} a -> s {httpStatus = a} :: DescribeProvisionedProductResponse)

instance
  Core.NFData
    DescribeProvisionedProductResponse
