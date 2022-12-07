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
-- Module      : Amazonka.ServiceCatalog.DescribeProvisionedProduct
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioned product.
module Amazonka.ServiceCatalog.DescribeProvisionedProduct
  ( -- * Creating a Request
    DescribeProvisionedProduct (..),
    newDescribeProvisionedProduct,

    -- * Request Lenses
    describeProvisionedProduct_name,
    describeProvisionedProduct_id,
    describeProvisionedProduct_acceptLanguage,

    -- * Destructuring the Response
    DescribeProvisionedProductResponse (..),
    newDescribeProvisionedProductResponse,

    -- * Response Lenses
    describeProvisionedProductResponse_cloudWatchDashboards,
    describeProvisionedProductResponse_provisionedProductDetail,
    describeProvisionedProductResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

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
    -- | The provisioned product identifier. You must provide the name or ID, but
    -- not both.
    --
    -- If you do not provide a name or ID, or you provide both name and ID, an
    -- @InvalidParametersException@ will occur.
    id :: Prelude.Maybe Prelude.Text,
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
-- 'id', 'describeProvisionedProduct_id' - The provisioned product identifier. You must provide the name or ID, but
-- not both.
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
    { name = Prelude.Nothing,
      id = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing
    }

-- | The name of the provisioned product. You must provide the name or ID,
-- but not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
describeProvisionedProduct_name :: Lens.Lens' DescribeProvisionedProduct (Prelude.Maybe Prelude.Text)
describeProvisionedProduct_name = Lens.lens (\DescribeProvisionedProduct' {name} -> name) (\s@DescribeProvisionedProduct' {} a -> s {name = a} :: DescribeProvisionedProduct)

-- | The provisioned product identifier. You must provide the name or ID, but
-- not both.
--
-- If you do not provide a name or ID, or you provide both name and ID, an
-- @InvalidParametersException@ will occur.
describeProvisionedProduct_id :: Lens.Lens' DescribeProvisionedProduct (Prelude.Maybe Prelude.Text)
describeProvisionedProduct_id = Lens.lens (\DescribeProvisionedProduct' {id} -> id) (\s@DescribeProvisionedProduct' {} a -> s {id = a} :: DescribeProvisionedProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisionedProduct_acceptLanguage :: Lens.Lens' DescribeProvisionedProduct (Prelude.Maybe Prelude.Text)
describeProvisionedProduct_acceptLanguage = Lens.lens (\DescribeProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisionedProduct' {} a -> s {acceptLanguage = a} :: DescribeProvisionedProduct)

instance Core.AWSRequest DescribeProvisionedProduct where
  type
    AWSResponse DescribeProvisionedProduct =
      DescribeProvisionedProductResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisionedProductResponse'
            Prelude.<$> ( x Data..?> "CloudWatchDashboards"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ProvisionedProductDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProvisionedProduct where
  hashWithSalt _salt DescribeProvisionedProduct' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` acceptLanguage

instance Prelude.NFData DescribeProvisionedProduct where
  rnf DescribeProvisionedProduct' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf acceptLanguage

instance Data.ToHeaders DescribeProvisionedProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeProvisionedProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProvisionedProduct where
  toJSON DescribeProvisionedProduct' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Id" Data..=) Prelude.<$> id,
            ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage
          ]
      )

instance Data.ToPath DescribeProvisionedProduct where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProvisionedProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisionedProductResponse' smart constructor.
data DescribeProvisionedProductResponse = DescribeProvisionedProductResponse'
  { -- | Any CloudWatch dashboards that were created when provisioning the
    -- product.
    cloudWatchDashboards :: Prelude.Maybe [CloudWatchDashboard],
    -- | Information about the provisioned product.
    provisionedProductDetail :: Prelude.Maybe ProvisionedProductDetail,
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
-- 'cloudWatchDashboards', 'describeProvisionedProductResponse_cloudWatchDashboards' - Any CloudWatch dashboards that were created when provisioning the
-- product.
--
-- 'provisionedProductDetail', 'describeProvisionedProductResponse_provisionedProductDetail' - Information about the provisioned product.
--
-- 'httpStatus', 'describeProvisionedProductResponse_httpStatus' - The response's http status code.
newDescribeProvisionedProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisionedProductResponse
newDescribeProvisionedProductResponse pHttpStatus_ =
  DescribeProvisionedProductResponse'
    { cloudWatchDashboards =
        Prelude.Nothing,
      provisionedProductDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any CloudWatch dashboards that were created when provisioning the
-- product.
describeProvisionedProductResponse_cloudWatchDashboards :: Lens.Lens' DescribeProvisionedProductResponse (Prelude.Maybe [CloudWatchDashboard])
describeProvisionedProductResponse_cloudWatchDashboards = Lens.lens (\DescribeProvisionedProductResponse' {cloudWatchDashboards} -> cloudWatchDashboards) (\s@DescribeProvisionedProductResponse' {} a -> s {cloudWatchDashboards = a} :: DescribeProvisionedProductResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the provisioned product.
describeProvisionedProductResponse_provisionedProductDetail :: Lens.Lens' DescribeProvisionedProductResponse (Prelude.Maybe ProvisionedProductDetail)
describeProvisionedProductResponse_provisionedProductDetail = Lens.lens (\DescribeProvisionedProductResponse' {provisionedProductDetail} -> provisionedProductDetail) (\s@DescribeProvisionedProductResponse' {} a -> s {provisionedProductDetail = a} :: DescribeProvisionedProductResponse)

-- | The response's http status code.
describeProvisionedProductResponse_httpStatus :: Lens.Lens' DescribeProvisionedProductResponse Prelude.Int
describeProvisionedProductResponse_httpStatus = Lens.lens (\DescribeProvisionedProductResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisionedProductResponse' {} a -> s {httpStatus = a} :: DescribeProvisionedProductResponse)

instance
  Prelude.NFData
    DescribeProvisionedProductResponse
  where
  rnf DescribeProvisionedProductResponse' {..} =
    Prelude.rnf cloudWatchDashboards
      `Prelude.seq` Prelude.rnf provisionedProductDetail
      `Prelude.seq` Prelude.rnf httpStatus
