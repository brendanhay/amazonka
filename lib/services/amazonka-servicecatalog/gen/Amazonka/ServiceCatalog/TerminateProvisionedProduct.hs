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
-- Module      : Amazonka.ServiceCatalog.TerminateProvisionedProduct
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified provisioned product.
--
-- This operation does not delete any records associated with the
-- provisioned product.
--
-- You can check the status of this request using DescribeRecord.
module Amazonka.ServiceCatalog.TerminateProvisionedProduct
  ( -- * Creating a Request
    TerminateProvisionedProduct (..),
    newTerminateProvisionedProduct,

    -- * Request Lenses
    terminateProvisionedProduct_acceptLanguage,
    terminateProvisionedProduct_ignoreErrors,
    terminateProvisionedProduct_provisionedProductId,
    terminateProvisionedProduct_provisionedProductName,
    terminateProvisionedProduct_retainPhysicalResources,
    terminateProvisionedProduct_terminateToken,

    -- * Destructuring the Response
    TerminateProvisionedProductResponse (..),
    newTerminateProvisionedProductResponse,

    -- * Response Lenses
    terminateProvisionedProductResponse_recordDetail,
    terminateProvisionedProductResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newTerminateProvisionedProduct' smart constructor.
data TerminateProvisionedProduct = TerminateProvisionedProduct'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | If set to true, Service Catalog stops managing the specified provisioned
    -- product even if it cannot delete the underlying resources.
    ignoreErrors :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the provisioned product. You cannot specify both
    -- @ProvisionedProductName@ and @ProvisionedProductId@.
    provisionedProductId :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioned product. You cannot specify both
    -- @ProvisionedProductName@ and @ProvisionedProductId@.
    provisionedProductName :: Prelude.Maybe Prelude.Text,
    -- | When this boolean parameter is set to true, the
    -- @TerminateProvisionedProduct@ API deletes the Service Catalog
    -- provisioned product. However, it does not remove the CloudFormation
    -- stack, stack set, or the underlying resources of the deleted provisioned
    -- product. The default value is false.
    retainPhysicalResources :: Prelude.Maybe Prelude.Bool,
    -- | An idempotency token that uniquely identifies the termination request.
    -- This token is only valid during the termination process. After the
    -- provisioned product is terminated, subsequent requests to terminate the
    -- same provisioned product always return __ResourceNotFound__.
    terminateToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateProvisionedProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'terminateProvisionedProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'ignoreErrors', 'terminateProvisionedProduct_ignoreErrors' - If set to true, Service Catalog stops managing the specified provisioned
-- product even if it cannot delete the underlying resources.
--
-- 'provisionedProductId', 'terminateProvisionedProduct_provisionedProductId' - The identifier of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
--
-- 'provisionedProductName', 'terminateProvisionedProduct_provisionedProductName' - The name of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
--
-- 'retainPhysicalResources', 'terminateProvisionedProduct_retainPhysicalResources' - When this boolean parameter is set to true, the
-- @TerminateProvisionedProduct@ API deletes the Service Catalog
-- provisioned product. However, it does not remove the CloudFormation
-- stack, stack set, or the underlying resources of the deleted provisioned
-- product. The default value is false.
--
-- 'terminateToken', 'terminateProvisionedProduct_terminateToken' - An idempotency token that uniquely identifies the termination request.
-- This token is only valid during the termination process. After the
-- provisioned product is terminated, subsequent requests to terminate the
-- same provisioned product always return __ResourceNotFound__.
newTerminateProvisionedProduct ::
  -- | 'terminateToken'
  Prelude.Text ->
  TerminateProvisionedProduct
newTerminateProvisionedProduct pTerminateToken_ =
  TerminateProvisionedProduct'
    { acceptLanguage =
        Prelude.Nothing,
      ignoreErrors = Prelude.Nothing,
      provisionedProductId = Prelude.Nothing,
      provisionedProductName = Prelude.Nothing,
      retainPhysicalResources = Prelude.Nothing,
      terminateToken = pTerminateToken_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
terminateProvisionedProduct_acceptLanguage :: Lens.Lens' TerminateProvisionedProduct (Prelude.Maybe Prelude.Text)
terminateProvisionedProduct_acceptLanguage = Lens.lens (\TerminateProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@TerminateProvisionedProduct' {} a -> s {acceptLanguage = a} :: TerminateProvisionedProduct)

-- | If set to true, Service Catalog stops managing the specified provisioned
-- product even if it cannot delete the underlying resources.
terminateProvisionedProduct_ignoreErrors :: Lens.Lens' TerminateProvisionedProduct (Prelude.Maybe Prelude.Bool)
terminateProvisionedProduct_ignoreErrors = Lens.lens (\TerminateProvisionedProduct' {ignoreErrors} -> ignoreErrors) (\s@TerminateProvisionedProduct' {} a -> s {ignoreErrors = a} :: TerminateProvisionedProduct)

-- | The identifier of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
terminateProvisionedProduct_provisionedProductId :: Lens.Lens' TerminateProvisionedProduct (Prelude.Maybe Prelude.Text)
terminateProvisionedProduct_provisionedProductId = Lens.lens (\TerminateProvisionedProduct' {provisionedProductId} -> provisionedProductId) (\s@TerminateProvisionedProduct' {} a -> s {provisionedProductId = a} :: TerminateProvisionedProduct)

-- | The name of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
terminateProvisionedProduct_provisionedProductName :: Lens.Lens' TerminateProvisionedProduct (Prelude.Maybe Prelude.Text)
terminateProvisionedProduct_provisionedProductName = Lens.lens (\TerminateProvisionedProduct' {provisionedProductName} -> provisionedProductName) (\s@TerminateProvisionedProduct' {} a -> s {provisionedProductName = a} :: TerminateProvisionedProduct)

-- | When this boolean parameter is set to true, the
-- @TerminateProvisionedProduct@ API deletes the Service Catalog
-- provisioned product. However, it does not remove the CloudFormation
-- stack, stack set, or the underlying resources of the deleted provisioned
-- product. The default value is false.
terminateProvisionedProduct_retainPhysicalResources :: Lens.Lens' TerminateProvisionedProduct (Prelude.Maybe Prelude.Bool)
terminateProvisionedProduct_retainPhysicalResources = Lens.lens (\TerminateProvisionedProduct' {retainPhysicalResources} -> retainPhysicalResources) (\s@TerminateProvisionedProduct' {} a -> s {retainPhysicalResources = a} :: TerminateProvisionedProduct)

-- | An idempotency token that uniquely identifies the termination request.
-- This token is only valid during the termination process. After the
-- provisioned product is terminated, subsequent requests to terminate the
-- same provisioned product always return __ResourceNotFound__.
terminateProvisionedProduct_terminateToken :: Lens.Lens' TerminateProvisionedProduct Prelude.Text
terminateProvisionedProduct_terminateToken = Lens.lens (\TerminateProvisionedProduct' {terminateToken} -> terminateToken) (\s@TerminateProvisionedProduct' {} a -> s {terminateToken = a} :: TerminateProvisionedProduct)

instance Core.AWSRequest TerminateProvisionedProduct where
  type
    AWSResponse TerminateProvisionedProduct =
      TerminateProvisionedProductResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TerminateProvisionedProductResponse'
            Prelude.<$> (x Data..?> "RecordDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateProvisionedProduct where
  hashWithSalt _salt TerminateProvisionedProduct' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` ignoreErrors
      `Prelude.hashWithSalt` provisionedProductId
      `Prelude.hashWithSalt` provisionedProductName
      `Prelude.hashWithSalt` retainPhysicalResources
      `Prelude.hashWithSalt` terminateToken

instance Prelude.NFData TerminateProvisionedProduct where
  rnf TerminateProvisionedProduct' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf ignoreErrors
      `Prelude.seq` Prelude.rnf provisionedProductId
      `Prelude.seq` Prelude.rnf provisionedProductName
      `Prelude.seq` Prelude.rnf retainPhysicalResources
      `Prelude.seq` Prelude.rnf terminateToken

instance Data.ToHeaders TerminateProvisionedProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.TerminateProvisionedProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateProvisionedProduct where
  toJSON TerminateProvisionedProduct' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("IgnoreErrors" Data..=) Prelude.<$> ignoreErrors,
            ("ProvisionedProductId" Data..=)
              Prelude.<$> provisionedProductId,
            ("ProvisionedProductName" Data..=)
              Prelude.<$> provisionedProductName,
            ("RetainPhysicalResources" Data..=)
              Prelude.<$> retainPhysicalResources,
            Prelude.Just
              ("TerminateToken" Data..= terminateToken)
          ]
      )

instance Data.ToPath TerminateProvisionedProduct where
  toPath = Prelude.const "/"

instance Data.ToQuery TerminateProvisionedProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateProvisionedProductResponse' smart constructor.
data TerminateProvisionedProductResponse = TerminateProvisionedProductResponse'
  { -- | Information about the result of this request.
    recordDetail :: Prelude.Maybe RecordDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateProvisionedProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDetail', 'terminateProvisionedProductResponse_recordDetail' - Information about the result of this request.
--
-- 'httpStatus', 'terminateProvisionedProductResponse_httpStatus' - The response's http status code.
newTerminateProvisionedProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TerminateProvisionedProductResponse
newTerminateProvisionedProductResponse pHttpStatus_ =
  TerminateProvisionedProductResponse'
    { recordDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the result of this request.
terminateProvisionedProductResponse_recordDetail :: Lens.Lens' TerminateProvisionedProductResponse (Prelude.Maybe RecordDetail)
terminateProvisionedProductResponse_recordDetail = Lens.lens (\TerminateProvisionedProductResponse' {recordDetail} -> recordDetail) (\s@TerminateProvisionedProductResponse' {} a -> s {recordDetail = a} :: TerminateProvisionedProductResponse)

-- | The response's http status code.
terminateProvisionedProductResponse_httpStatus :: Lens.Lens' TerminateProvisionedProductResponse Prelude.Int
terminateProvisionedProductResponse_httpStatus = Lens.lens (\TerminateProvisionedProductResponse' {httpStatus} -> httpStatus) (\s@TerminateProvisionedProductResponse' {} a -> s {httpStatus = a} :: TerminateProvisionedProductResponse)

instance
  Prelude.NFData
    TerminateProvisionedProductResponse
  where
  rnf TerminateProvisionedProductResponse' {..} =
    Prelude.rnf recordDetail
      `Prelude.seq` Prelude.rnf httpStatus
