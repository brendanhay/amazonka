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
-- Module      : Amazonka.ServiceCatalog.ImportAsProvisionedProduct
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the import of a resource as an Service Catalog provisioned
-- product that is associated to an Service Catalog product and
-- provisioning artifact. Once imported, all supported Service Catalog
-- governance actions are supported on the provisioned product.
--
-- Resource import only supports CloudFormation stack ARNs. CloudFormation
-- StackSets and non-root nested stacks are not supported.
--
-- The CloudFormation stack must have one of the following statuses to be
-- imported: @CREATE_COMPLETE@, @UPDATE_COMPLETE@,
-- @UPDATE_ROLLBACK_COMPLETE@, @IMPORT_COMPLETE@,
-- @IMPORT_ROLLBACK_COMPLETE@.
--
-- Import of the resource requires that the CloudFormation stack template
-- matches the associated Service Catalog product provisioning artifact.
--
-- The user or role that performs this operation must have the
-- @cloudformation:GetTemplate@ and @cloudformation:DescribeStacks@ IAM
-- policy permissions.
module Amazonka.ServiceCatalog.ImportAsProvisionedProduct
  ( -- * Creating a Request
    ImportAsProvisionedProduct (..),
    newImportAsProvisionedProduct,

    -- * Request Lenses
    importAsProvisionedProduct_acceptLanguage,
    importAsProvisionedProduct_productId,
    importAsProvisionedProduct_provisioningArtifactId,
    importAsProvisionedProduct_provisionedProductName,
    importAsProvisionedProduct_physicalId,
    importAsProvisionedProduct_idempotencyToken,

    -- * Destructuring the Response
    ImportAsProvisionedProductResponse (..),
    newImportAsProvisionedProductResponse,

    -- * Response Lenses
    importAsProvisionedProductResponse_recordDetail,
    importAsProvisionedProductResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newImportAsProvisionedProduct' smart constructor.
data ImportAsProvisionedProduct = ImportAsProvisionedProduct'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Text,
    -- | The user-friendly name of the provisioned product. The value must be
    -- unique for the Amazon Web Services account. The name cannot be updated
    -- after the product is provisioned.
    provisionedProductName :: Prelude.Text,
    -- | The unique identifier of the resource to be imported. It only currently
    -- supports CloudFormation stack IDs.
    physicalId :: Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAsProvisionedProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'importAsProvisionedProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'importAsProvisionedProduct_productId' - The product identifier.
--
-- 'provisioningArtifactId', 'importAsProvisionedProduct_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'provisionedProductName', 'importAsProvisionedProduct_provisionedProductName' - The user-friendly name of the provisioned product. The value must be
-- unique for the Amazon Web Services account. The name cannot be updated
-- after the product is provisioned.
--
-- 'physicalId', 'importAsProvisionedProduct_physicalId' - The unique identifier of the resource to be imported. It only currently
-- supports CloudFormation stack IDs.
--
-- 'idempotencyToken', 'importAsProvisionedProduct_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
newImportAsProvisionedProduct ::
  -- | 'productId'
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  -- | 'provisionedProductName'
  Prelude.Text ->
  -- | 'physicalId'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  ImportAsProvisionedProduct
newImportAsProvisionedProduct
  pProductId_
  pProvisioningArtifactId_
  pProvisionedProductName_
  pPhysicalId_
  pIdempotencyToken_ =
    ImportAsProvisionedProduct'
      { acceptLanguage =
          Prelude.Nothing,
        productId = pProductId_,
        provisioningArtifactId =
          pProvisioningArtifactId_,
        provisionedProductName =
          pProvisionedProductName_,
        physicalId = pPhysicalId_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
importAsProvisionedProduct_acceptLanguage :: Lens.Lens' ImportAsProvisionedProduct (Prelude.Maybe Prelude.Text)
importAsProvisionedProduct_acceptLanguage = Lens.lens (\ImportAsProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@ImportAsProvisionedProduct' {} a -> s {acceptLanguage = a} :: ImportAsProvisionedProduct)

-- | The product identifier.
importAsProvisionedProduct_productId :: Lens.Lens' ImportAsProvisionedProduct Prelude.Text
importAsProvisionedProduct_productId = Lens.lens (\ImportAsProvisionedProduct' {productId} -> productId) (\s@ImportAsProvisionedProduct' {} a -> s {productId = a} :: ImportAsProvisionedProduct)

-- | The identifier of the provisioning artifact.
importAsProvisionedProduct_provisioningArtifactId :: Lens.Lens' ImportAsProvisionedProduct Prelude.Text
importAsProvisionedProduct_provisioningArtifactId = Lens.lens (\ImportAsProvisionedProduct' {provisioningArtifactId} -> provisioningArtifactId) (\s@ImportAsProvisionedProduct' {} a -> s {provisioningArtifactId = a} :: ImportAsProvisionedProduct)

-- | The user-friendly name of the provisioned product. The value must be
-- unique for the Amazon Web Services account. The name cannot be updated
-- after the product is provisioned.
importAsProvisionedProduct_provisionedProductName :: Lens.Lens' ImportAsProvisionedProduct Prelude.Text
importAsProvisionedProduct_provisionedProductName = Lens.lens (\ImportAsProvisionedProduct' {provisionedProductName} -> provisionedProductName) (\s@ImportAsProvisionedProduct' {} a -> s {provisionedProductName = a} :: ImportAsProvisionedProduct)

-- | The unique identifier of the resource to be imported. It only currently
-- supports CloudFormation stack IDs.
importAsProvisionedProduct_physicalId :: Lens.Lens' ImportAsProvisionedProduct Prelude.Text
importAsProvisionedProduct_physicalId = Lens.lens (\ImportAsProvisionedProduct' {physicalId} -> physicalId) (\s@ImportAsProvisionedProduct' {} a -> s {physicalId = a} :: ImportAsProvisionedProduct)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
importAsProvisionedProduct_idempotencyToken :: Lens.Lens' ImportAsProvisionedProduct Prelude.Text
importAsProvisionedProduct_idempotencyToken = Lens.lens (\ImportAsProvisionedProduct' {idempotencyToken} -> idempotencyToken) (\s@ImportAsProvisionedProduct' {} a -> s {idempotencyToken = a} :: ImportAsProvisionedProduct)

instance Core.AWSRequest ImportAsProvisionedProduct where
  type
    AWSResponse ImportAsProvisionedProduct =
      ImportAsProvisionedProductResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportAsProvisionedProductResponse'
            Prelude.<$> (x Data..?> "RecordDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportAsProvisionedProduct where
  hashWithSalt _salt ImportAsProvisionedProduct' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` provisioningArtifactId
      `Prelude.hashWithSalt` provisionedProductName
      `Prelude.hashWithSalt` physicalId
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData ImportAsProvisionedProduct where
  rnf ImportAsProvisionedProduct' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf provisioningArtifactId
      `Prelude.seq` Prelude.rnf provisionedProductName
      `Prelude.seq` Prelude.rnf physicalId
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Data.ToHeaders ImportAsProvisionedProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ImportAsProvisionedProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportAsProvisionedProduct where
  toJSON ImportAsProvisionedProduct' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just
              ( "ProvisioningArtifactId"
                  Data..= provisioningArtifactId
              ),
            Prelude.Just
              ( "ProvisionedProductName"
                  Data..= provisionedProductName
              ),
            Prelude.Just ("PhysicalId" Data..= physicalId),
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath ImportAsProvisionedProduct where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportAsProvisionedProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportAsProvisionedProductResponse' smart constructor.
data ImportAsProvisionedProductResponse = ImportAsProvisionedProductResponse'
  { recordDetail :: Prelude.Maybe RecordDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAsProvisionedProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDetail', 'importAsProvisionedProductResponse_recordDetail' - Undocumented member.
--
-- 'httpStatus', 'importAsProvisionedProductResponse_httpStatus' - The response's http status code.
newImportAsProvisionedProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportAsProvisionedProductResponse
newImportAsProvisionedProductResponse pHttpStatus_ =
  ImportAsProvisionedProductResponse'
    { recordDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
importAsProvisionedProductResponse_recordDetail :: Lens.Lens' ImportAsProvisionedProductResponse (Prelude.Maybe RecordDetail)
importAsProvisionedProductResponse_recordDetail = Lens.lens (\ImportAsProvisionedProductResponse' {recordDetail} -> recordDetail) (\s@ImportAsProvisionedProductResponse' {} a -> s {recordDetail = a} :: ImportAsProvisionedProductResponse)

-- | The response's http status code.
importAsProvisionedProductResponse_httpStatus :: Lens.Lens' ImportAsProvisionedProductResponse Prelude.Int
importAsProvisionedProductResponse_httpStatus = Lens.lens (\ImportAsProvisionedProductResponse' {httpStatus} -> httpStatus) (\s@ImportAsProvisionedProductResponse' {} a -> s {httpStatus = a} :: ImportAsProvisionedProductResponse)

instance
  Prelude.NFData
    ImportAsProvisionedProductResponse
  where
  rnf ImportAsProvisionedProductResponse' {..} =
    Prelude.rnf recordDetail
      `Prelude.seq` Prelude.rnf httpStatus
