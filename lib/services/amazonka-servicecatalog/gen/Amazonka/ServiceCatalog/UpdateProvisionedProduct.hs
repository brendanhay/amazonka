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
-- Module      : Amazonka.ServiceCatalog.UpdateProvisionedProduct
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests updates to the configuration of the specified provisioned
-- product.
--
-- If there are tags associated with the object, they cannot be updated or
-- added. Depending on the specific updates requested, this operation can
-- update with no interruption, with some interruption, or replace the
-- provisioned product entirely.
--
-- You can check the status of this request using DescribeRecord.
module Amazonka.ServiceCatalog.UpdateProvisionedProduct
  ( -- * Creating a Request
    UpdateProvisionedProduct (..),
    newUpdateProvisionedProduct,

    -- * Request Lenses
    updateProvisionedProduct_acceptLanguage,
    updateProvisionedProduct_pathId,
    updateProvisionedProduct_pathName,
    updateProvisionedProduct_productId,
    updateProvisionedProduct_productName,
    updateProvisionedProduct_provisionedProductId,
    updateProvisionedProduct_provisionedProductName,
    updateProvisionedProduct_provisioningArtifactId,
    updateProvisionedProduct_provisioningArtifactName,
    updateProvisionedProduct_provisioningParameters,
    updateProvisionedProduct_provisioningPreferences,
    updateProvisionedProduct_tags,
    updateProvisionedProduct_updateToken,

    -- * Destructuring the Response
    UpdateProvisionedProductResponse (..),
    newUpdateProvisionedProductResponse,

    -- * Response Lenses
    updateProvisionedProductResponse_recordDetail,
    updateProvisionedProductResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newUpdateProvisionedProduct' smart constructor.
data UpdateProvisionedProduct = UpdateProvisionedProduct'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The path identifier. This value is optional if the product has a default
    -- path, and required if the product has more than one path. You must
    -- provide the name or ID, but not both.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | The name of the path. You must provide the name or ID, but not both.
    pathName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the product. You must provide the name or ID, but not
    -- both.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The name of the product. You must provide the name or ID, but not both.
    productName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioned product. You must provide the name or
    -- ID, but not both.
    provisionedProductId :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioned product. You cannot specify both
    -- @ProvisionedProductName@ and @ProvisionedProductId@.
    provisionedProductName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning artifact. You must provide the name or ID,
    -- but not both.
    provisioningArtifactName :: Prelude.Maybe Prelude.Text,
    -- | The new parameters.
    provisioningParameters :: Prelude.Maybe [UpdateProvisioningParameter],
    -- | An object that contains information about the provisioning preferences
    -- for a stack set.
    provisioningPreferences :: Prelude.Maybe UpdateProvisioningPreferences,
    -- | One or more tags. Requires the product to have @RESOURCE_UPDATE@
    -- constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to
    -- allow tag updates.
    tags :: Prelude.Maybe [Tag],
    -- | The idempotency token that uniquely identifies the provisioning update
    -- request.
    updateToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisionedProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'updateProvisionedProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pathId', 'updateProvisionedProduct_pathId' - The path identifier. This value is optional if the product has a default
-- path, and required if the product has more than one path. You must
-- provide the name or ID, but not both.
--
-- 'pathName', 'updateProvisionedProduct_pathName' - The name of the path. You must provide the name or ID, but not both.
--
-- 'productId', 'updateProvisionedProduct_productId' - The identifier of the product. You must provide the name or ID, but not
-- both.
--
-- 'productName', 'updateProvisionedProduct_productName' - The name of the product. You must provide the name or ID, but not both.
--
-- 'provisionedProductId', 'updateProvisionedProduct_provisionedProductId' - The identifier of the provisioned product. You must provide the name or
-- ID, but not both.
--
-- 'provisionedProductName', 'updateProvisionedProduct_provisionedProductName' - The name of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
--
-- 'provisioningArtifactId', 'updateProvisionedProduct_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'provisioningArtifactName', 'updateProvisionedProduct_provisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
--
-- 'provisioningParameters', 'updateProvisionedProduct_provisioningParameters' - The new parameters.
--
-- 'provisioningPreferences', 'updateProvisionedProduct_provisioningPreferences' - An object that contains information about the provisioning preferences
-- for a stack set.
--
-- 'tags', 'updateProvisionedProduct_tags' - One or more tags. Requires the product to have @RESOURCE_UPDATE@
-- constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to
-- allow tag updates.
--
-- 'updateToken', 'updateProvisionedProduct_updateToken' - The idempotency token that uniquely identifies the provisioning update
-- request.
newUpdateProvisionedProduct ::
  -- | 'updateToken'
  Prelude.Text ->
  UpdateProvisionedProduct
newUpdateProvisionedProduct pUpdateToken_ =
  UpdateProvisionedProduct'
    { acceptLanguage =
        Prelude.Nothing,
      pathId = Prelude.Nothing,
      pathName = Prelude.Nothing,
      productId = Prelude.Nothing,
      productName = Prelude.Nothing,
      provisionedProductId = Prelude.Nothing,
      provisionedProductName = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      provisioningArtifactName = Prelude.Nothing,
      provisioningParameters = Prelude.Nothing,
      provisioningPreferences = Prelude.Nothing,
      tags = Prelude.Nothing,
      updateToken = pUpdateToken_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateProvisionedProduct_acceptLanguage :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_acceptLanguage = Lens.lens (\UpdateProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@UpdateProvisionedProduct' {} a -> s {acceptLanguage = a} :: UpdateProvisionedProduct)

-- | The path identifier. This value is optional if the product has a default
-- path, and required if the product has more than one path. You must
-- provide the name or ID, but not both.
updateProvisionedProduct_pathId :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_pathId = Lens.lens (\UpdateProvisionedProduct' {pathId} -> pathId) (\s@UpdateProvisionedProduct' {} a -> s {pathId = a} :: UpdateProvisionedProduct)

-- | The name of the path. You must provide the name or ID, but not both.
updateProvisionedProduct_pathName :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_pathName = Lens.lens (\UpdateProvisionedProduct' {pathName} -> pathName) (\s@UpdateProvisionedProduct' {} a -> s {pathName = a} :: UpdateProvisionedProduct)

-- | The identifier of the product. You must provide the name or ID, but not
-- both.
updateProvisionedProduct_productId :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_productId = Lens.lens (\UpdateProvisionedProduct' {productId} -> productId) (\s@UpdateProvisionedProduct' {} a -> s {productId = a} :: UpdateProvisionedProduct)

-- | The name of the product. You must provide the name or ID, but not both.
updateProvisionedProduct_productName :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_productName = Lens.lens (\UpdateProvisionedProduct' {productName} -> productName) (\s@UpdateProvisionedProduct' {} a -> s {productName = a} :: UpdateProvisionedProduct)

-- | The identifier of the provisioned product. You must provide the name or
-- ID, but not both.
updateProvisionedProduct_provisionedProductId :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_provisionedProductId = Lens.lens (\UpdateProvisionedProduct' {provisionedProductId} -> provisionedProductId) (\s@UpdateProvisionedProduct' {} a -> s {provisionedProductId = a} :: UpdateProvisionedProduct)

-- | The name of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
updateProvisionedProduct_provisionedProductName :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_provisionedProductName = Lens.lens (\UpdateProvisionedProduct' {provisionedProductName} -> provisionedProductName) (\s@UpdateProvisionedProduct' {} a -> s {provisionedProductName = a} :: UpdateProvisionedProduct)

-- | The identifier of the provisioning artifact.
updateProvisionedProduct_provisioningArtifactId :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_provisioningArtifactId = Lens.lens (\UpdateProvisionedProduct' {provisioningArtifactId} -> provisioningArtifactId) (\s@UpdateProvisionedProduct' {} a -> s {provisioningArtifactId = a} :: UpdateProvisionedProduct)

-- | The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
updateProvisionedProduct_provisioningArtifactName :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_provisioningArtifactName = Lens.lens (\UpdateProvisionedProduct' {provisioningArtifactName} -> provisioningArtifactName) (\s@UpdateProvisionedProduct' {} a -> s {provisioningArtifactName = a} :: UpdateProvisionedProduct)

-- | The new parameters.
updateProvisionedProduct_provisioningParameters :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe [UpdateProvisioningParameter])
updateProvisionedProduct_provisioningParameters = Lens.lens (\UpdateProvisionedProduct' {provisioningParameters} -> provisioningParameters) (\s@UpdateProvisionedProduct' {} a -> s {provisioningParameters = a} :: UpdateProvisionedProduct) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains information about the provisioning preferences
-- for a stack set.
updateProvisionedProduct_provisioningPreferences :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe UpdateProvisioningPreferences)
updateProvisionedProduct_provisioningPreferences = Lens.lens (\UpdateProvisionedProduct' {provisioningPreferences} -> provisioningPreferences) (\s@UpdateProvisionedProduct' {} a -> s {provisioningPreferences = a} :: UpdateProvisionedProduct)

-- | One or more tags. Requires the product to have @RESOURCE_UPDATE@
-- constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to
-- allow tag updates.
updateProvisionedProduct_tags :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe [Tag])
updateProvisionedProduct_tags = Lens.lens (\UpdateProvisionedProduct' {tags} -> tags) (\s@UpdateProvisionedProduct' {} a -> s {tags = a} :: UpdateProvisionedProduct) Prelude.. Lens.mapping Lens.coerced

-- | The idempotency token that uniquely identifies the provisioning update
-- request.
updateProvisionedProduct_updateToken :: Lens.Lens' UpdateProvisionedProduct Prelude.Text
updateProvisionedProduct_updateToken = Lens.lens (\UpdateProvisionedProduct' {updateToken} -> updateToken) (\s@UpdateProvisionedProduct' {} a -> s {updateToken = a} :: UpdateProvisionedProduct)

instance Core.AWSRequest UpdateProvisionedProduct where
  type
    AWSResponse UpdateProvisionedProduct =
      UpdateProvisionedProductResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProvisionedProductResponse'
            Prelude.<$> (x Data..?> "RecordDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProvisionedProduct where
  hashWithSalt _salt UpdateProvisionedProduct' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` pathId
      `Prelude.hashWithSalt` pathName
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` provisionedProductId
      `Prelude.hashWithSalt` provisionedProductName
      `Prelude.hashWithSalt` provisioningArtifactId
      `Prelude.hashWithSalt` provisioningArtifactName
      `Prelude.hashWithSalt` provisioningParameters
      `Prelude.hashWithSalt` provisioningPreferences
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` updateToken

instance Prelude.NFData UpdateProvisionedProduct where
  rnf UpdateProvisionedProduct' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf pathId
      `Prelude.seq` Prelude.rnf pathName
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf productName
      `Prelude.seq` Prelude.rnf provisionedProductId
      `Prelude.seq` Prelude.rnf provisionedProductName
      `Prelude.seq` Prelude.rnf provisioningArtifactId
      `Prelude.seq` Prelude.rnf provisioningArtifactName
      `Prelude.seq` Prelude.rnf provisioningParameters
      `Prelude.seq` Prelude.rnf provisioningPreferences
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updateToken

instance Data.ToHeaders UpdateProvisionedProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.UpdateProvisionedProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProvisionedProduct where
  toJSON UpdateProvisionedProduct' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("PathId" Data..=) Prelude.<$> pathId,
            ("PathName" Data..=) Prelude.<$> pathName,
            ("ProductId" Data..=) Prelude.<$> productId,
            ("ProductName" Data..=) Prelude.<$> productName,
            ("ProvisionedProductId" Data..=)
              Prelude.<$> provisionedProductId,
            ("ProvisionedProductName" Data..=)
              Prelude.<$> provisionedProductName,
            ("ProvisioningArtifactId" Data..=)
              Prelude.<$> provisioningArtifactId,
            ("ProvisioningArtifactName" Data..=)
              Prelude.<$> provisioningArtifactName,
            ("ProvisioningParameters" Data..=)
              Prelude.<$> provisioningParameters,
            ("ProvisioningPreferences" Data..=)
              Prelude.<$> provisioningPreferences,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("UpdateToken" Data..= updateToken)
          ]
      )

instance Data.ToPath UpdateProvisionedProduct where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProvisionedProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProvisionedProductResponse' smart constructor.
data UpdateProvisionedProductResponse = UpdateProvisionedProductResponse'
  { -- | Information about the result of the request.
    recordDetail :: Prelude.Maybe RecordDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisionedProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDetail', 'updateProvisionedProductResponse_recordDetail' - Information about the result of the request.
--
-- 'httpStatus', 'updateProvisionedProductResponse_httpStatus' - The response's http status code.
newUpdateProvisionedProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProvisionedProductResponse
newUpdateProvisionedProductResponse pHttpStatus_ =
  UpdateProvisionedProductResponse'
    { recordDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the result of the request.
updateProvisionedProductResponse_recordDetail :: Lens.Lens' UpdateProvisionedProductResponse (Prelude.Maybe RecordDetail)
updateProvisionedProductResponse_recordDetail = Lens.lens (\UpdateProvisionedProductResponse' {recordDetail} -> recordDetail) (\s@UpdateProvisionedProductResponse' {} a -> s {recordDetail = a} :: UpdateProvisionedProductResponse)

-- | The response's http status code.
updateProvisionedProductResponse_httpStatus :: Lens.Lens' UpdateProvisionedProductResponse Prelude.Int
updateProvisionedProductResponse_httpStatus = Lens.lens (\UpdateProvisionedProductResponse' {httpStatus} -> httpStatus) (\s@UpdateProvisionedProductResponse' {} a -> s {httpStatus = a} :: UpdateProvisionedProductResponse)

instance
  Prelude.NFData
    UpdateProvisionedProductResponse
  where
  rnf UpdateProvisionedProductResponse' {..} =
    Prelude.rnf recordDetail
      `Prelude.seq` Prelude.rnf httpStatus
