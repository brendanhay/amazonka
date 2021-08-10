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
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisionedProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ServiceCatalog.UpdateProvisionedProduct
  ( -- * Creating a Request
    UpdateProvisionedProduct (..),
    newUpdateProvisionedProduct,

    -- * Request Lenses
    updateProvisionedProduct_provisionedProductName,
    updateProvisionedProduct_provisioningPreferences,
    updateProvisionedProduct_provisionedProductId,
    updateProvisionedProduct_provisioningArtifactName,
    updateProvisionedProduct_provisioningArtifactId,
    updateProvisionedProduct_productName,
    updateProvisionedProduct_tags,
    updateProvisionedProduct_productId,
    updateProvisionedProduct_provisioningParameters,
    updateProvisionedProduct_pathId,
    updateProvisionedProduct_acceptLanguage,
    updateProvisionedProduct_pathName,
    updateProvisionedProduct_updateToken,

    -- * Destructuring the Response
    UpdateProvisionedProductResponse (..),
    newUpdateProvisionedProductResponse,

    -- * Response Lenses
    updateProvisionedProductResponse_recordDetail,
    updateProvisionedProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateProvisionedProduct' smart constructor.
data UpdateProvisionedProduct = UpdateProvisionedProduct'
  { -- | The name of the provisioned product. You cannot specify both
    -- @ProvisionedProductName@ and @ProvisionedProductId@.
    provisionedProductName :: Prelude.Maybe Prelude.Text,
    -- | An object that contains information about the provisioning preferences
    -- for a stack set.
    provisioningPreferences :: Prelude.Maybe UpdateProvisioningPreferences,
    -- | The identifier of the provisioned product. You must provide the name or
    -- ID, but not both.
    provisionedProductId :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning artifact. You must provide the name or ID,
    -- but not both.
    provisioningArtifactName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The name of the product. You must provide the name or ID, but not both.
    productName :: Prelude.Maybe Prelude.Text,
    -- | One or more tags. Requires the product to have @RESOURCE_UPDATE@
    -- constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to
    -- allow tag updates.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the product. You must provide the name or ID, but not
    -- both.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The new parameters.
    provisioningParameters :: Prelude.Maybe [UpdateProvisioningParameter],
    -- | The path identifier. This value is optional if the product has a default
    -- path, and required if the product has more than one path. You must
    -- provide the name or ID, but not both.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The name of the path. You must provide the name or ID, but not both.
    pathName :: Prelude.Maybe Prelude.Text,
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
-- 'provisionedProductName', 'updateProvisionedProduct_provisionedProductName' - The name of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
--
-- 'provisioningPreferences', 'updateProvisionedProduct_provisioningPreferences' - An object that contains information about the provisioning preferences
-- for a stack set.
--
-- 'provisionedProductId', 'updateProvisionedProduct_provisionedProductId' - The identifier of the provisioned product. You must provide the name or
-- ID, but not both.
--
-- 'provisioningArtifactName', 'updateProvisionedProduct_provisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
--
-- 'provisioningArtifactId', 'updateProvisionedProduct_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'productName', 'updateProvisionedProduct_productName' - The name of the product. You must provide the name or ID, but not both.
--
-- 'tags', 'updateProvisionedProduct_tags' - One or more tags. Requires the product to have @RESOURCE_UPDATE@
-- constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to
-- allow tag updates.
--
-- 'productId', 'updateProvisionedProduct_productId' - The identifier of the product. You must provide the name or ID, but not
-- both.
--
-- 'provisioningParameters', 'updateProvisionedProduct_provisioningParameters' - The new parameters.
--
-- 'pathId', 'updateProvisionedProduct_pathId' - The path identifier. This value is optional if the product has a default
-- path, and required if the product has more than one path. You must
-- provide the name or ID, but not both.
--
-- 'acceptLanguage', 'updateProvisionedProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pathName', 'updateProvisionedProduct_pathName' - The name of the path. You must provide the name or ID, but not both.
--
-- 'updateToken', 'updateProvisionedProduct_updateToken' - The idempotency token that uniquely identifies the provisioning update
-- request.
newUpdateProvisionedProduct ::
  -- | 'updateToken'
  Prelude.Text ->
  UpdateProvisionedProduct
newUpdateProvisionedProduct pUpdateToken_ =
  UpdateProvisionedProduct'
    { provisionedProductName =
        Prelude.Nothing,
      provisioningPreferences = Prelude.Nothing,
      provisionedProductId = Prelude.Nothing,
      provisioningArtifactName = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      productName = Prelude.Nothing,
      tags = Prelude.Nothing,
      productId = Prelude.Nothing,
      provisioningParameters = Prelude.Nothing,
      pathId = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      pathName = Prelude.Nothing,
      updateToken = pUpdateToken_
    }

-- | The name of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
updateProvisionedProduct_provisionedProductName :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_provisionedProductName = Lens.lens (\UpdateProvisionedProduct' {provisionedProductName} -> provisionedProductName) (\s@UpdateProvisionedProduct' {} a -> s {provisionedProductName = a} :: UpdateProvisionedProduct)

-- | An object that contains information about the provisioning preferences
-- for a stack set.
updateProvisionedProduct_provisioningPreferences :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe UpdateProvisioningPreferences)
updateProvisionedProduct_provisioningPreferences = Lens.lens (\UpdateProvisionedProduct' {provisioningPreferences} -> provisioningPreferences) (\s@UpdateProvisionedProduct' {} a -> s {provisioningPreferences = a} :: UpdateProvisionedProduct)

-- | The identifier of the provisioned product. You must provide the name or
-- ID, but not both.
updateProvisionedProduct_provisionedProductId :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_provisionedProductId = Lens.lens (\UpdateProvisionedProduct' {provisionedProductId} -> provisionedProductId) (\s@UpdateProvisionedProduct' {} a -> s {provisionedProductId = a} :: UpdateProvisionedProduct)

-- | The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
updateProvisionedProduct_provisioningArtifactName :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_provisioningArtifactName = Lens.lens (\UpdateProvisionedProduct' {provisioningArtifactName} -> provisioningArtifactName) (\s@UpdateProvisionedProduct' {} a -> s {provisioningArtifactName = a} :: UpdateProvisionedProduct)

-- | The identifier of the provisioning artifact.
updateProvisionedProduct_provisioningArtifactId :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_provisioningArtifactId = Lens.lens (\UpdateProvisionedProduct' {provisioningArtifactId} -> provisioningArtifactId) (\s@UpdateProvisionedProduct' {} a -> s {provisioningArtifactId = a} :: UpdateProvisionedProduct)

-- | The name of the product. You must provide the name or ID, but not both.
updateProvisionedProduct_productName :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_productName = Lens.lens (\UpdateProvisionedProduct' {productName} -> productName) (\s@UpdateProvisionedProduct' {} a -> s {productName = a} :: UpdateProvisionedProduct)

-- | One or more tags. Requires the product to have @RESOURCE_UPDATE@
-- constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to
-- allow tag updates.
updateProvisionedProduct_tags :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe [Tag])
updateProvisionedProduct_tags = Lens.lens (\UpdateProvisionedProduct' {tags} -> tags) (\s@UpdateProvisionedProduct' {} a -> s {tags = a} :: UpdateProvisionedProduct) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier of the product. You must provide the name or ID, but not
-- both.
updateProvisionedProduct_productId :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_productId = Lens.lens (\UpdateProvisionedProduct' {productId} -> productId) (\s@UpdateProvisionedProduct' {} a -> s {productId = a} :: UpdateProvisionedProduct)

-- | The new parameters.
updateProvisionedProduct_provisioningParameters :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe [UpdateProvisioningParameter])
updateProvisionedProduct_provisioningParameters = Lens.lens (\UpdateProvisionedProduct' {provisioningParameters} -> provisioningParameters) (\s@UpdateProvisionedProduct' {} a -> s {provisioningParameters = a} :: UpdateProvisionedProduct) Prelude.. Lens.mapping Lens._Coerce

-- | The path identifier. This value is optional if the product has a default
-- path, and required if the product has more than one path. You must
-- provide the name or ID, but not both.
updateProvisionedProduct_pathId :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_pathId = Lens.lens (\UpdateProvisionedProduct' {pathId} -> pathId) (\s@UpdateProvisionedProduct' {} a -> s {pathId = a} :: UpdateProvisionedProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateProvisionedProduct_acceptLanguage :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_acceptLanguage = Lens.lens (\UpdateProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@UpdateProvisionedProduct' {} a -> s {acceptLanguage = a} :: UpdateProvisionedProduct)

-- | The name of the path. You must provide the name or ID, but not both.
updateProvisionedProduct_pathName :: Lens.Lens' UpdateProvisionedProduct (Prelude.Maybe Prelude.Text)
updateProvisionedProduct_pathName = Lens.lens (\UpdateProvisionedProduct' {pathName} -> pathName) (\s@UpdateProvisionedProduct' {} a -> s {pathName = a} :: UpdateProvisionedProduct)

-- | The idempotency token that uniquely identifies the provisioning update
-- request.
updateProvisionedProduct_updateToken :: Lens.Lens' UpdateProvisionedProduct Prelude.Text
updateProvisionedProduct_updateToken = Lens.lens (\UpdateProvisionedProduct' {updateToken} -> updateToken) (\s@UpdateProvisionedProduct' {} a -> s {updateToken = a} :: UpdateProvisionedProduct)

instance Core.AWSRequest UpdateProvisionedProduct where
  type
    AWSResponse UpdateProvisionedProduct =
      UpdateProvisionedProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProvisionedProductResponse'
            Prelude.<$> (x Core..?> "RecordDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProvisionedProduct

instance Prelude.NFData UpdateProvisionedProduct

instance Core.ToHeaders UpdateProvisionedProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdateProvisionedProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProvisionedProduct where
  toJSON UpdateProvisionedProduct' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProvisionedProductName" Core..=)
              Prelude.<$> provisionedProductName,
            ("ProvisioningPreferences" Core..=)
              Prelude.<$> provisioningPreferences,
            ("ProvisionedProductId" Core..=)
              Prelude.<$> provisionedProductId,
            ("ProvisioningArtifactName" Core..=)
              Prelude.<$> provisioningArtifactName,
            ("ProvisioningArtifactId" Core..=)
              Prelude.<$> provisioningArtifactId,
            ("ProductName" Core..=) Prelude.<$> productName,
            ("Tags" Core..=) Prelude.<$> tags,
            ("ProductId" Core..=) Prelude.<$> productId,
            ("ProvisioningParameters" Core..=)
              Prelude.<$> provisioningParameters,
            ("PathId" Core..=) Prelude.<$> pathId,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            ("PathName" Core..=) Prelude.<$> pathName,
            Prelude.Just ("UpdateToken" Core..= updateToken)
          ]
      )

instance Core.ToPath UpdateProvisionedProduct where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateProvisionedProduct where
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
