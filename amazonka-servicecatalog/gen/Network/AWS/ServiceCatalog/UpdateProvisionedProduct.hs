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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateProvisionedProduct' smart constructor.
data UpdateProvisionedProduct = UpdateProvisionedProduct'
  { -- | The name of the provisioned product. You cannot specify both
    -- @ProvisionedProductName@ and @ProvisionedProductId@.
    provisionedProductName :: Core.Maybe Core.Text,
    -- | An object that contains information about the provisioning preferences
    -- for a stack set.
    provisioningPreferences :: Core.Maybe UpdateProvisioningPreferences,
    -- | The identifier of the provisioned product. You must provide the name or
    -- ID, but not both.
    provisionedProductId :: Core.Maybe Core.Text,
    -- | The name of the provisioning artifact. You must provide the name or ID,
    -- but not both.
    provisioningArtifactName :: Core.Maybe Core.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The name of the product. You must provide the name or ID, but not both.
    productName :: Core.Maybe Core.Text,
    -- | One or more tags. Requires the product to have @RESOURCE_UPDATE@
    -- constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to
    -- allow tag updates.
    tags :: Core.Maybe [Tag],
    -- | The identifier of the product. You must provide the name or ID, but not
    -- both.
    productId :: Core.Maybe Core.Text,
    -- | The new parameters.
    provisioningParameters :: Core.Maybe [UpdateProvisioningParameter],
    -- | The path identifier. This value is optional if the product has a default
    -- path, and required if the product has more than one path. You must
    -- provide the name or ID, but not both.
    pathId :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The name of the path. You must provide the name or ID, but not both.
    pathName :: Core.Maybe Core.Text,
    -- | The idempotency token that uniquely identifies the provisioning update
    -- request.
    updateToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateProvisionedProduct
newUpdateProvisionedProduct pUpdateToken_ =
  UpdateProvisionedProduct'
    { provisionedProductName =
        Core.Nothing,
      provisioningPreferences = Core.Nothing,
      provisionedProductId = Core.Nothing,
      provisioningArtifactName = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      productName = Core.Nothing,
      tags = Core.Nothing,
      productId = Core.Nothing,
      provisioningParameters = Core.Nothing,
      pathId = Core.Nothing,
      acceptLanguage = Core.Nothing,
      pathName = Core.Nothing,
      updateToken = pUpdateToken_
    }

-- | The name of the provisioned product. You cannot specify both
-- @ProvisionedProductName@ and @ProvisionedProductId@.
updateProvisionedProduct_provisionedProductName :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_provisionedProductName = Lens.lens (\UpdateProvisionedProduct' {provisionedProductName} -> provisionedProductName) (\s@UpdateProvisionedProduct' {} a -> s {provisionedProductName = a} :: UpdateProvisionedProduct)

-- | An object that contains information about the provisioning preferences
-- for a stack set.
updateProvisionedProduct_provisioningPreferences :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe UpdateProvisioningPreferences)
updateProvisionedProduct_provisioningPreferences = Lens.lens (\UpdateProvisionedProduct' {provisioningPreferences} -> provisioningPreferences) (\s@UpdateProvisionedProduct' {} a -> s {provisioningPreferences = a} :: UpdateProvisionedProduct)

-- | The identifier of the provisioned product. You must provide the name or
-- ID, but not both.
updateProvisionedProduct_provisionedProductId :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_provisionedProductId = Lens.lens (\UpdateProvisionedProduct' {provisionedProductId} -> provisionedProductId) (\s@UpdateProvisionedProduct' {} a -> s {provisionedProductId = a} :: UpdateProvisionedProduct)

-- | The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
updateProvisionedProduct_provisioningArtifactName :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_provisioningArtifactName = Lens.lens (\UpdateProvisionedProduct' {provisioningArtifactName} -> provisioningArtifactName) (\s@UpdateProvisionedProduct' {} a -> s {provisioningArtifactName = a} :: UpdateProvisionedProduct)

-- | The identifier of the provisioning artifact.
updateProvisionedProduct_provisioningArtifactId :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_provisioningArtifactId = Lens.lens (\UpdateProvisionedProduct' {provisioningArtifactId} -> provisioningArtifactId) (\s@UpdateProvisionedProduct' {} a -> s {provisioningArtifactId = a} :: UpdateProvisionedProduct)

-- | The name of the product. You must provide the name or ID, but not both.
updateProvisionedProduct_productName :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_productName = Lens.lens (\UpdateProvisionedProduct' {productName} -> productName) (\s@UpdateProvisionedProduct' {} a -> s {productName = a} :: UpdateProvisionedProduct)

-- | One or more tags. Requires the product to have @RESOURCE_UPDATE@
-- constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to
-- allow tag updates.
updateProvisionedProduct_tags :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe [Tag])
updateProvisionedProduct_tags = Lens.lens (\UpdateProvisionedProduct' {tags} -> tags) (\s@UpdateProvisionedProduct' {} a -> s {tags = a} :: UpdateProvisionedProduct) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the product. You must provide the name or ID, but not
-- both.
updateProvisionedProduct_productId :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_productId = Lens.lens (\UpdateProvisionedProduct' {productId} -> productId) (\s@UpdateProvisionedProduct' {} a -> s {productId = a} :: UpdateProvisionedProduct)

-- | The new parameters.
updateProvisionedProduct_provisioningParameters :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe [UpdateProvisioningParameter])
updateProvisionedProduct_provisioningParameters = Lens.lens (\UpdateProvisionedProduct' {provisioningParameters} -> provisioningParameters) (\s@UpdateProvisionedProduct' {} a -> s {provisioningParameters = a} :: UpdateProvisionedProduct) Core.. Lens.mapping Lens._Coerce

-- | The path identifier. This value is optional if the product has a default
-- path, and required if the product has more than one path. You must
-- provide the name or ID, but not both.
updateProvisionedProduct_pathId :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_pathId = Lens.lens (\UpdateProvisionedProduct' {pathId} -> pathId) (\s@UpdateProvisionedProduct' {} a -> s {pathId = a} :: UpdateProvisionedProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateProvisionedProduct_acceptLanguage :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_acceptLanguage = Lens.lens (\UpdateProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@UpdateProvisionedProduct' {} a -> s {acceptLanguage = a} :: UpdateProvisionedProduct)

-- | The name of the path. You must provide the name or ID, but not both.
updateProvisionedProduct_pathName :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Core.Text)
updateProvisionedProduct_pathName = Lens.lens (\UpdateProvisionedProduct' {pathName} -> pathName) (\s@UpdateProvisionedProduct' {} a -> s {pathName = a} :: UpdateProvisionedProduct)

-- | The idempotency token that uniquely identifies the provisioning update
-- request.
updateProvisionedProduct_updateToken :: Lens.Lens' UpdateProvisionedProduct Core.Text
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
            Core.<$> (x Core..?> "RecordDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateProvisionedProduct

instance Core.NFData UpdateProvisionedProduct

instance Core.ToHeaders UpdateProvisionedProduct where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdateProvisionedProduct" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateProvisionedProduct where
  toJSON UpdateProvisionedProduct' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProvisionedProductName" Core..=)
              Core.<$> provisionedProductName,
            ("ProvisioningPreferences" Core..=)
              Core.<$> provisioningPreferences,
            ("ProvisionedProductId" Core..=)
              Core.<$> provisionedProductId,
            ("ProvisioningArtifactName" Core..=)
              Core.<$> provisioningArtifactName,
            ("ProvisioningArtifactId" Core..=)
              Core.<$> provisioningArtifactId,
            ("ProductName" Core..=) Core.<$> productName,
            ("Tags" Core..=) Core.<$> tags,
            ("ProductId" Core..=) Core.<$> productId,
            ("ProvisioningParameters" Core..=)
              Core.<$> provisioningParameters,
            ("PathId" Core..=) Core.<$> pathId,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PathName" Core..=) Core.<$> pathName,
            Core.Just ("UpdateToken" Core..= updateToken)
          ]
      )

instance Core.ToPath UpdateProvisionedProduct where
  toPath = Core.const "/"

instance Core.ToQuery UpdateProvisionedProduct where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateProvisionedProductResponse' smart constructor.
data UpdateProvisionedProductResponse = UpdateProvisionedProductResponse'
  { -- | Information about the result of the request.
    recordDetail :: Core.Maybe RecordDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateProvisionedProductResponse
newUpdateProvisionedProductResponse pHttpStatus_ =
  UpdateProvisionedProductResponse'
    { recordDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the result of the request.
updateProvisionedProductResponse_recordDetail :: Lens.Lens' UpdateProvisionedProductResponse (Core.Maybe RecordDetail)
updateProvisionedProductResponse_recordDetail = Lens.lens (\UpdateProvisionedProductResponse' {recordDetail} -> recordDetail) (\s@UpdateProvisionedProductResponse' {} a -> s {recordDetail = a} :: UpdateProvisionedProductResponse)

-- | The response's http status code.
updateProvisionedProductResponse_httpStatus :: Lens.Lens' UpdateProvisionedProductResponse Core.Int
updateProvisionedProductResponse_httpStatus = Lens.lens (\UpdateProvisionedProductResponse' {httpStatus} -> httpStatus) (\s@UpdateProvisionedProductResponse' {} a -> s {httpStatus = a} :: UpdateProvisionedProductResponse)

instance Core.NFData UpdateProvisionedProductResponse
