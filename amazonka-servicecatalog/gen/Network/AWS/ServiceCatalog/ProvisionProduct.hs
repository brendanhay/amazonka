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
-- Module      : Network.AWS.ServiceCatalog.ProvisionProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions the specified product.
--
-- A provisioned product is a resourced instance of a product. For example,
-- provisioning a product based on a CloudFormation template launches a
-- CloudFormation stack and its underlying resources. You can check the
-- status of this request using DescribeRecord.
--
-- If the request contains a tag key with an empty list of values, there is
-- a tag conflict for that key. Do not include conflicted keys as tags, or
-- this causes the error \"Parameter validation failed: Missing required
-- parameter in Tags[/N/]:/Value/\".
module Network.AWS.ServiceCatalog.ProvisionProduct
  ( -- * Creating a Request
    ProvisionProduct (..),
    newProvisionProduct,

    -- * Request Lenses
    provisionProduct_provisioningPreferences,
    provisionProduct_notificationArns,
    provisionProduct_provisioningArtifactName,
    provisionProduct_provisioningArtifactId,
    provisionProduct_productName,
    provisionProduct_tags,
    provisionProduct_productId,
    provisionProduct_provisioningParameters,
    provisionProduct_pathId,
    provisionProduct_acceptLanguage,
    provisionProduct_pathName,
    provisionProduct_provisionedProductName,
    provisionProduct_provisionToken,

    -- * Destructuring the Response
    ProvisionProductResponse (..),
    newProvisionProductResponse,

    -- * Response Lenses
    provisionProductResponse_recordDetail,
    provisionProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newProvisionProduct' smart constructor.
data ProvisionProduct = ProvisionProduct'
  { -- | An object that contains information about the provisioning preferences
    -- for a stack set.
    provisioningPreferences :: Core.Maybe ProvisioningPreferences,
    -- | Passed to CloudFormation. The SNS topic ARNs to which to publish
    -- stack-related events.
    notificationArns :: Core.Maybe [Core.Text],
    -- | The name of the provisioning artifact. You must provide the name or ID,
    -- but not both.
    provisioningArtifactName :: Core.Maybe Core.Text,
    -- | The identifier of the provisioning artifact. You must provide the name
    -- or ID, but not both.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The name of the product. You must provide the name or ID, but not both.
    productName :: Core.Maybe Core.Text,
    -- | One or more tags.
    tags :: Core.Maybe [Tag],
    -- | The product identifier. You must provide the name or ID, but not both.
    productId :: Core.Maybe Core.Text,
    -- | Parameters specified by the administrator that are required for
    -- provisioning the product.
    provisioningParameters :: Core.Maybe [ProvisioningParameter],
    -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path. To list the paths for a product, use ListLaunchPaths. You must
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
    -- | A user-friendly name for the provisioned product. This value must be
    -- unique for the AWS account and cannot be updated after the product is
    -- provisioned.
    provisionedProductName :: Core.Text,
    -- | An idempotency token that uniquely identifies the provisioning request.
    provisionToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisionProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningPreferences', 'provisionProduct_provisioningPreferences' - An object that contains information about the provisioning preferences
-- for a stack set.
--
-- 'notificationArns', 'provisionProduct_notificationArns' - Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
--
-- 'provisioningArtifactName', 'provisionProduct_provisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
--
-- 'provisioningArtifactId', 'provisionProduct_provisioningArtifactId' - The identifier of the provisioning artifact. You must provide the name
-- or ID, but not both.
--
-- 'productName', 'provisionProduct_productName' - The name of the product. You must provide the name or ID, but not both.
--
-- 'tags', 'provisionProduct_tags' - One or more tags.
--
-- 'productId', 'provisionProduct_productId' - The product identifier. You must provide the name or ID, but not both.
--
-- 'provisioningParameters', 'provisionProduct_provisioningParameters' - Parameters specified by the administrator that are required for
-- provisioning the product.
--
-- 'pathId', 'provisionProduct_pathId' - The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths. You must
-- provide the name or ID, but not both.
--
-- 'acceptLanguage', 'provisionProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pathName', 'provisionProduct_pathName' - The name of the path. You must provide the name or ID, but not both.
--
-- 'provisionedProductName', 'provisionProduct_provisionedProductName' - A user-friendly name for the provisioned product. This value must be
-- unique for the AWS account and cannot be updated after the product is
-- provisioned.
--
-- 'provisionToken', 'provisionProduct_provisionToken' - An idempotency token that uniquely identifies the provisioning request.
newProvisionProduct ::
  -- | 'provisionedProductName'
  Core.Text ->
  -- | 'provisionToken'
  Core.Text ->
  ProvisionProduct
newProvisionProduct
  pProvisionedProductName_
  pProvisionToken_ =
    ProvisionProduct'
      { provisioningPreferences =
          Core.Nothing,
        notificationArns = Core.Nothing,
        provisioningArtifactName = Core.Nothing,
        provisioningArtifactId = Core.Nothing,
        productName = Core.Nothing,
        tags = Core.Nothing,
        productId = Core.Nothing,
        provisioningParameters = Core.Nothing,
        pathId = Core.Nothing,
        acceptLanguage = Core.Nothing,
        pathName = Core.Nothing,
        provisionedProductName = pProvisionedProductName_,
        provisionToken = pProvisionToken_
      }

-- | An object that contains information about the provisioning preferences
-- for a stack set.
provisionProduct_provisioningPreferences :: Lens.Lens' ProvisionProduct (Core.Maybe ProvisioningPreferences)
provisionProduct_provisioningPreferences = Lens.lens (\ProvisionProduct' {provisioningPreferences} -> provisioningPreferences) (\s@ProvisionProduct' {} a -> s {provisioningPreferences = a} :: ProvisionProduct)

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish
-- stack-related events.
provisionProduct_notificationArns :: Lens.Lens' ProvisionProduct (Core.Maybe [Core.Text])
provisionProduct_notificationArns = Lens.lens (\ProvisionProduct' {notificationArns} -> notificationArns) (\s@ProvisionProduct' {} a -> s {notificationArns = a} :: ProvisionProduct) Core.. Lens.mapping Lens._Coerce

-- | The name of the provisioning artifact. You must provide the name or ID,
-- but not both.
provisionProduct_provisioningArtifactName :: Lens.Lens' ProvisionProduct (Core.Maybe Core.Text)
provisionProduct_provisioningArtifactName = Lens.lens (\ProvisionProduct' {provisioningArtifactName} -> provisioningArtifactName) (\s@ProvisionProduct' {} a -> s {provisioningArtifactName = a} :: ProvisionProduct)

-- | The identifier of the provisioning artifact. You must provide the name
-- or ID, but not both.
provisionProduct_provisioningArtifactId :: Lens.Lens' ProvisionProduct (Core.Maybe Core.Text)
provisionProduct_provisioningArtifactId = Lens.lens (\ProvisionProduct' {provisioningArtifactId} -> provisioningArtifactId) (\s@ProvisionProduct' {} a -> s {provisioningArtifactId = a} :: ProvisionProduct)

-- | The name of the product. You must provide the name or ID, but not both.
provisionProduct_productName :: Lens.Lens' ProvisionProduct (Core.Maybe Core.Text)
provisionProduct_productName = Lens.lens (\ProvisionProduct' {productName} -> productName) (\s@ProvisionProduct' {} a -> s {productName = a} :: ProvisionProduct)

-- | One or more tags.
provisionProduct_tags :: Lens.Lens' ProvisionProduct (Core.Maybe [Tag])
provisionProduct_tags = Lens.lens (\ProvisionProduct' {tags} -> tags) (\s@ProvisionProduct' {} a -> s {tags = a} :: ProvisionProduct) Core.. Lens.mapping Lens._Coerce

-- | The product identifier. You must provide the name or ID, but not both.
provisionProduct_productId :: Lens.Lens' ProvisionProduct (Core.Maybe Core.Text)
provisionProduct_productId = Lens.lens (\ProvisionProduct' {productId} -> productId) (\s@ProvisionProduct' {} a -> s {productId = a} :: ProvisionProduct)

-- | Parameters specified by the administrator that are required for
-- provisioning the product.
provisionProduct_provisioningParameters :: Lens.Lens' ProvisionProduct (Core.Maybe [ProvisioningParameter])
provisionProduct_provisioningParameters = Lens.lens (\ProvisionProduct' {provisioningParameters} -> provisioningParameters) (\s@ProvisionProduct' {} a -> s {provisioningParameters = a} :: ProvisionProduct) Core.. Lens.mapping Lens._Coerce

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path. To list the paths for a product, use ListLaunchPaths. You must
-- provide the name or ID, but not both.
provisionProduct_pathId :: Lens.Lens' ProvisionProduct (Core.Maybe Core.Text)
provisionProduct_pathId = Lens.lens (\ProvisionProduct' {pathId} -> pathId) (\s@ProvisionProduct' {} a -> s {pathId = a} :: ProvisionProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
provisionProduct_acceptLanguage :: Lens.Lens' ProvisionProduct (Core.Maybe Core.Text)
provisionProduct_acceptLanguage = Lens.lens (\ProvisionProduct' {acceptLanguage} -> acceptLanguage) (\s@ProvisionProduct' {} a -> s {acceptLanguage = a} :: ProvisionProduct)

-- | The name of the path. You must provide the name or ID, but not both.
provisionProduct_pathName :: Lens.Lens' ProvisionProduct (Core.Maybe Core.Text)
provisionProduct_pathName = Lens.lens (\ProvisionProduct' {pathName} -> pathName) (\s@ProvisionProduct' {} a -> s {pathName = a} :: ProvisionProduct)

-- | A user-friendly name for the provisioned product. This value must be
-- unique for the AWS account and cannot be updated after the product is
-- provisioned.
provisionProduct_provisionedProductName :: Lens.Lens' ProvisionProduct Core.Text
provisionProduct_provisionedProductName = Lens.lens (\ProvisionProduct' {provisionedProductName} -> provisionedProductName) (\s@ProvisionProduct' {} a -> s {provisionedProductName = a} :: ProvisionProduct)

-- | An idempotency token that uniquely identifies the provisioning request.
provisionProduct_provisionToken :: Lens.Lens' ProvisionProduct Core.Text
provisionProduct_provisionToken = Lens.lens (\ProvisionProduct' {provisionToken} -> provisionToken) (\s@ProvisionProduct' {} a -> s {provisionToken = a} :: ProvisionProduct)

instance Core.AWSRequest ProvisionProduct where
  type
    AWSResponse ProvisionProduct =
      ProvisionProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ProvisionProductResponse'
            Core.<$> (x Core..?> "RecordDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ProvisionProduct

instance Core.NFData ProvisionProduct

instance Core.ToHeaders ProvisionProduct where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ProvisionProduct" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ProvisionProduct where
  toJSON ProvisionProduct' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProvisioningPreferences" Core..=)
              Core.<$> provisioningPreferences,
            ("NotificationArns" Core..=)
              Core.<$> notificationArns,
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
            Core.Just
              ( "ProvisionedProductName"
                  Core..= provisionedProductName
              ),
            Core.Just ("ProvisionToken" Core..= provisionToken)
          ]
      )

instance Core.ToPath ProvisionProduct where
  toPath = Core.const "/"

instance Core.ToQuery ProvisionProduct where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newProvisionProductResponse' smart constructor.
data ProvisionProductResponse = ProvisionProductResponse'
  { -- | Information about the result of provisioning the product.
    recordDetail :: Core.Maybe RecordDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisionProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDetail', 'provisionProductResponse_recordDetail' - Information about the result of provisioning the product.
--
-- 'httpStatus', 'provisionProductResponse_httpStatus' - The response's http status code.
newProvisionProductResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ProvisionProductResponse
newProvisionProductResponse pHttpStatus_ =
  ProvisionProductResponse'
    { recordDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the result of provisioning the product.
provisionProductResponse_recordDetail :: Lens.Lens' ProvisionProductResponse (Core.Maybe RecordDetail)
provisionProductResponse_recordDetail = Lens.lens (\ProvisionProductResponse' {recordDetail} -> recordDetail) (\s@ProvisionProductResponse' {} a -> s {recordDetail = a} :: ProvisionProductResponse)

-- | The response's http status code.
provisionProductResponse_httpStatus :: Lens.Lens' ProvisionProductResponse Core.Int
provisionProductResponse_httpStatus = Lens.lens (\ProvisionProductResponse' {httpStatus} -> httpStatus) (\s@ProvisionProductResponse' {} a -> s {httpStatus = a} :: ProvisionProductResponse)

instance Core.NFData ProvisionProductResponse
