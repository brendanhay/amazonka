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
-- Module      : Network.AWS.ServiceCatalog.CopyProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified source product to the specified target product or a
-- new product.
--
-- You can copy a product to the same account or another account. You can
-- copy a product to the same region or another region.
--
-- This operation is performed asynchronously. To track the progress of the
-- operation, use DescribeCopyProductStatus.
module Network.AWS.ServiceCatalog.CopyProduct
  ( -- * Creating a Request
    CopyProduct (..),
    newCopyProduct,

    -- * Request Lenses
    copyProduct_targetProductName,
    copyProduct_copyOptions,
    copyProduct_targetProductId,
    copyProduct_sourceProvisioningArtifactIdentifiers,
    copyProduct_acceptLanguage,
    copyProduct_sourceProductArn,
    copyProduct_idempotencyToken,

    -- * Destructuring the Response
    CopyProductResponse (..),
    newCopyProductResponse,

    -- * Response Lenses
    copyProductResponse_copyProductToken,
    copyProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCopyProduct' smart constructor.
data CopyProduct = CopyProduct'
  { -- | A name for the target product. The default is the name of the source
    -- product.
    targetProductName :: Core.Maybe Core.Text,
    -- | The copy options. If the value is @CopyTags@, the tags from the source
    -- product are copied to the target product.
    copyOptions :: Core.Maybe [CopyOption],
    -- | The identifier of the target product. By default, a new product is
    -- created.
    targetProductId :: Core.Maybe Core.Text,
    -- | The identifiers of the provisioning artifacts (also known as versions)
    -- of the product to copy. By default, all provisioning artifacts are
    -- copied.
    sourceProvisioningArtifactIdentifiers :: Core.Maybe [Core.HashMap ProvisioningArtifactPropertyName Core.Text],
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the source product.
    sourceProductArn :: Core.Text,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetProductName', 'copyProduct_targetProductName' - A name for the target product. The default is the name of the source
-- product.
--
-- 'copyOptions', 'copyProduct_copyOptions' - The copy options. If the value is @CopyTags@, the tags from the source
-- product are copied to the target product.
--
-- 'targetProductId', 'copyProduct_targetProductId' - The identifier of the target product. By default, a new product is
-- created.
--
-- 'sourceProvisioningArtifactIdentifiers', 'copyProduct_sourceProvisioningArtifactIdentifiers' - The identifiers of the provisioning artifacts (also known as versions)
-- of the product to copy. By default, all provisioning artifacts are
-- copied.
--
-- 'acceptLanguage', 'copyProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'sourceProductArn', 'copyProduct_sourceProductArn' - The Amazon Resource Name (ARN) of the source product.
--
-- 'idempotencyToken', 'copyProduct_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
newCopyProduct ::
  -- | 'sourceProductArn'
  Core.Text ->
  -- | 'idempotencyToken'
  Core.Text ->
  CopyProduct
newCopyProduct pSourceProductArn_ pIdempotencyToken_ =
  CopyProduct'
    { targetProductName = Core.Nothing,
      copyOptions = Core.Nothing,
      targetProductId = Core.Nothing,
      sourceProvisioningArtifactIdentifiers = Core.Nothing,
      acceptLanguage = Core.Nothing,
      sourceProductArn = pSourceProductArn_,
      idempotencyToken = pIdempotencyToken_
    }

-- | A name for the target product. The default is the name of the source
-- product.
copyProduct_targetProductName :: Lens.Lens' CopyProduct (Core.Maybe Core.Text)
copyProduct_targetProductName = Lens.lens (\CopyProduct' {targetProductName} -> targetProductName) (\s@CopyProduct' {} a -> s {targetProductName = a} :: CopyProduct)

-- | The copy options. If the value is @CopyTags@, the tags from the source
-- product are copied to the target product.
copyProduct_copyOptions :: Lens.Lens' CopyProduct (Core.Maybe [CopyOption])
copyProduct_copyOptions = Lens.lens (\CopyProduct' {copyOptions} -> copyOptions) (\s@CopyProduct' {} a -> s {copyOptions = a} :: CopyProduct) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the target product. By default, a new product is
-- created.
copyProduct_targetProductId :: Lens.Lens' CopyProduct (Core.Maybe Core.Text)
copyProduct_targetProductId = Lens.lens (\CopyProduct' {targetProductId} -> targetProductId) (\s@CopyProduct' {} a -> s {targetProductId = a} :: CopyProduct)

-- | The identifiers of the provisioning artifacts (also known as versions)
-- of the product to copy. By default, all provisioning artifacts are
-- copied.
copyProduct_sourceProvisioningArtifactIdentifiers :: Lens.Lens' CopyProduct (Core.Maybe [Core.HashMap ProvisioningArtifactPropertyName Core.Text])
copyProduct_sourceProvisioningArtifactIdentifiers = Lens.lens (\CopyProduct' {sourceProvisioningArtifactIdentifiers} -> sourceProvisioningArtifactIdentifiers) (\s@CopyProduct' {} a -> s {sourceProvisioningArtifactIdentifiers = a} :: CopyProduct) Core.. Lens.mapping Lens._Coerce

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
copyProduct_acceptLanguage :: Lens.Lens' CopyProduct (Core.Maybe Core.Text)
copyProduct_acceptLanguage = Lens.lens (\CopyProduct' {acceptLanguage} -> acceptLanguage) (\s@CopyProduct' {} a -> s {acceptLanguage = a} :: CopyProduct)

-- | The Amazon Resource Name (ARN) of the source product.
copyProduct_sourceProductArn :: Lens.Lens' CopyProduct Core.Text
copyProduct_sourceProductArn = Lens.lens (\CopyProduct' {sourceProductArn} -> sourceProductArn) (\s@CopyProduct' {} a -> s {sourceProductArn = a} :: CopyProduct)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
copyProduct_idempotencyToken :: Lens.Lens' CopyProduct Core.Text
copyProduct_idempotencyToken = Lens.lens (\CopyProduct' {idempotencyToken} -> idempotencyToken) (\s@CopyProduct' {} a -> s {idempotencyToken = a} :: CopyProduct)

instance Core.AWSRequest CopyProduct where
  type AWSResponse CopyProduct = CopyProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyProductResponse'
            Core.<$> (x Core..?> "CopyProductToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CopyProduct

instance Core.NFData CopyProduct

instance Core.ToHeaders CopyProduct where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CopyProduct" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CopyProduct where
  toJSON CopyProduct' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TargetProductName" Core..=)
              Core.<$> targetProductName,
            ("CopyOptions" Core..=) Core.<$> copyOptions,
            ("TargetProductId" Core..=) Core.<$> targetProductId,
            ("SourceProvisioningArtifactIdentifiers" Core..=)
              Core.<$> sourceProvisioningArtifactIdentifiers,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just
              ("SourceProductArn" Core..= sourceProductArn),
            Core.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CopyProduct where
  toPath = Core.const "/"

instance Core.ToQuery CopyProduct where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCopyProductResponse' smart constructor.
data CopyProductResponse = CopyProductResponse'
  { -- | The token to use to track the progress of the operation.
    copyProductToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyProductToken', 'copyProductResponse_copyProductToken' - The token to use to track the progress of the operation.
--
-- 'httpStatus', 'copyProductResponse_httpStatus' - The response's http status code.
newCopyProductResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CopyProductResponse
newCopyProductResponse pHttpStatus_ =
  CopyProductResponse'
    { copyProductToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to track the progress of the operation.
copyProductResponse_copyProductToken :: Lens.Lens' CopyProductResponse (Core.Maybe Core.Text)
copyProductResponse_copyProductToken = Lens.lens (\CopyProductResponse' {copyProductToken} -> copyProductToken) (\s@CopyProductResponse' {} a -> s {copyProductToken = a} :: CopyProductResponse)

-- | The response's http status code.
copyProductResponse_httpStatus :: Lens.Lens' CopyProductResponse Core.Int
copyProductResponse_httpStatus = Lens.lens (\CopyProductResponse' {httpStatus} -> httpStatus) (\s@CopyProductResponse' {} a -> s {httpStatus = a} :: CopyProductResponse)

instance Core.NFData CopyProductResponse
