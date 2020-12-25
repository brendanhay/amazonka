{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CopyProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified source product to the specified target product or a new product.
--
-- You can copy a product to the same account or another account. You can copy a product to the same region or another region.
-- This operation is performed asynchronously. To track the progress of the operation, use 'DescribeCopyProductStatus' .
module Network.AWS.ServiceCatalog.CopyProduct
  ( -- * Creating a request
    CopyProduct (..),
    mkCopyProduct,

    -- ** Request lenses
    cSourceProductArn,
    cIdempotencyToken,
    cAcceptLanguage,
    cCopyOptions,
    cSourceProvisioningArtifactIdentifiers,
    cTargetProductId,
    cTargetProductName,

    -- * Destructuring the response
    CopyProductResponse (..),
    mkCopyProductResponse,

    -- ** Response lenses
    crsCopyProductToken,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCopyProduct' smart constructor.
data CopyProduct = CopyProduct'
  { -- | The Amazon Resource Name (ARN) of the source product.
    sourceProductArn :: Types.SourceProductArn,
    -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Types.IdempotencyToken,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | The copy options. If the value is @CopyTags@ , the tags from the source product are copied to the target product.
    copyOptions :: Core.Maybe [Types.CopyOption],
    -- | The identifiers of the provisioning artifacts (also known as versions) of the product to copy. By default, all provisioning artifacts are copied.
    sourceProvisioningArtifactIdentifiers :: Core.Maybe [Core.HashMap Types.ProvisioningArtifactPropertyName Types.ProvisioningArtifactPropertyValue],
    -- | The identifier of the target product. By default, a new product is created.
    targetProductId :: Core.Maybe Types.Id,
    -- | A name for the target product. The default is the name of the source product.
    targetProductName :: Core.Maybe Types.TargetProductName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyProduct' value with any optional fields omitted.
mkCopyProduct ::
  -- | 'sourceProductArn'
  Types.SourceProductArn ->
  -- | 'idempotencyToken'
  Types.IdempotencyToken ->
  CopyProduct
mkCopyProduct sourceProductArn idempotencyToken =
  CopyProduct'
    { sourceProductArn,
      idempotencyToken,
      acceptLanguage = Core.Nothing,
      copyOptions = Core.Nothing,
      sourceProvisioningArtifactIdentifiers = Core.Nothing,
      targetProductId = Core.Nothing,
      targetProductName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the source product.
--
-- /Note:/ Consider using 'sourceProductArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceProductArn :: Lens.Lens' CopyProduct Types.SourceProductArn
cSourceProductArn = Lens.field @"sourceProductArn"
{-# DEPRECATED cSourceProductArn "Use generic-lens or generic-optics with 'sourceProductArn' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIdempotencyToken :: Lens.Lens' CopyProduct Types.IdempotencyToken
cIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED cIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAcceptLanguage :: Lens.Lens' CopyProduct (Core.Maybe Types.AcceptLanguage)
cAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED cAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The copy options. If the value is @CopyTags@ , the tags from the source product are copied to the target product.
--
-- /Note:/ Consider using 'copyOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCopyOptions :: Lens.Lens' CopyProduct (Core.Maybe [Types.CopyOption])
cCopyOptions = Lens.field @"copyOptions"
{-# DEPRECATED cCopyOptions "Use generic-lens or generic-optics with 'copyOptions' instead." #-}

-- | The identifiers of the provisioning artifacts (also known as versions) of the product to copy. By default, all provisioning artifacts are copied.
--
-- /Note:/ Consider using 'sourceProvisioningArtifactIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceProvisioningArtifactIdentifiers :: Lens.Lens' CopyProduct (Core.Maybe [Core.HashMap Types.ProvisioningArtifactPropertyName Types.ProvisioningArtifactPropertyValue])
cSourceProvisioningArtifactIdentifiers = Lens.field @"sourceProvisioningArtifactIdentifiers"
{-# DEPRECATED cSourceProvisioningArtifactIdentifiers "Use generic-lens or generic-optics with 'sourceProvisioningArtifactIdentifiers' instead." #-}

-- | The identifier of the target product. By default, a new product is created.
--
-- /Note:/ Consider using 'targetProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetProductId :: Lens.Lens' CopyProduct (Core.Maybe Types.Id)
cTargetProductId = Lens.field @"targetProductId"
{-# DEPRECATED cTargetProductId "Use generic-lens or generic-optics with 'targetProductId' instead." #-}

-- | A name for the target product. The default is the name of the source product.
--
-- /Note:/ Consider using 'targetProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetProductName :: Lens.Lens' CopyProduct (Core.Maybe Types.TargetProductName)
cTargetProductName = Lens.field @"targetProductName"
{-# DEPRECATED cTargetProductName "Use generic-lens or generic-optics with 'targetProductName' instead." #-}

instance Core.FromJSON CopyProduct where
  toJSON CopyProduct {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceProductArn" Core..= sourceProductArn),
            Core.Just ("IdempotencyToken" Core..= idempotencyToken),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("CopyOptions" Core..=) Core.<$> copyOptions,
            ("SourceProvisioningArtifactIdentifiers" Core..=)
              Core.<$> sourceProvisioningArtifactIdentifiers,
            ("TargetProductId" Core..=) Core.<$> targetProductId,
            ("TargetProductName" Core..=) Core.<$> targetProductName
          ]
      )

instance Core.AWSRequest CopyProduct where
  type Rs CopyProduct = CopyProductResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.CopyProduct")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CopyProductResponse'
            Core.<$> (x Core..:? "CopyProductToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyProductResponse' smart constructor.
data CopyProductResponse = CopyProductResponse'
  { -- | The token to use to track the progress of the operation.
    copyProductToken :: Core.Maybe Types.Id,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyProductResponse' value with any optional fields omitted.
mkCopyProductResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyProductResponse
mkCopyProductResponse responseStatus =
  CopyProductResponse'
    { copyProductToken = Core.Nothing,
      responseStatus
    }

-- | The token to use to track the progress of the operation.
--
-- /Note:/ Consider using 'copyProductToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCopyProductToken :: Lens.Lens' CopyProductResponse (Core.Maybe Types.Id)
crsCopyProductToken = Lens.field @"copyProductToken"
{-# DEPRECATED crsCopyProductToken "Use generic-lens or generic-optics with 'copyProductToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CopyProductResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
