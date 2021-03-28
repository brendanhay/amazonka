{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CopyProduct (..)
    , mkCopyProduct
    -- ** Request lenses
    , cSourceProductArn
    , cIdempotencyToken
    , cAcceptLanguage
    , cCopyOptions
    , cSourceProvisioningArtifactIdentifiers
    , cTargetProductId
    , cTargetProductName

    -- * Destructuring the response
    , CopyProductResponse (..)
    , mkCopyProductResponse
    -- ** Response lenses
    , crsCopyProductToken
    , crsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCopyProduct' smart constructor.
data CopyProduct = CopyProduct'
  { sourceProductArn :: Types.SourceProductArn
    -- ^ The Amazon Resource Name (ARN) of the source product.
  , idempotencyToken :: Types.IdempotencyToken
    -- ^ A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request. 
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
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
  , copyOptions :: Core.Maybe [Types.CopyOption]
    -- ^ The copy options. If the value is @CopyTags@ , the tags from the source product are copied to the target product.
  , sourceProvisioningArtifactIdentifiers :: Core.Maybe [Core.HashMap Types.ProvisioningArtifactPropertyName Types.ProvisioningArtifactPropertyValue]
    -- ^ The identifiers of the provisioning artifacts (also known as versions) of the product to copy. By default, all provisioning artifacts are copied.
  , targetProductId :: Core.Maybe Types.Id
    -- ^ The identifier of the target product. By default, a new product is created.
  , targetProductName :: Core.Maybe Types.TargetProductName
    -- ^ A name for the target product. The default is the name of the source product.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyProduct' value with any optional fields omitted.
mkCopyProduct
    :: Types.SourceProductArn -- ^ 'sourceProductArn'
    -> Types.IdempotencyToken -- ^ 'idempotencyToken'
    -> CopyProduct
mkCopyProduct sourceProductArn idempotencyToken
  = CopyProduct'{sourceProductArn, idempotencyToken,
                 acceptLanguage = Core.Nothing, copyOptions = Core.Nothing,
                 sourceProvisioningArtifactIdentifiers = Core.Nothing,
                 targetProductId = Core.Nothing, targetProductName = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the source product.
--
-- /Note:/ Consider using 'sourceProductArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceProductArn :: Lens.Lens' CopyProduct Types.SourceProductArn
cSourceProductArn = Lens.field @"sourceProductArn"
{-# INLINEABLE cSourceProductArn #-}
{-# DEPRECATED sourceProductArn "Use generic-lens or generic-optics with 'sourceProductArn' instead"  #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request. 
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIdempotencyToken :: Lens.Lens' CopyProduct Types.IdempotencyToken
cIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE cIdempotencyToken #-}
{-# DEPRECATED idempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead"  #-}

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
{-# INLINEABLE cAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The copy options. If the value is @CopyTags@ , the tags from the source product are copied to the target product.
--
-- /Note:/ Consider using 'copyOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCopyOptions :: Lens.Lens' CopyProduct (Core.Maybe [Types.CopyOption])
cCopyOptions = Lens.field @"copyOptions"
{-# INLINEABLE cCopyOptions #-}
{-# DEPRECATED copyOptions "Use generic-lens or generic-optics with 'copyOptions' instead"  #-}

-- | The identifiers of the provisioning artifacts (also known as versions) of the product to copy. By default, all provisioning artifacts are copied.
--
-- /Note:/ Consider using 'sourceProvisioningArtifactIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceProvisioningArtifactIdentifiers :: Lens.Lens' CopyProduct (Core.Maybe [Core.HashMap Types.ProvisioningArtifactPropertyName Types.ProvisioningArtifactPropertyValue])
cSourceProvisioningArtifactIdentifiers = Lens.field @"sourceProvisioningArtifactIdentifiers"
{-# INLINEABLE cSourceProvisioningArtifactIdentifiers #-}
{-# DEPRECATED sourceProvisioningArtifactIdentifiers "Use generic-lens or generic-optics with 'sourceProvisioningArtifactIdentifiers' instead"  #-}

-- | The identifier of the target product. By default, a new product is created.
--
-- /Note:/ Consider using 'targetProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetProductId :: Lens.Lens' CopyProduct (Core.Maybe Types.Id)
cTargetProductId = Lens.field @"targetProductId"
{-# INLINEABLE cTargetProductId #-}
{-# DEPRECATED targetProductId "Use generic-lens or generic-optics with 'targetProductId' instead"  #-}

-- | A name for the target product. The default is the name of the source product.
--
-- /Note:/ Consider using 'targetProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetProductName :: Lens.Lens' CopyProduct (Core.Maybe Types.TargetProductName)
cTargetProductName = Lens.field @"targetProductName"
{-# INLINEABLE cTargetProductName #-}
{-# DEPRECATED targetProductName "Use generic-lens or generic-optics with 'targetProductName' instead"  #-}

instance Core.ToQuery CopyProduct where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CopyProduct where
        toHeaders CopyProduct{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.CopyProduct")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CopyProduct where
        toJSON CopyProduct{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SourceProductArn" Core..= sourceProductArn),
                  Core.Just ("IdempotencyToken" Core..= idempotencyToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("CopyOptions" Core..=) Core.<$> copyOptions,
                  ("SourceProvisioningArtifactIdentifiers" Core..=) Core.<$>
                    sourceProvisioningArtifactIdentifiers,
                  ("TargetProductId" Core..=) Core.<$> targetProductId,
                  ("TargetProductName" Core..=) Core.<$> targetProductName])

instance Core.AWSRequest CopyProduct where
        type Rs CopyProduct = CopyProductResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CopyProductResponse' Core.<$>
                   (x Core..:? "CopyProductToken") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCopyProductResponse' smart constructor.
data CopyProductResponse = CopyProductResponse'
  { copyProductToken :: Core.Maybe Types.Id
    -- ^ The token to use to track the progress of the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyProductResponse' value with any optional fields omitted.
mkCopyProductResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CopyProductResponse
mkCopyProductResponse responseStatus
  = CopyProductResponse'{copyProductToken = Core.Nothing,
                         responseStatus}

-- | The token to use to track the progress of the operation.
--
-- /Note:/ Consider using 'copyProductToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCopyProductToken :: Lens.Lens' CopyProductResponse (Core.Maybe Types.Id)
crsCopyProductToken = Lens.field @"copyProductToken"
{-# INLINEABLE crsCopyProductToken #-}
{-# DEPRECATED copyProductToken "Use generic-lens or generic-optics with 'copyProductToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CopyProductResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
