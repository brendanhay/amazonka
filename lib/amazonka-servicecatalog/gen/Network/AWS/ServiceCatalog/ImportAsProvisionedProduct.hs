{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ImportAsProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the import of a resource as a Service Catalog provisioned product that is associated to a Service Catalog product and provisioning artifact. Once imported all supported Service Catalog governance actions are supported on the provisioned product.
--
-- Resource import only supports CloudFormation stack ARNs. CloudFormation StackSets and non-root nested stacks are not supported.
-- The CloudFormation stack must have one of the following statuses to be imported: CREATE_COMPLETE, UPDATE_COMPLETE, UPDATE_ROLLBACK_COMPLETE, IMPORT_COMPLETE, IMPORT_ROLLBACK_COMPLETE.
-- Import of the resource requires that the CloudFormation stack template matches the associated Service Catalog product provisioning artifact. 
module Network.AWS.ServiceCatalog.ImportAsProvisionedProduct
    (
    -- * Creating a request
      ImportAsProvisionedProduct (..)
    , mkImportAsProvisionedProduct
    -- ** Request lenses
    , iappProductId
    , iappProvisioningArtifactId
    , iappProvisionedProductName
    , iappPhysicalId
    , iappIdempotencyToken
    , iappAcceptLanguage

    -- * Destructuring the response
    , ImportAsProvisionedProductResponse (..)
    , mkImportAsProvisionedProductResponse
    -- ** Response lenses
    , iapprrsRecordDetail
    , iapprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkImportAsProvisionedProduct' smart constructor.
data ImportAsProvisionedProduct = ImportAsProvisionedProduct'
  { productId :: Types.ProductId
    -- ^ The product identifier.
  , provisioningArtifactId :: Types.ProvisioningArtifactId
    -- ^ The identifier of the provisioning artifact.
  , provisionedProductName :: Types.ProvisionedProductName
    -- ^ The user-friendly name of the provisioned product. The value must be unique for the AWS account. The name cannot be updated after the product is provisioned. 
  , physicalId :: Types.PhysicalId
    -- ^ The unique identifier of the resource to be imported. It only currently supports CloudFormation stack IDs.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportAsProvisionedProduct' value with any optional fields omitted.
mkImportAsProvisionedProduct
    :: Types.ProductId -- ^ 'productId'
    -> Types.ProvisioningArtifactId -- ^ 'provisioningArtifactId'
    -> Types.ProvisionedProductName -- ^ 'provisionedProductName'
    -> Types.PhysicalId -- ^ 'physicalId'
    -> Types.IdempotencyToken -- ^ 'idempotencyToken'
    -> ImportAsProvisionedProduct
mkImportAsProvisionedProduct productId provisioningArtifactId
  provisionedProductName physicalId idempotencyToken
  = ImportAsProvisionedProduct'{productId, provisioningArtifactId,
                                provisionedProductName, physicalId, idempotencyToken,
                                acceptLanguage = Core.Nothing}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappProductId :: Lens.Lens' ImportAsProvisionedProduct Types.ProductId
iappProductId = Lens.field @"productId"
{-# INLINEABLE iappProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappProvisioningArtifactId :: Lens.Lens' ImportAsProvisionedProduct Types.ProvisioningArtifactId
iappProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE iappProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | The user-friendly name of the provisioned product. The value must be unique for the AWS account. The name cannot be updated after the product is provisioned. 
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappProvisionedProductName :: Lens.Lens' ImportAsProvisionedProduct Types.ProvisionedProductName
iappProvisionedProductName = Lens.field @"provisionedProductName"
{-# INLINEABLE iappProvisionedProductName #-}
{-# DEPRECATED provisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead"  #-}

-- | The unique identifier of the resource to be imported. It only currently supports CloudFormation stack IDs.
--
-- /Note:/ Consider using 'physicalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappPhysicalId :: Lens.Lens' ImportAsProvisionedProduct Types.PhysicalId
iappPhysicalId = Lens.field @"physicalId"
{-# INLINEABLE iappPhysicalId #-}
{-# DEPRECATED physicalId "Use generic-lens or generic-optics with 'physicalId' instead"  #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappIdempotencyToken :: Lens.Lens' ImportAsProvisionedProduct Types.IdempotencyToken
iappIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE iappIdempotencyToken #-}
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
iappAcceptLanguage :: Lens.Lens' ImportAsProvisionedProduct (Core.Maybe Types.AcceptLanguage)
iappAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE iappAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery ImportAsProvisionedProduct where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportAsProvisionedProduct where
        toHeaders ImportAsProvisionedProduct{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ImportAsProvisionedProduct")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportAsProvisionedProduct where
        toJSON ImportAsProvisionedProduct{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  Core.Just
                    ("ProvisioningArtifactId" Core..= provisioningArtifactId),
                  Core.Just
                    ("ProvisionedProductName" Core..= provisionedProductName),
                  Core.Just ("PhysicalId" Core..= physicalId),
                  Core.Just ("IdempotencyToken" Core..= idempotencyToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest ImportAsProvisionedProduct where
        type Rs ImportAsProvisionedProduct =
             ImportAsProvisionedProductResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ImportAsProvisionedProductResponse' Core.<$>
                   (x Core..:? "RecordDetail") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportAsProvisionedProductResponse' smart constructor.
data ImportAsProvisionedProductResponse = ImportAsProvisionedProductResponse'
  { recordDetail :: Core.Maybe Types.RecordDetail
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImportAsProvisionedProductResponse' value with any optional fields omitted.
mkImportAsProvisionedProductResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportAsProvisionedProductResponse
mkImportAsProvisionedProductResponse responseStatus
  = ImportAsProvisionedProductResponse'{recordDetail = Core.Nothing,
                                        responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapprrsRecordDetail :: Lens.Lens' ImportAsProvisionedProductResponse (Core.Maybe Types.RecordDetail)
iapprrsRecordDetail = Lens.field @"recordDetail"
{-# INLINEABLE iapprrsRecordDetail #-}
{-# DEPRECATED recordDetail "Use generic-lens or generic-optics with 'recordDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapprrsResponseStatus :: Lens.Lens' ImportAsProvisionedProductResponse Core.Int
iapprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iapprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
