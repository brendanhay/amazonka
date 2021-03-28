{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.TerminateProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified provisioned product.
--
-- This operation does not delete any records associated with the provisioned product.
-- You can check the status of this request using 'DescribeRecord' .
module Network.AWS.ServiceCatalog.TerminateProvisionedProduct
    (
    -- * Creating a request
      TerminateProvisionedProduct (..)
    , mkTerminateProvisionedProduct
    -- ** Request lenses
    , tppTerminateToken
    , tppAcceptLanguage
    , tppIgnoreErrors
    , tppProvisionedProductId
    , tppProvisionedProductName
    , tppRetainPhysicalResources

    -- * Destructuring the response
    , TerminateProvisionedProductResponse (..)
    , mkTerminateProvisionedProductResponse
    -- ** Response lenses
    , tpprrsRecordDetail
    , tpprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkTerminateProvisionedProduct' smart constructor.
data TerminateProvisionedProduct = TerminateProvisionedProduct'
  { terminateToken :: Types.IdempotencyToken
    -- ^ An idempotency token that uniquely identifies the termination request. This token is only valid during the termination process. After the provisioned product is terminated, subsequent requests to terminate the same provisioned product always return __ResourceNotFound__ .
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
  , ignoreErrors :: Core.Maybe Core.Bool
    -- ^ If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
  , provisionedProductId :: Core.Maybe Types.ProvisionedProductId
    -- ^ The identifier of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
  , provisionedProductName :: Core.Maybe Types.ProvisionedProductNameOrArn
    -- ^ The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
  , retainPhysicalResources :: Core.Maybe Core.Bool
    -- ^ When this boolean parameter is set to true, the TerminateProvisionedProduct API deletes the Service Catalog provisioned product. However, it does not remove the CloudFormation stack, stack set, or the underlying resources of the deleted provisioned product. The default value is false.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateProvisionedProduct' value with any optional fields omitted.
mkTerminateProvisionedProduct
    :: Types.IdempotencyToken -- ^ 'terminateToken'
    -> TerminateProvisionedProduct
mkTerminateProvisionedProduct terminateToken
  = TerminateProvisionedProduct'{terminateToken,
                                 acceptLanguage = Core.Nothing, ignoreErrors = Core.Nothing,
                                 provisionedProductId = Core.Nothing,
                                 provisionedProductName = Core.Nothing,
                                 retainPhysicalResources = Core.Nothing}

-- | An idempotency token that uniquely identifies the termination request. This token is only valid during the termination process. After the provisioned product is terminated, subsequent requests to terminate the same provisioned product always return __ResourceNotFound__ .
--
-- /Note:/ Consider using 'terminateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppTerminateToken :: Lens.Lens' TerminateProvisionedProduct Types.IdempotencyToken
tppTerminateToken = Lens.field @"terminateToken"
{-# INLINEABLE tppTerminateToken #-}
{-# DEPRECATED terminateToken "Use generic-lens or generic-optics with 'terminateToken' instead"  #-}

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
tppAcceptLanguage :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Types.AcceptLanguage)
tppAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE tppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
--
-- /Note:/ Consider using 'ignoreErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppIgnoreErrors :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Core.Bool)
tppIgnoreErrors = Lens.field @"ignoreErrors"
{-# INLINEABLE tppIgnoreErrors #-}
{-# DEPRECATED ignoreErrors "Use generic-lens or generic-optics with 'ignoreErrors' instead"  #-}

-- | The identifier of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppProvisionedProductId :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Types.ProvisionedProductId)
tppProvisionedProductId = Lens.field @"provisionedProductId"
{-# INLINEABLE tppProvisionedProductId #-}
{-# DEPRECATED provisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead"  #-}

-- | The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppProvisionedProductName :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Types.ProvisionedProductNameOrArn)
tppProvisionedProductName = Lens.field @"provisionedProductName"
{-# INLINEABLE tppProvisionedProductName #-}
{-# DEPRECATED provisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead"  #-}

-- | When this boolean parameter is set to true, the TerminateProvisionedProduct API deletes the Service Catalog provisioned product. However, it does not remove the CloudFormation stack, stack set, or the underlying resources of the deleted provisioned product. The default value is false.
--
-- /Note:/ Consider using 'retainPhysicalResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppRetainPhysicalResources :: Lens.Lens' TerminateProvisionedProduct (Core.Maybe Core.Bool)
tppRetainPhysicalResources = Lens.field @"retainPhysicalResources"
{-# INLINEABLE tppRetainPhysicalResources #-}
{-# DEPRECATED retainPhysicalResources "Use generic-lens or generic-optics with 'retainPhysicalResources' instead"  #-}

instance Core.ToQuery TerminateProvisionedProduct where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TerminateProvisionedProduct where
        toHeaders TerminateProvisionedProduct{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.TerminateProvisionedProduct")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TerminateProvisionedProduct where
        toJSON TerminateProvisionedProduct{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TerminateToken" Core..= terminateToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("IgnoreErrors" Core..=) Core.<$> ignoreErrors,
                  ("ProvisionedProductId" Core..=) Core.<$> provisionedProductId,
                  ("ProvisionedProductName" Core..=) Core.<$> provisionedProductName,
                  ("RetainPhysicalResources" Core..=) Core.<$>
                    retainPhysicalResources])

instance Core.AWSRequest TerminateProvisionedProduct where
        type Rs TerminateProvisionedProduct =
             TerminateProvisionedProductResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TerminateProvisionedProductResponse' Core.<$>
                   (x Core..:? "RecordDetail") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTerminateProvisionedProductResponse' smart constructor.
data TerminateProvisionedProductResponse = TerminateProvisionedProductResponse'
  { recordDetail :: Core.Maybe Types.RecordDetail
    -- ^ Information about the result of this request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TerminateProvisionedProductResponse' value with any optional fields omitted.
mkTerminateProvisionedProductResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TerminateProvisionedProductResponse
mkTerminateProvisionedProductResponse responseStatus
  = TerminateProvisionedProductResponse'{recordDetail = Core.Nothing,
                                         responseStatus}

-- | Information about the result of this request.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpprrsRecordDetail :: Lens.Lens' TerminateProvisionedProductResponse (Core.Maybe Types.RecordDetail)
tpprrsRecordDetail = Lens.field @"recordDetail"
{-# INLINEABLE tpprrsRecordDetail #-}
{-# DEPRECATED recordDetail "Use generic-lens or generic-optics with 'recordDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpprrsResponseStatus :: Lens.Lens' TerminateProvisionedProductResponse Core.Int
tpprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tpprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
