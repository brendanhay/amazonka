{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests updates to the configuration of the specified provisioned product.
--
-- If there are tags associated with the object, they cannot be updated or added. Depending on the specific updates requested, this operation can update with no interruption, with some interruption, or replace the provisioned product entirely.
-- You can check the status of this request using 'DescribeRecord' .
module Network.AWS.ServiceCatalog.UpdateProvisionedProduct
    (
    -- * Creating a request
      UpdateProvisionedProduct (..)
    , mkUpdateProvisionedProduct
    -- ** Request lenses
    , uppUpdateToken
    , uppAcceptLanguage
    , uppPathId
    , uppPathName
    , uppProductId
    , uppProductName
    , uppProvisionedProductId
    , uppProvisionedProductName
    , uppProvisioningArtifactId
    , uppProvisioningArtifactName
    , uppProvisioningParameters
    , uppProvisioningPreferences
    , uppTags

    -- * Destructuring the response
    , UpdateProvisionedProductResponse (..)
    , mkUpdateProvisionedProductResponse
    -- ** Response lenses
    , upprrsRecordDetail
    , upprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkUpdateProvisionedProduct' smart constructor.
data UpdateProvisionedProduct = UpdateProvisionedProduct'
  { updateToken :: Types.IdempotencyToken
    -- ^ The idempotency token that uniquely identifies the provisioning update request.
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
  , pathId :: Core.Maybe Types.PathId
    -- ^ The path identifier. This value is optional if the product has a default path, and required if the product has more than one path. You must provide the name or ID, but not both.
  , pathName :: Core.Maybe Types.PortfolioDisplayName
    -- ^ The name of the path. You must provide the name or ID, but not both.
  , productId :: Core.Maybe Types.ProductId
    -- ^ The identifier of the product. You must provide the name or ID, but not both.
  , productName :: Core.Maybe Types.ProductName
    -- ^ The name of the product. You must provide the name or ID, but not both.
  , provisionedProductId :: Core.Maybe Types.ProvisionedProductId
    -- ^ The identifier of the provisioned product. You must provide the name or ID, but not both.
  , provisionedProductName :: Core.Maybe Types.ProvisionedProductNameOrArn
    -- ^ The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
  , provisioningArtifactId :: Core.Maybe Types.ProvisioningArtifactId
    -- ^ The identifier of the provisioning artifact.
  , provisioningArtifactName :: Core.Maybe Types.ProvisioningArtifactName
    -- ^ The name of the provisioning artifact. You must provide the name or ID, but not both.
  , provisioningParameters :: Core.Maybe [Types.UpdateProvisioningParameter]
    -- ^ The new parameters.
  , provisioningPreferences :: Core.Maybe Types.UpdateProvisioningPreferences
    -- ^ An object that contains information about the provisioning preferences for a stack set.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags. Requires the product to have @RESOURCE_UPDATE@ constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProvisionedProduct' value with any optional fields omitted.
mkUpdateProvisionedProduct
    :: Types.IdempotencyToken -- ^ 'updateToken'
    -> UpdateProvisionedProduct
mkUpdateProvisionedProduct updateToken
  = UpdateProvisionedProduct'{updateToken,
                              acceptLanguage = Core.Nothing, pathId = Core.Nothing,
                              pathName = Core.Nothing, productId = Core.Nothing,
                              productName = Core.Nothing, provisionedProductId = Core.Nothing,
                              provisionedProductName = Core.Nothing,
                              provisioningArtifactId = Core.Nothing,
                              provisioningArtifactName = Core.Nothing,
                              provisioningParameters = Core.Nothing,
                              provisioningPreferences = Core.Nothing, tags = Core.Nothing}

-- | The idempotency token that uniquely identifies the provisioning update request.
--
-- /Note:/ Consider using 'updateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppUpdateToken :: Lens.Lens' UpdateProvisionedProduct Types.IdempotencyToken
uppUpdateToken = Lens.field @"updateToken"
{-# INLINEABLE uppUpdateToken #-}
{-# DEPRECATED updateToken "Use generic-lens or generic-optics with 'updateToken' instead"  #-}

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
uppAcceptLanguage :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.AcceptLanguage)
uppAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE uppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The path identifier. This value is optional if the product has a default path, and required if the product has more than one path. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppPathId :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.PathId)
uppPathId = Lens.field @"pathId"
{-# INLINEABLE uppPathId #-}
{-# DEPRECATED pathId "Use generic-lens or generic-optics with 'pathId' instead"  #-}

-- | The name of the path. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppPathName :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.PortfolioDisplayName)
uppPathName = Lens.field @"pathName"
{-# INLINEABLE uppPathName #-}
{-# DEPRECATED pathName "Use generic-lens or generic-optics with 'pathName' instead"  #-}

-- | The identifier of the product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProductId :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.ProductId)
uppProductId = Lens.field @"productId"
{-# INLINEABLE uppProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The name of the product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProductName :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.ProductName)
uppProductName = Lens.field @"productName"
{-# INLINEABLE uppProductName #-}
{-# DEPRECATED productName "Use generic-lens or generic-optics with 'productName' instead"  #-}

-- | The identifier of the provisioned product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisionedProductId :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.ProvisionedProductId)
uppProvisionedProductId = Lens.field @"provisionedProductId"
{-# INLINEABLE uppProvisionedProductId #-}
{-# DEPRECATED provisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead"  #-}

-- | The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisionedProductName :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.ProvisionedProductNameOrArn)
uppProvisionedProductName = Lens.field @"provisionedProductName"
{-# INLINEABLE uppProvisionedProductName #-}
{-# DEPRECATED provisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisioningArtifactId :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.ProvisioningArtifactId)
uppProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE uppProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | The name of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisioningArtifactName :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.ProvisioningArtifactName)
uppProvisioningArtifactName = Lens.field @"provisioningArtifactName"
{-# INLINEABLE uppProvisioningArtifactName #-}
{-# DEPRECATED provisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead"  #-}

-- | The new parameters.
--
-- /Note:/ Consider using 'provisioningParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisioningParameters :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe [Types.UpdateProvisioningParameter])
uppProvisioningParameters = Lens.field @"provisioningParameters"
{-# INLINEABLE uppProvisioningParameters #-}
{-# DEPRECATED provisioningParameters "Use generic-lens or generic-optics with 'provisioningParameters' instead"  #-}

-- | An object that contains information about the provisioning preferences for a stack set.
--
-- /Note:/ Consider using 'provisioningPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisioningPreferences :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe Types.UpdateProvisioningPreferences)
uppProvisioningPreferences = Lens.field @"provisioningPreferences"
{-# INLINEABLE uppProvisioningPreferences #-}
{-# DEPRECATED provisioningPreferences "Use generic-lens or generic-optics with 'provisioningPreferences' instead"  #-}

-- | One or more tags. Requires the product to have @RESOURCE_UPDATE@ constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppTags :: Lens.Lens' UpdateProvisionedProduct (Core.Maybe [Types.Tag])
uppTags = Lens.field @"tags"
{-# INLINEABLE uppTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery UpdateProvisionedProduct where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateProvisionedProduct where
        toHeaders UpdateProvisionedProduct{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.UpdateProvisionedProduct")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateProvisionedProduct where
        toJSON UpdateProvisionedProduct{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UpdateToken" Core..= updateToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PathId" Core..=) Core.<$> pathId,
                  ("PathName" Core..=) Core.<$> pathName,
                  ("ProductId" Core..=) Core.<$> productId,
                  ("ProductName" Core..=) Core.<$> productName,
                  ("ProvisionedProductId" Core..=) Core.<$> provisionedProductId,
                  ("ProvisionedProductName" Core..=) Core.<$> provisionedProductName,
                  ("ProvisioningArtifactId" Core..=) Core.<$> provisioningArtifactId,
                  ("ProvisioningArtifactName" Core..=) Core.<$>
                    provisioningArtifactName,
                  ("ProvisioningParameters" Core..=) Core.<$> provisioningParameters,
                  ("ProvisioningPreferences" Core..=) Core.<$>
                    provisioningPreferences,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest UpdateProvisionedProduct where
        type Rs UpdateProvisionedProduct = UpdateProvisionedProductResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateProvisionedProductResponse' Core.<$>
                   (x Core..:? "RecordDetail") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateProvisionedProductResponse' smart constructor.
data UpdateProvisionedProductResponse = UpdateProvisionedProductResponse'
  { recordDetail :: Core.Maybe Types.RecordDetail
    -- ^ Information about the result of the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateProvisionedProductResponse' value with any optional fields omitted.
mkUpdateProvisionedProductResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateProvisionedProductResponse
mkUpdateProvisionedProductResponse responseStatus
  = UpdateProvisionedProductResponse'{recordDetail = Core.Nothing,
                                      responseStatus}

-- | Information about the result of the request.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upprrsRecordDetail :: Lens.Lens' UpdateProvisionedProductResponse (Core.Maybe Types.RecordDetail)
upprrsRecordDetail = Lens.field @"recordDetail"
{-# INLINEABLE upprrsRecordDetail #-}
{-# DEPRECATED recordDetail "Use generic-lens or generic-optics with 'recordDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upprrsResponseStatus :: Lens.Lens' UpdateProvisionedProductResponse Core.Int
upprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE upprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
