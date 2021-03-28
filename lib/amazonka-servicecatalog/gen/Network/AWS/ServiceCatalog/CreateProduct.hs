{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a product.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.CreateProduct
    (
    -- * Creating a request
      CreateProduct (..)
    , mkCreateProduct
    -- ** Request lenses
    , cpName
    , cpOwner
    , cpProductType
    , cpProvisioningArtifactParameters
    , cpIdempotencyToken
    , cpAcceptLanguage
    , cpDescription
    , cpDistributor
    , cpSupportDescription
    , cpSupportEmail
    , cpSupportUrl
    , cpTags

    -- * Destructuring the response
    , CreateProductResponse (..)
    , mkCreateProductResponse
    -- ** Response lenses
    , cprrsProductViewDetail
    , cprrsProvisioningArtifactDetail
    , cprrsTags
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreateProduct' smart constructor.
data CreateProduct = CreateProduct'
  { name :: Types.Name
    -- ^ The name of the product.
  , owner :: Types.ProductViewOwner
    -- ^ The owner of the product.
  , productType :: Types.ProductType
    -- ^ The type of product.
  , provisioningArtifactParameters :: Types.ProvisioningArtifactProperties
    -- ^ The configuration of the provisioning artifact. The @info@ field accepts @ImportFromPhysicalID@ .
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
  , description :: Core.Maybe Types.Description
    -- ^ The description of the product.
  , distributor :: Core.Maybe Types.ProductViewOwner
    -- ^ The distributor of the product.
  , supportDescription :: Core.Maybe Types.SupportDescription
    -- ^ The support information about the product.
  , supportEmail :: Core.Maybe Types.SupportEmail
    -- ^ The contact email for product support.
  , supportUrl :: Core.Maybe Types.SupportUrl
    -- ^ The contact URL for product support.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProduct' value with any optional fields omitted.
mkCreateProduct
    :: Types.Name -- ^ 'name'
    -> Types.ProductViewOwner -- ^ 'owner'
    -> Types.ProductType -- ^ 'productType'
    -> Types.ProvisioningArtifactProperties -- ^ 'provisioningArtifactParameters'
    -> Types.IdempotencyToken -- ^ 'idempotencyToken'
    -> CreateProduct
mkCreateProduct name owner productType
  provisioningArtifactParameters idempotencyToken
  = CreateProduct'{name, owner, productType,
                   provisioningArtifactParameters, idempotencyToken,
                   acceptLanguage = Core.Nothing, description = Core.Nothing,
                   distributor = Core.Nothing, supportDescription = Core.Nothing,
                   supportEmail = Core.Nothing, supportUrl = Core.Nothing,
                   tags = Core.Nothing}

-- | The name of the product.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProduct Types.Name
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The owner of the product.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOwner :: Lens.Lens' CreateProduct Types.ProductViewOwner
cpOwner = Lens.field @"owner"
{-# INLINEABLE cpOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The type of product.
--
-- /Note:/ Consider using 'productType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProductType :: Lens.Lens' CreateProduct Types.ProductType
cpProductType = Lens.field @"productType"
{-# INLINEABLE cpProductType #-}
{-# DEPRECATED productType "Use generic-lens or generic-optics with 'productType' instead"  #-}

-- | The configuration of the provisioning artifact. The @info@ field accepts @ImportFromPhysicalID@ .
--
-- /Note:/ Consider using 'provisioningArtifactParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProvisioningArtifactParameters :: Lens.Lens' CreateProduct Types.ProvisioningArtifactProperties
cpProvisioningArtifactParameters = Lens.field @"provisioningArtifactParameters"
{-# INLINEABLE cpProvisioningArtifactParameters #-}
{-# DEPRECATED provisioningArtifactParameters "Use generic-lens or generic-optics with 'provisioningArtifactParameters' instead"  #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpIdempotencyToken :: Lens.Lens' CreateProduct Types.IdempotencyToken
cpIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE cpIdempotencyToken #-}
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
cpAcceptLanguage :: Lens.Lens' CreateProduct (Core.Maybe Types.AcceptLanguage)
cpAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE cpAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The description of the product.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreateProduct (Core.Maybe Types.Description)
cpDescription = Lens.field @"description"
{-# INLINEABLE cpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The distributor of the product.
--
-- /Note:/ Consider using 'distributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDistributor :: Lens.Lens' CreateProduct (Core.Maybe Types.ProductViewOwner)
cpDistributor = Lens.field @"distributor"
{-# INLINEABLE cpDistributor #-}
{-# DEPRECATED distributor "Use generic-lens or generic-optics with 'distributor' instead"  #-}

-- | The support information about the product.
--
-- /Note:/ Consider using 'supportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSupportDescription :: Lens.Lens' CreateProduct (Core.Maybe Types.SupportDescription)
cpSupportDescription = Lens.field @"supportDescription"
{-# INLINEABLE cpSupportDescription #-}
{-# DEPRECATED supportDescription "Use generic-lens or generic-optics with 'supportDescription' instead"  #-}

-- | The contact email for product support.
--
-- /Note:/ Consider using 'supportEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSupportEmail :: Lens.Lens' CreateProduct (Core.Maybe Types.SupportEmail)
cpSupportEmail = Lens.field @"supportEmail"
{-# INLINEABLE cpSupportEmail #-}
{-# DEPRECATED supportEmail "Use generic-lens or generic-optics with 'supportEmail' instead"  #-}

-- | The contact URL for product support.
--
-- /Note:/ Consider using 'supportUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSupportUrl :: Lens.Lens' CreateProduct (Core.Maybe Types.SupportUrl)
cpSupportUrl = Lens.field @"supportUrl"
{-# INLINEABLE cpSupportUrl #-}
{-# DEPRECATED supportUrl "Use generic-lens or generic-optics with 'supportUrl' instead"  #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreateProduct (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# INLINEABLE cpTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateProduct where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProduct where
        toHeaders CreateProduct{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.CreateProduct")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProduct where
        toJSON CreateProduct{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Owner" Core..= owner),
                  Core.Just ("ProductType" Core..= productType),
                  Core.Just
                    ("ProvisioningArtifactParameters" Core..=
                       provisioningArtifactParameters),
                  Core.Just ("IdempotencyToken" Core..= idempotencyToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("Description" Core..=) Core.<$> description,
                  ("Distributor" Core..=) Core.<$> distributor,
                  ("SupportDescription" Core..=) Core.<$> supportDescription,
                  ("SupportEmail" Core..=) Core.<$> supportEmail,
                  ("SupportUrl" Core..=) Core.<$> supportUrl,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateProduct where
        type Rs CreateProduct = CreateProductResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProductResponse' Core.<$>
                   (x Core..:? "ProductViewDetail") Core.<*>
                     x Core..:? "ProvisioningArtifactDetail"
                     Core.<*> x Core..:? "Tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProductResponse' smart constructor.
data CreateProductResponse = CreateProductResponse'
  { productViewDetail :: Core.Maybe Types.ProductViewDetail
    -- ^ Information about the product view.
  , provisioningArtifactDetail :: Core.Maybe Types.ProvisioningArtifactDetail
    -- ^ Information about the provisioning artifact. 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Information about the tags associated with the product.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateProductResponse' value with any optional fields omitted.
mkCreateProductResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProductResponse
mkCreateProductResponse responseStatus
  = CreateProductResponse'{productViewDetail = Core.Nothing,
                           provisioningArtifactDetail = Core.Nothing, tags = Core.Nothing,
                           responseStatus}

-- | Information about the product view.
--
-- /Note:/ Consider using 'productViewDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProductViewDetail :: Lens.Lens' CreateProductResponse (Core.Maybe Types.ProductViewDetail)
cprrsProductViewDetail = Lens.field @"productViewDetail"
{-# INLINEABLE cprrsProductViewDetail #-}
{-# DEPRECATED productViewDetail "Use generic-lens or generic-optics with 'productViewDetail' instead"  #-}

-- | Information about the provisioning artifact. 
--
-- /Note:/ Consider using 'provisioningArtifactDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProvisioningArtifactDetail :: Lens.Lens' CreateProductResponse (Core.Maybe Types.ProvisioningArtifactDetail)
cprrsProvisioningArtifactDetail = Lens.field @"provisioningArtifactDetail"
{-# INLINEABLE cprrsProvisioningArtifactDetail #-}
{-# DEPRECATED provisioningArtifactDetail "Use generic-lens or generic-optics with 'provisioningArtifactDetail' instead"  #-}

-- | Information about the tags associated with the product.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsTags :: Lens.Lens' CreateProductResponse (Core.Maybe [Types.Tag])
cprrsTags = Lens.field @"tags"
{-# INLINEABLE cprrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProductResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
