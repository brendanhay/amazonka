{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning artifact (also known as a version) for the specified product.
--
-- You cannot create a provisioning artifact for a product that was shared with you.
module Network.AWS.ServiceCatalog.CreateProvisioningArtifact
    (
    -- * Creating a request
      CreateProvisioningArtifact (..)
    , mkCreateProvisioningArtifact
    -- ** Request lenses
    , cpaProductId
    , cpaParameters
    , cpaIdempotencyToken
    , cpaAcceptLanguage

    -- * Destructuring the response
    , CreateProvisioningArtifactResponse (..)
    , mkCreateProvisioningArtifactResponse
    -- ** Response lenses
    , cparrsInfo
    , cparrsProvisioningArtifactDetail
    , cparrsStatus
    , cparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreateProvisioningArtifact' smart constructor.
data CreateProvisioningArtifact = CreateProvisioningArtifact'
  { productId :: Types.ProductId
    -- ^ The product identifier.
  , parameters :: Types.ProvisioningArtifactProperties
    -- ^ The configuration for the provisioning artifact. The @info@ field accepts @ImportFromPhysicalID@ . 
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

-- | Creates a 'CreateProvisioningArtifact' value with any optional fields omitted.
mkCreateProvisioningArtifact
    :: Types.ProductId -- ^ 'productId'
    -> Types.ProvisioningArtifactProperties -- ^ 'parameters'
    -> Types.IdempotencyToken -- ^ 'idempotencyToken'
    -> CreateProvisioningArtifact
mkCreateProvisioningArtifact productId parameters idempotencyToken
  = CreateProvisioningArtifact'{productId, parameters,
                                idempotencyToken, acceptLanguage = Core.Nothing}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaProductId :: Lens.Lens' CreateProvisioningArtifact Types.ProductId
cpaProductId = Lens.field @"productId"
{-# INLINEABLE cpaProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The configuration for the provisioning artifact. The @info@ field accepts @ImportFromPhysicalID@ . 
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaParameters :: Lens.Lens' CreateProvisioningArtifact Types.ProvisioningArtifactProperties
cpaParameters = Lens.field @"parameters"
{-# INLINEABLE cpaParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaIdempotencyToken :: Lens.Lens' CreateProvisioningArtifact Types.IdempotencyToken
cpaIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE cpaIdempotencyToken #-}
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
cpaAcceptLanguage :: Lens.Lens' CreateProvisioningArtifact (Core.Maybe Types.AcceptLanguage)
cpaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE cpaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery CreateProvisioningArtifact where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProvisioningArtifact where
        toHeaders CreateProvisioningArtifact{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.CreateProvisioningArtifact")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProvisioningArtifact where
        toJSON CreateProvisioningArtifact{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  Core.Just ("Parameters" Core..= parameters),
                  Core.Just ("IdempotencyToken" Core..= idempotencyToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest CreateProvisioningArtifact where
        type Rs CreateProvisioningArtifact =
             CreateProvisioningArtifactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProvisioningArtifactResponse' Core.<$>
                   (x Core..:? "Info") Core.<*>
                     x Core..:? "ProvisioningArtifactDetail"
                     Core.<*> x Core..:? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProvisioningArtifactResponse' smart constructor.
data CreateProvisioningArtifactResponse = CreateProvisioningArtifactResponse'
  { info :: Core.Maybe (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue)
    -- ^ The URL of the CloudFormation template in Amazon S3, in JSON format.
  , provisioningArtifactDetail :: Core.Maybe Types.ProvisioningArtifactDetail
    -- ^ Information about the provisioning artifact.
  , status :: Core.Maybe Types.RequestStatus
    -- ^ The status of the current request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateProvisioningArtifactResponse' value with any optional fields omitted.
mkCreateProvisioningArtifactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProvisioningArtifactResponse
mkCreateProvisioningArtifactResponse responseStatus
  = CreateProvisioningArtifactResponse'{info = Core.Nothing,
                                        provisioningArtifactDetail = Core.Nothing,
                                        status = Core.Nothing, responseStatus}

-- | The URL of the CloudFormation template in Amazon S3, in JSON format.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparrsInfo :: Lens.Lens' CreateProvisioningArtifactResponse (Core.Maybe (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue))
cparrsInfo = Lens.field @"info"
{-# INLINEABLE cparrsInfo #-}
{-# DEPRECATED info "Use generic-lens or generic-optics with 'info' instead"  #-}

-- | Information about the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparrsProvisioningArtifactDetail :: Lens.Lens' CreateProvisioningArtifactResponse (Core.Maybe Types.ProvisioningArtifactDetail)
cparrsProvisioningArtifactDetail = Lens.field @"provisioningArtifactDetail"
{-# INLINEABLE cparrsProvisioningArtifactDetail #-}
{-# DEPRECATED provisioningArtifactDetail "Use generic-lens or generic-optics with 'provisioningArtifactDetail' instead"  #-}

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparrsStatus :: Lens.Lens' CreateProvisioningArtifactResponse (Core.Maybe Types.RequestStatus)
cparrsStatus = Lens.field @"status"
{-# INLINEABLE cparrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparrsResponseStatus :: Lens.Lens' CreateProvisioningArtifactResponse Core.Int
cparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
