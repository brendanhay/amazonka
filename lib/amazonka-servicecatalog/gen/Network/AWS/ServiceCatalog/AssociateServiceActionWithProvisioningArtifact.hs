{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a self-service action with a provisioning artifact.
module Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
    (
    -- * Creating a request
      AssociateServiceActionWithProvisioningArtifact (..)
    , mkAssociateServiceActionWithProvisioningArtifact
    -- ** Request lenses
    , asawpaProductId
    , asawpaProvisioningArtifactId
    , asawpaServiceActionId
    , asawpaAcceptLanguage

    -- * Destructuring the response
    , AssociateServiceActionWithProvisioningArtifactResponse (..)
    , mkAssociateServiceActionWithProvisioningArtifactResponse
    -- ** Response lenses
    , asawparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAssociateServiceActionWithProvisioningArtifact' smart constructor.
data AssociateServiceActionWithProvisioningArtifact = AssociateServiceActionWithProvisioningArtifact'
  { productId :: Types.ProductId
    -- ^ The product identifier. For example, @prod-abcdzk7xy33qa@ .
  , provisioningArtifactId :: Types.ProvisioningArtifactId
    -- ^ The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
  , serviceActionId :: Types.ServiceActionId
    -- ^ The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
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

-- | Creates a 'AssociateServiceActionWithProvisioningArtifact' value with any optional fields omitted.
mkAssociateServiceActionWithProvisioningArtifact
    :: Types.ProductId -- ^ 'productId'
    -> Types.ProvisioningArtifactId -- ^ 'provisioningArtifactId'
    -> Types.ServiceActionId -- ^ 'serviceActionId'
    -> AssociateServiceActionWithProvisioningArtifact
mkAssociateServiceActionWithProvisioningArtifact productId
  provisioningArtifactId serviceActionId
  = AssociateServiceActionWithProvisioningArtifact'{productId,
                                                    provisioningArtifactId, serviceActionId,
                                                    acceptLanguage = Core.Nothing}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asawpaProductId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Types.ProductId
asawpaProductId = Lens.field @"productId"
{-# INLINEABLE asawpaProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asawpaProvisioningArtifactId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Types.ProvisioningArtifactId
asawpaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE asawpaProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asawpaServiceActionId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Types.ServiceActionId
asawpaServiceActionId = Lens.field @"serviceActionId"
{-# INLINEABLE asawpaServiceActionId #-}
{-# DEPRECATED serviceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead"  #-}

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
asawpaAcceptLanguage :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact (Core.Maybe Types.AcceptLanguage)
asawpaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE asawpaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery
           AssociateServiceActionWithProvisioningArtifact
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           AssociateServiceActionWithProvisioningArtifact
         where
        toHeaders AssociateServiceActionWithProvisioningArtifact{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.AssociateServiceActionWithProvisioningArtifact")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           AssociateServiceActionWithProvisioningArtifact
         where
        toJSON AssociateServiceActionWithProvisioningArtifact{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  Core.Just
                    ("ProvisioningArtifactId" Core..= provisioningArtifactId),
                  Core.Just ("ServiceActionId" Core..= serviceActionId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest
           AssociateServiceActionWithProvisioningArtifact
         where
        type Rs AssociateServiceActionWithProvisioningArtifact =
             AssociateServiceActionWithProvisioningArtifactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateServiceActionWithProvisioningArtifactResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateServiceActionWithProvisioningArtifactResponse' smart constructor.
newtype AssociateServiceActionWithProvisioningArtifactResponse = AssociateServiceActionWithProvisioningArtifactResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateServiceActionWithProvisioningArtifactResponse' value with any optional fields omitted.
mkAssociateServiceActionWithProvisioningArtifactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateServiceActionWithProvisioningArtifactResponse
mkAssociateServiceActionWithProvisioningArtifactResponse
  responseStatus
  = AssociateServiceActionWithProvisioningArtifactResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asawparrsResponseStatus :: Lens.Lens' AssociateServiceActionWithProvisioningArtifactResponse Core.Int
asawparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asawparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
