{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified self-service action association from the specified provisioning artifact.
module Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
    (
    -- * Creating a request
      DisassociateServiceActionFromProvisioningArtifact (..)
    , mkDisassociateServiceActionFromProvisioningArtifact
    -- ** Request lenses
    , dsafpaProductId
    , dsafpaProvisioningArtifactId
    , dsafpaServiceActionId
    , dsafpaAcceptLanguage

    -- * Destructuring the response
    , DisassociateServiceActionFromProvisioningArtifactResponse (..)
    , mkDisassociateServiceActionFromProvisioningArtifactResponse
    -- ** Response lenses
    , dsafparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisassociateServiceActionFromProvisioningArtifact' smart constructor.
data DisassociateServiceActionFromProvisioningArtifact = DisassociateServiceActionFromProvisioningArtifact'
  { productId :: Types.Id
    -- ^ The product identifier. For example, @prod-abcdzk7xy33qa@ .
  , provisioningArtifactId :: Types.Id
    -- ^ The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
  , serviceActionId :: Types.Id
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

-- | Creates a 'DisassociateServiceActionFromProvisioningArtifact' value with any optional fields omitted.
mkDisassociateServiceActionFromProvisioningArtifact
    :: Types.Id -- ^ 'productId'
    -> Types.Id -- ^ 'provisioningArtifactId'
    -> Types.Id -- ^ 'serviceActionId'
    -> DisassociateServiceActionFromProvisioningArtifact
mkDisassociateServiceActionFromProvisioningArtifact productId
  provisioningArtifactId serviceActionId
  = DisassociateServiceActionFromProvisioningArtifact'{productId,
                                                       provisioningArtifactId, serviceActionId,
                                                       acceptLanguage = Core.Nothing}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaProductId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Types.Id
dsafpaProductId = Lens.field @"productId"
{-# INLINEABLE dsafpaProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaProvisioningArtifactId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Types.Id
dsafpaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE dsafpaProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaServiceActionId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Types.Id
dsafpaServiceActionId = Lens.field @"serviceActionId"
{-# INLINEABLE dsafpaServiceActionId #-}
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
dsafpaAcceptLanguage :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact (Core.Maybe Types.AcceptLanguage)
dsafpaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dsafpaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery
           DisassociateServiceActionFromProvisioningArtifact
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           DisassociateServiceActionFromProvisioningArtifact
         where
        toHeaders DisassociateServiceActionFromProvisioningArtifact{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DisassociateServiceActionFromProvisioningArtifact")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           DisassociateServiceActionFromProvisioningArtifact
         where
        toJSON DisassociateServiceActionFromProvisioningArtifact{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  Core.Just
                    ("ProvisioningArtifactId" Core..= provisioningArtifactId),
                  Core.Just ("ServiceActionId" Core..= serviceActionId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest
           DisassociateServiceActionFromProvisioningArtifact
         where
        type Rs DisassociateServiceActionFromProvisioningArtifact =
             DisassociateServiceActionFromProvisioningArtifactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateServiceActionFromProvisioningArtifactResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
newtype DisassociateServiceActionFromProvisioningArtifactResponse = DisassociateServiceActionFromProvisioningArtifactResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateServiceActionFromProvisioningArtifactResponse' value with any optional fields omitted.
mkDisassociateServiceActionFromProvisioningArtifactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateServiceActionFromProvisioningArtifactResponse
mkDisassociateServiceActionFromProvisioningArtifactResponse
  responseStatus
  = DisassociateServiceActionFromProvisioningArtifactResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafparrsResponseStatus :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifactResponse Core.Int
dsafparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsafparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
