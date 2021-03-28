{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a batch of self-service actions from the specified provisioning artifact.
module Network.AWS.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
    (
    -- * Creating a request
      BatchDisassociateServiceActionFromProvisioningArtifact (..)
    , mkBatchDisassociateServiceActionFromProvisioningArtifact
    -- ** Request lenses
    , bdsafpaServiceActionAssociations
    , bdsafpaAcceptLanguage

    -- * Destructuring the response
    , BatchDisassociateServiceActionFromProvisioningArtifactResponse (..)
    , mkBatchDisassociateServiceActionFromProvisioningArtifactResponse
    -- ** Response lenses
    , bdsafparrsFailedServiceActionAssociations
    , bdsafparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkBatchDisassociateServiceActionFromProvisioningArtifact' smart constructor.
data BatchDisassociateServiceActionFromProvisioningArtifact = BatchDisassociateServiceActionFromProvisioningArtifact'
  { serviceActionAssociations :: Core.NonEmpty Types.ServiceActionAssociation
    -- ^ One or more associations, each consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
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

-- | Creates a 'BatchDisassociateServiceActionFromProvisioningArtifact' value with any optional fields omitted.
mkBatchDisassociateServiceActionFromProvisioningArtifact
    :: Core.NonEmpty Types.ServiceActionAssociation -- ^ 'serviceActionAssociations'
    -> BatchDisassociateServiceActionFromProvisioningArtifact
mkBatchDisassociateServiceActionFromProvisioningArtifact
  serviceActionAssociations
  = BatchDisassociateServiceActionFromProvisioningArtifact'{serviceActionAssociations,
                                                            acceptLanguage = Core.Nothing}

-- | One or more associations, each consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
--
-- /Note:/ Consider using 'serviceActionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsafpaServiceActionAssociations :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifact (Core.NonEmpty Types.ServiceActionAssociation)
bdsafpaServiceActionAssociations = Lens.field @"serviceActionAssociations"
{-# INLINEABLE bdsafpaServiceActionAssociations #-}
{-# DEPRECATED serviceActionAssociations "Use generic-lens or generic-optics with 'serviceActionAssociations' instead"  #-}

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
bdsafpaAcceptLanguage :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifact (Core.Maybe Types.AcceptLanguage)
bdsafpaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE bdsafpaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery
           BatchDisassociateServiceActionFromProvisioningArtifact
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders
           BatchDisassociateServiceActionFromProvisioningArtifact
         where
        toHeaders
          BatchDisassociateServiceActionFromProvisioningArtifact{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.BatchDisassociateServiceActionFromProvisioningArtifact")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON
           BatchDisassociateServiceActionFromProvisioningArtifact
         where
        toJSON BatchDisassociateServiceActionFromProvisioningArtifact{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ServiceActionAssociations" Core..= serviceActionAssociations),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest
           BatchDisassociateServiceActionFromProvisioningArtifact
         where
        type Rs BatchDisassociateServiceActionFromProvisioningArtifact =
             BatchDisassociateServiceActionFromProvisioningArtifactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchDisassociateServiceActionFromProvisioningArtifactResponse'
                   Core.<$>
                   (x Core..:? "FailedServiceActionAssociations") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchDisassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
data BatchDisassociateServiceActionFromProvisioningArtifactResponse = BatchDisassociateServiceActionFromProvisioningArtifactResponse'
  { failedServiceActionAssociations :: Core.Maybe [Types.FailedServiceActionAssociation]
    -- ^ An object that contains a list of errors, along with information to help you identify the self-service action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDisassociateServiceActionFromProvisioningArtifactResponse' value with any optional fields omitted.
mkBatchDisassociateServiceActionFromProvisioningArtifactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchDisassociateServiceActionFromProvisioningArtifactResponse
mkBatchDisassociateServiceActionFromProvisioningArtifactResponse
  responseStatus
  = BatchDisassociateServiceActionFromProvisioningArtifactResponse'{failedServiceActionAssociations
                                                                      = Core.Nothing,
                                                                    responseStatus}

-- | An object that contains a list of errors, along with information to help you identify the self-service action.
--
-- /Note:/ Consider using 'failedServiceActionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsafparrsFailedServiceActionAssociations :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifactResponse (Core.Maybe [Types.FailedServiceActionAssociation])
bdsafparrsFailedServiceActionAssociations = Lens.field @"failedServiceActionAssociations"
{-# INLINEABLE bdsafparrsFailedServiceActionAssociations #-}
{-# DEPRECATED failedServiceActionAssociations "Use generic-lens or generic-optics with 'failedServiceActionAssociations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsafparrsResponseStatus :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifactResponse Core.Int
bdsafparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bdsafparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
