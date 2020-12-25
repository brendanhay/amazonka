{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DisassociateServiceActionFromProvisioningArtifact (..),
    mkDisassociateServiceActionFromProvisioningArtifact,

    -- ** Request lenses
    dsafpaProductId,
    dsafpaProvisioningArtifactId,
    dsafpaServiceActionId,
    dsafpaAcceptLanguage,

    -- * Destructuring the response
    DisassociateServiceActionFromProvisioningArtifactResponse (..),
    mkDisassociateServiceActionFromProvisioningArtifactResponse,

    -- ** Response lenses
    dsafparrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisassociateServiceActionFromProvisioningArtifact' smart constructor.
data DisassociateServiceActionFromProvisioningArtifact = DisassociateServiceActionFromProvisioningArtifact'
  { -- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
    productId :: Types.Id,
    -- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
    provisioningArtifactId :: Types.Id,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
    serviceActionId :: Types.Id,
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateServiceActionFromProvisioningArtifact' value with any optional fields omitted.
mkDisassociateServiceActionFromProvisioningArtifact ::
  -- | 'productId'
  Types.Id ->
  -- | 'provisioningArtifactId'
  Types.Id ->
  -- | 'serviceActionId'
  Types.Id ->
  DisassociateServiceActionFromProvisioningArtifact
mkDisassociateServiceActionFromProvisioningArtifact
  productId
  provisioningArtifactId
  serviceActionId =
    DisassociateServiceActionFromProvisioningArtifact'
      { productId,
        provisioningArtifactId,
        serviceActionId,
        acceptLanguage = Core.Nothing
      }

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaProductId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Types.Id
dsafpaProductId = Lens.field @"productId"
{-# DEPRECATED dsafpaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaProvisioningArtifactId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Types.Id
dsafpaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# DEPRECATED dsafpaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaServiceActionId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Types.Id
dsafpaServiceActionId = Lens.field @"serviceActionId"
{-# DEPRECATED dsafpaServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

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
{-# DEPRECATED dsafpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance
  Core.FromJSON
    DisassociateServiceActionFromProvisioningArtifact
  where
  toJSON DisassociateServiceActionFromProvisioningArtifact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProductId" Core..= productId),
            Core.Just
              ("ProvisioningArtifactId" Core..= provisioningArtifactId),
            Core.Just ("ServiceActionId" Core..= serviceActionId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance
  Core.AWSRequest
    DisassociateServiceActionFromProvisioningArtifact
  where
  type
    Rs DisassociateServiceActionFromProvisioningArtifact =
      DisassociateServiceActionFromProvisioningArtifactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DisassociateServiceActionFromProvisioningArtifact"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateServiceActionFromProvisioningArtifactResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
newtype DisassociateServiceActionFromProvisioningArtifactResponse = DisassociateServiceActionFromProvisioningArtifactResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateServiceActionFromProvisioningArtifactResponse' value with any optional fields omitted.
mkDisassociateServiceActionFromProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateServiceActionFromProvisioningArtifactResponse
mkDisassociateServiceActionFromProvisioningArtifactResponse
  responseStatus =
    DisassociateServiceActionFromProvisioningArtifactResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafparrsResponseStatus :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifactResponse Core.Int
dsafparrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsafparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
