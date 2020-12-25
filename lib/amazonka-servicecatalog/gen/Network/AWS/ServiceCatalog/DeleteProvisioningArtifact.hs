{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified provisioning artifact (also known as a version) for the specified product.
--
-- You cannot delete a provisioning artifact associated with a product that was shared with you. You cannot delete the last provisioning artifact for a product, because a product must have at least one provisioning artifact.
module Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
  ( -- * Creating a request
    DeleteProvisioningArtifact (..),
    mkDeleteProvisioningArtifact,

    -- ** Request lenses
    dpafProductId,
    dpafProvisioningArtifactId,
    dpafAcceptLanguage,

    -- * Destructuring the response
    DeleteProvisioningArtifactResponse (..),
    mkDeleteProvisioningArtifactResponse,

    -- ** Response lenses
    dparfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeleteProvisioningArtifact' smart constructor.
data DeleteProvisioningArtifact = DeleteProvisioningArtifact'
  { -- | The product identifier.
    productId :: Types.Id,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Types.Id,
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

-- | Creates a 'DeleteProvisioningArtifact' value with any optional fields omitted.
mkDeleteProvisioningArtifact ::
  -- | 'productId'
  Types.Id ->
  -- | 'provisioningArtifactId'
  Types.Id ->
  DeleteProvisioningArtifact
mkDeleteProvisioningArtifact productId provisioningArtifactId =
  DeleteProvisioningArtifact'
    { productId,
      provisioningArtifactId,
      acceptLanguage = Core.Nothing
    }

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpafProductId :: Lens.Lens' DeleteProvisioningArtifact Types.Id
dpafProductId = Lens.field @"productId"
{-# DEPRECATED dpafProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpafProvisioningArtifactId :: Lens.Lens' DeleteProvisioningArtifact Types.Id
dpafProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# DEPRECATED dpafProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

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
dpafAcceptLanguage :: Lens.Lens' DeleteProvisioningArtifact (Core.Maybe Types.AcceptLanguage)
dpafAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpafAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DeleteProvisioningArtifact where
  toJSON DeleteProvisioningArtifact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProductId" Core..= productId),
            Core.Just
              ("ProvisioningArtifactId" Core..= provisioningArtifactId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DeleteProvisioningArtifact where
  type
    Rs DeleteProvisioningArtifact =
      DeleteProvisioningArtifactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DeleteProvisioningArtifact"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisioningArtifactResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteProvisioningArtifactResponse' smart constructor.
newtype DeleteProvisioningArtifactResponse = DeleteProvisioningArtifactResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProvisioningArtifactResponse' value with any optional fields omitted.
mkDeleteProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteProvisioningArtifactResponse
mkDeleteProvisioningArtifactResponse responseStatus =
  DeleteProvisioningArtifactResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparfrsResponseStatus :: Lens.Lens' DeleteProvisioningArtifactResponse Core.Int
dparfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dparfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
