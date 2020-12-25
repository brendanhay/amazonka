{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified provisioning artifact (also known as a version) for the specified product.
--
-- You cannot update a provisioning artifact for a product that was shared with you.
module Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
  ( -- * Creating a request
    UpdateProvisioningArtifact (..),
    mkUpdateProvisioningArtifact,

    -- ** Request lenses
    upaProductId,
    upaProvisioningArtifactId,
    upaAcceptLanguage,
    upaActive,
    upaDescription,
    upaGuidance,
    upaName,

    -- * Destructuring the response
    UpdateProvisioningArtifactResponse (..),
    mkUpdateProvisioningArtifactResponse,

    -- ** Response lenses
    uparrsInfo,
    uparrsProvisioningArtifactDetail,
    uparrsStatus,
    uparrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkUpdateProvisioningArtifact' smart constructor.
data UpdateProvisioningArtifact = UpdateProvisioningArtifact'
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | Indicates whether the product version is active.
    --
    -- Inactive provisioning artifacts are invisible to end users. End users cannot launch or update a provisioned product from an inactive provisioning artifact.
    active :: Core.Maybe Core.Bool,
    -- | The updated description of the provisioning artifact.
    description :: Core.Maybe Types.ProvisioningArtifactDescription,
    -- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
    --
    -- The @DEFAULT@ value indicates that the product version is active.
    -- The administrator can set the guidance to @DEPRECATED@ to inform users that the product version is deprecated. Users are able to make updates to a provisioned product of a deprecated version but cannot launch new provisioned products using a deprecated version.
    guidance :: Core.Maybe Types.ProvisioningArtifactGuidance,
    -- | The updated name of the provisioning artifact.
    name :: Core.Maybe Types.ProvisioningArtifactName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProvisioningArtifact' value with any optional fields omitted.
mkUpdateProvisioningArtifact ::
  -- | 'productId'
  Types.Id ->
  -- | 'provisioningArtifactId'
  Types.Id ->
  UpdateProvisioningArtifact
mkUpdateProvisioningArtifact productId provisioningArtifactId =
  UpdateProvisioningArtifact'
    { productId,
      provisioningArtifactId,
      acceptLanguage = Core.Nothing,
      active = Core.Nothing,
      description = Core.Nothing,
      guidance = Core.Nothing,
      name = Core.Nothing
    }

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaProductId :: Lens.Lens' UpdateProvisioningArtifact Types.Id
upaProductId = Lens.field @"productId"
{-# DEPRECATED upaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaProvisioningArtifactId :: Lens.Lens' UpdateProvisioningArtifact Types.Id
upaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# DEPRECATED upaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

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
upaAcceptLanguage :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Types.AcceptLanguage)
upaAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED upaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | Indicates whether the product version is active.
--
-- Inactive provisioning artifacts are invisible to end users. End users cannot launch or update a provisioned product from an inactive provisioning artifact.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaActive :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Core.Bool)
upaActive = Lens.field @"active"
{-# DEPRECATED upaActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The updated description of the provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaDescription :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Types.ProvisioningArtifactDescription)
upaDescription = Lens.field @"description"
{-# DEPRECATED upaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- The @DEFAULT@ value indicates that the product version is active.
-- The administrator can set the guidance to @DEPRECATED@ to inform users that the product version is deprecated. Users are able to make updates to a provisioned product of a deprecated version but cannot launch new provisioned products using a deprecated version.
--
-- /Note:/ Consider using 'guidance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaGuidance :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Types.ProvisioningArtifactGuidance)
upaGuidance = Lens.field @"guidance"
{-# DEPRECATED upaGuidance "Use generic-lens or generic-optics with 'guidance' instead." #-}

-- | The updated name of the provisioning artifact.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaName :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Types.ProvisioningArtifactName)
upaName = Lens.field @"name"
{-# DEPRECATED upaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateProvisioningArtifact where
  toJSON UpdateProvisioningArtifact {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProductId" Core..= productId),
            Core.Just
              ("ProvisioningArtifactId" Core..= provisioningArtifactId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("Active" Core..=) Core.<$> active,
            ("Description" Core..=) Core.<$> description,
            ("Guidance" Core..=) Core.<$> guidance,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateProvisioningArtifact where
  type
    Rs UpdateProvisioningArtifact =
      UpdateProvisioningArtifactResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.UpdateProvisioningArtifact"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProvisioningArtifactResponse'
            Core.<$> (x Core..:? "Info")
            Core.<*> (x Core..:? "ProvisioningArtifactDetail")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateProvisioningArtifactResponse' smart constructor.
data UpdateProvisioningArtifactResponse = UpdateProvisioningArtifactResponse'
  { -- | The URL of the CloudFormation template in Amazon S3.
    info :: Core.Maybe (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue),
    -- | Information about the provisioning artifact.
    provisioningArtifactDetail :: Core.Maybe Types.ProvisioningArtifactDetail,
    -- | The status of the current request.
    status :: Core.Maybe Types.RequestStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateProvisioningArtifactResponse' value with any optional fields omitted.
mkUpdateProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateProvisioningArtifactResponse
mkUpdateProvisioningArtifactResponse responseStatus =
  UpdateProvisioningArtifactResponse'
    { info = Core.Nothing,
      provisioningArtifactDetail = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The URL of the CloudFormation template in Amazon S3.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uparrsInfo :: Lens.Lens' UpdateProvisioningArtifactResponse (Core.Maybe (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue))
uparrsInfo = Lens.field @"info"
{-# DEPRECATED uparrsInfo "Use generic-lens or generic-optics with 'info' instead." #-}

-- | Information about the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uparrsProvisioningArtifactDetail :: Lens.Lens' UpdateProvisioningArtifactResponse (Core.Maybe Types.ProvisioningArtifactDetail)
uparrsProvisioningArtifactDetail = Lens.field @"provisioningArtifactDetail"
{-# DEPRECATED uparrsProvisioningArtifactDetail "Use generic-lens or generic-optics with 'provisioningArtifactDetail' instead." #-}

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uparrsStatus :: Lens.Lens' UpdateProvisioningArtifactResponse (Core.Maybe Types.RequestStatus)
uparrsStatus = Lens.field @"status"
{-# DEPRECATED uparrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uparrsResponseStatus :: Lens.Lens' UpdateProvisioningArtifactResponse Core.Int
uparrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
