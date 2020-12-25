{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.RegisterContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a container image to your Amazon Lightsail container service.
module Network.AWS.Lightsail.RegisterContainerImage
  ( -- * Creating a request
    RegisterContainerImage (..),
    mkRegisterContainerImage,

    -- ** Request lenses
    rciServiceName,
    rciLabel,
    rciDigest,

    -- * Destructuring the response
    RegisterContainerImageResponse (..),
    mkRegisterContainerImageResponse,

    -- ** Response lenses
    rcirrsContainerImage,
    rcirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterContainerImage' smart constructor.
data RegisterContainerImage = RegisterContainerImage'
  { -- | The name of the container service for which to register a container image.
    serviceName :: Types.ServiceName,
    -- | The label for the container image when it's registered to the container service.
    --
    -- Use a descriptive label that you can use to track the different versions of your registered container images.
    -- Use the @GetContainerImages@ action to return the container images registered to a Lightsail container service. The label is the @<imagelabel>@ portion of the following image name example:
    --
    --     * @:container-service-1.<imagelabel>.1@
    --
    --
    -- If the name of your container service is @mycontainerservice@ , and the label that you specify is @mystaticwebsite@ , then the name of the registered container image will be @:mycontainerservice.mystaticwebsite.1@ .
    -- The number at the end of these image name examples represents the version of the registered container image. If you push and register another container image to the same Lightsail container service, with the same label, then the version number for the new registered container image will be @2@ . If you push and register another container image, the version number will be @3@ , and so on.
    label :: Types.Label,
    -- | The digest of the container image to be registered.
    digest :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterContainerImage' value with any optional fields omitted.
mkRegisterContainerImage ::
  -- | 'serviceName'
  Types.ServiceName ->
  -- | 'label'
  Types.Label ->
  -- | 'digest'
  Types.String ->
  RegisterContainerImage
mkRegisterContainerImage serviceName label digest =
  RegisterContainerImage' {serviceName, label, digest}

-- | The name of the container service for which to register a container image.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciServiceName :: Lens.Lens' RegisterContainerImage Types.ServiceName
rciServiceName = Lens.field @"serviceName"
{-# DEPRECATED rciServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The label for the container image when it's registered to the container service.
--
-- Use a descriptive label that you can use to track the different versions of your registered container images.
-- Use the @GetContainerImages@ action to return the container images registered to a Lightsail container service. The label is the @<imagelabel>@ portion of the following image name example:
--
--     * @:container-service-1.<imagelabel>.1@
--
--
-- If the name of your container service is @mycontainerservice@ , and the label that you specify is @mystaticwebsite@ , then the name of the registered container image will be @:mycontainerservice.mystaticwebsite.1@ .
-- The number at the end of these image name examples represents the version of the registered container image. If you push and register another container image to the same Lightsail container service, with the same label, then the version number for the new registered container image will be @2@ . If you push and register another container image, the version number will be @3@ , and so on.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciLabel :: Lens.Lens' RegisterContainerImage Types.Label
rciLabel = Lens.field @"label"
{-# DEPRECATED rciLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The digest of the container image to be registered.
--
-- /Note:/ Consider using 'digest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciDigest :: Lens.Lens' RegisterContainerImage Types.String
rciDigest = Lens.field @"digest"
{-# DEPRECATED rciDigest "Use generic-lens or generic-optics with 'digest' instead." #-}

instance Core.FromJSON RegisterContainerImage where
  toJSON RegisterContainerImage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("serviceName" Core..= serviceName),
            Core.Just ("label" Core..= label),
            Core.Just ("digest" Core..= digest)
          ]
      )

instance Core.AWSRequest RegisterContainerImage where
  type Rs RegisterContainerImage = RegisterContainerImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.RegisterContainerImage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterContainerImageResponse'
            Core.<$> (x Core..:? "containerImage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterContainerImageResponse' smart constructor.
data RegisterContainerImageResponse = RegisterContainerImageResponse'
  { containerImage :: Core.Maybe Types.ContainerImage,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RegisterContainerImageResponse' value with any optional fields omitted.
mkRegisterContainerImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterContainerImageResponse
mkRegisterContainerImageResponse responseStatus =
  RegisterContainerImageResponse'
    { containerImage = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'containerImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirrsContainerImage :: Lens.Lens' RegisterContainerImageResponse (Core.Maybe Types.ContainerImage)
rcirrsContainerImage = Lens.field @"containerImage"
{-# DEPRECATED rcirrsContainerImage "Use generic-lens or generic-optics with 'containerImage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirrsResponseStatus :: Lens.Lens' RegisterContainerImageResponse Core.Int
rcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
