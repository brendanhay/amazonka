{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RegisterContainerImage (..)
    , mkRegisterContainerImage
    -- ** Request lenses
    , rciServiceName
    , rciLabel
    , rciDigest

    -- * Destructuring the response
    , RegisterContainerImageResponse (..)
    , mkRegisterContainerImageResponse
    -- ** Response lenses
    , rcirrsContainerImage
    , rcirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterContainerImage' smart constructor.
data RegisterContainerImage = RegisterContainerImage'
  { serviceName :: Types.ServiceName
    -- ^ The name of the container service for which to register a container image.
  , label :: Types.Label
    -- ^ The label for the container image when it's registered to the container service.
--
-- Use a descriptive label that you can use to track the different versions of your registered container images.
-- Use the @GetContainerImages@ action to return the container images registered to a Lightsail container service. The label is the @<imagelabel>@ portion of the following image name example:
--
--     * @:container-service-1.<imagelabel>.1@ 
--
--
-- If the name of your container service is @mycontainerservice@ , and the label that you specify is @mystaticwebsite@ , then the name of the registered container image will be @:mycontainerservice.mystaticwebsite.1@ .
-- The number at the end of these image name examples represents the version of the registered container image. If you push and register another container image to the same Lightsail container service, with the same label, then the version number for the new registered container image will be @2@ . If you push and register another container image, the version number will be @3@ , and so on.
  , digest :: Core.Text
    -- ^ The digest of the container image to be registered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterContainerImage' value with any optional fields omitted.
mkRegisterContainerImage
    :: Types.ServiceName -- ^ 'serviceName'
    -> Types.Label -- ^ 'label'
    -> Core.Text -- ^ 'digest'
    -> RegisterContainerImage
mkRegisterContainerImage serviceName label digest
  = RegisterContainerImage'{serviceName, label, digest}

-- | The name of the container service for which to register a container image.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciServiceName :: Lens.Lens' RegisterContainerImage Types.ServiceName
rciServiceName = Lens.field @"serviceName"
{-# INLINEABLE rciServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

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
{-# INLINEABLE rciLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

-- | The digest of the container image to be registered.
--
-- /Note:/ Consider using 'digest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciDigest :: Lens.Lens' RegisterContainerImage Core.Text
rciDigest = Lens.field @"digest"
{-# INLINEABLE rciDigest #-}
{-# DEPRECATED digest "Use generic-lens or generic-optics with 'digest' instead"  #-}

instance Core.ToQuery RegisterContainerImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterContainerImage where
        toHeaders RegisterContainerImage{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.RegisterContainerImage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterContainerImage where
        toJSON RegisterContainerImage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("serviceName" Core..= serviceName),
                  Core.Just ("label" Core..= label),
                  Core.Just ("digest" Core..= digest)])

instance Core.AWSRequest RegisterContainerImage where
        type Rs RegisterContainerImage = RegisterContainerImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterContainerImageResponse' Core.<$>
                   (x Core..:? "containerImage") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterContainerImageResponse' smart constructor.
data RegisterContainerImageResponse = RegisterContainerImageResponse'
  { containerImage :: Core.Maybe Types.ContainerImage
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RegisterContainerImageResponse' value with any optional fields omitted.
mkRegisterContainerImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterContainerImageResponse
mkRegisterContainerImageResponse responseStatus
  = RegisterContainerImageResponse'{containerImage = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'containerImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirrsContainerImage :: Lens.Lens' RegisterContainerImageResponse (Core.Maybe Types.ContainerImage)
rcirrsContainerImage = Lens.field @"containerImage"
{-# INLINEABLE rcirrsContainerImage #-}
{-# DEPRECATED containerImage "Use generic-lens or generic-optics with 'containerImage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirrsResponseStatus :: Lens.Lens' RegisterContainerImageResponse Core.Int
rcirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
