{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the container images that are registered to your Amazon Lightsail container service.
module Network.AWS.Lightsail.GetContainerImages
  ( -- * Creating a request
    GetContainerImages (..),
    mkGetContainerImages,

    -- ** Request lenses
    gciServiceName,

    -- * Destructuring the response
    GetContainerImagesResponse (..),
    mkGetContainerImagesResponse,

    -- ** Response lenses
    gcirrsContainerImages,
    gcirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerImages' smart constructor.
newtype GetContainerImages = GetContainerImages'
  { -- | The name of the container service for which to return registered container images.
    serviceName :: Types.ServiceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerImages' value with any optional fields omitted.
mkGetContainerImages ::
  -- | 'serviceName'
  Types.ServiceName ->
  GetContainerImages
mkGetContainerImages serviceName = GetContainerImages' {serviceName}

-- | The name of the container service for which to return registered container images.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciServiceName :: Lens.Lens' GetContainerImages Types.ServiceName
gciServiceName = Lens.field @"serviceName"
{-# DEPRECATED gciServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Core.FromJSON GetContainerImages where
  toJSON GetContainerImages {..} =
    Core.object
      (Core.catMaybes [Core.Just ("serviceName" Core..= serviceName)])

instance Core.AWSRequest GetContainerImages where
  type Rs GetContainerImages = GetContainerImagesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetContainerImages")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerImagesResponse'
            Core.<$> (x Core..:? "containerImages")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContainerImagesResponse' smart constructor.
data GetContainerImagesResponse = GetContainerImagesResponse'
  { -- | An array of objects that describe container images that are registered to the container service.
    containerImages :: Core.Maybe [Types.ContainerImage],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetContainerImagesResponse' value with any optional fields omitted.
mkGetContainerImagesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetContainerImagesResponse
mkGetContainerImagesResponse responseStatus =
  GetContainerImagesResponse'
    { containerImages = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe container images that are registered to the container service.
--
-- /Note:/ Consider using 'containerImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsContainerImages :: Lens.Lens' GetContainerImagesResponse (Core.Maybe [Types.ContainerImage])
gcirrsContainerImages = Lens.field @"containerImages"
{-# DEPRECATED gcirrsContainerImages "Use generic-lens or generic-optics with 'containerImages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirrsResponseStatus :: Lens.Lens' GetContainerImagesResponse Core.Int
gcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
