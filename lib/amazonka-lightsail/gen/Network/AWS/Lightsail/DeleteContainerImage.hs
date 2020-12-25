{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a container image that is registered to your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerImage
  ( -- * Creating a request
    DeleteContainerImage (..),
    mkDeleteContainerImage,

    -- ** Request lenses
    dciServiceName,
    dciImage,

    -- * Destructuring the response
    DeleteContainerImageResponse (..),
    mkDeleteContainerImageResponse,

    -- ** Response lenses
    dcirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContainerImage' smart constructor.
data DeleteContainerImage = DeleteContainerImage'
  { -- | The name of the container service for which to delete a registered container image.
    serviceName :: Types.ContainerServiceName,
    -- | The name of the container image to delete from the container service.
    --
    -- Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
    image :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerImage' value with any optional fields omitted.
mkDeleteContainerImage ::
  -- | 'serviceName'
  Types.ContainerServiceName ->
  -- | 'image'
  Types.String ->
  DeleteContainerImage
mkDeleteContainerImage serviceName image =
  DeleteContainerImage' {serviceName, image}

-- | The name of the container service for which to delete a registered container image.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciServiceName :: Lens.Lens' DeleteContainerImage Types.ContainerServiceName
dciServiceName = Lens.field @"serviceName"
{-# DEPRECATED dciServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The name of the container image to delete from the container service.
--
-- Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciImage :: Lens.Lens' DeleteContainerImage Types.String
dciImage = Lens.field @"image"
{-# DEPRECATED dciImage "Use generic-lens or generic-optics with 'image' instead." #-}

instance Core.FromJSON DeleteContainerImage where
  toJSON DeleteContainerImage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("serviceName" Core..= serviceName),
            Core.Just ("image" Core..= image)
          ]
      )

instance Core.AWSRequest DeleteContainerImage where
  type Rs DeleteContainerImage = DeleteContainerImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.DeleteContainerImage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerImageResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteContainerImageResponse' smart constructor.
newtype DeleteContainerImageResponse = DeleteContainerImageResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerImageResponse' value with any optional fields omitted.
mkDeleteContainerImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteContainerImageResponse
mkDeleteContainerImageResponse responseStatus =
  DeleteContainerImageResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirrsResponseStatus :: Lens.Lens' DeleteContainerImageResponse Core.Int
dcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
