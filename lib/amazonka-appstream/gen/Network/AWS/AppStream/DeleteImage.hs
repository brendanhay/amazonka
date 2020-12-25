{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image. You cannot delete an image when it is in use. After you delete an image, you cannot provision new capacity using the image.
module Network.AWS.AppStream.DeleteImage
  ( -- * Creating a request
    DeleteImage (..),
    mkDeleteImage,

    -- ** Request lenses
    diName,

    -- * Destructuring the response
    DeleteImageResponse (..),
    mkDeleteImageResponse,

    -- ** Response lenses
    dirrsImage,
    dirrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteImage' smart constructor.
newtype DeleteImage = DeleteImage'
  { -- | The name of the image.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImage' value with any optional fields omitted.
mkDeleteImage ::
  -- | 'name'
  Types.Name ->
  DeleteImage
mkDeleteImage name = DeleteImage' {name}

-- | The name of the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DeleteImage Types.Name
diName = Lens.field @"name"
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteImage where
  toJSON DeleteImage {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteImage where
  type Rs DeleteImage = DeleteImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "PhotonAdminProxyService.DeleteImage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImageResponse'
            Core.<$> (x Core..:? "Image") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteImageResponse' smart constructor.
data DeleteImageResponse = DeleteImageResponse'
  { -- | Information about the image.
    image :: Core.Maybe Types.Image,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteImageResponse' value with any optional fields omitted.
mkDeleteImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteImageResponse
mkDeleteImageResponse responseStatus =
  DeleteImageResponse' {image = Core.Nothing, responseStatus}

-- | Information about the image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsImage :: Lens.Lens' DeleteImageResponse (Core.Maybe Types.Image)
dirrsImage = Lens.field @"image"
{-# DEPRECATED dirrsImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteImageResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
