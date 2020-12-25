{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image builder and releases the capacity.
module Network.AWS.AppStream.DeleteImageBuilder
  ( -- * Creating a request
    DeleteImageBuilder (..),
    mkDeleteImageBuilder,

    -- ** Request lenses
    dibName,

    -- * Destructuring the response
    DeleteImageBuilderResponse (..),
    mkDeleteImageBuilderResponse,

    -- ** Response lenses
    dibrrsImageBuilder,
    dibrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteImageBuilder' smart constructor.
newtype DeleteImageBuilder = DeleteImageBuilder'
  { -- | The name of the image builder.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImageBuilder' value with any optional fields omitted.
mkDeleteImageBuilder ::
  -- | 'name'
  Types.Name ->
  DeleteImageBuilder
mkDeleteImageBuilder name = DeleteImageBuilder' {name}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibName :: Lens.Lens' DeleteImageBuilder Types.Name
dibName = Lens.field @"name"
{-# DEPRECATED dibName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteImageBuilder where
  toJSON DeleteImageBuilder {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteImageBuilder where
  type Rs DeleteImageBuilder = DeleteImageBuilderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.DeleteImageBuilder")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImageBuilderResponse'
            Core.<$> (x Core..:? "ImageBuilder") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteImageBuilderResponse' smart constructor.
data DeleteImageBuilderResponse = DeleteImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Core.Maybe Types.ImageBuilder,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteImageBuilderResponse' value with any optional fields omitted.
mkDeleteImageBuilderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteImageBuilderResponse
mkDeleteImageBuilderResponse responseStatus =
  DeleteImageBuilderResponse'
    { imageBuilder = Core.Nothing,
      responseStatus
    }

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrrsImageBuilder :: Lens.Lens' DeleteImageBuilderResponse (Core.Maybe Types.ImageBuilder)
dibrrsImageBuilder = Lens.field @"imageBuilder"
{-# DEPRECATED dibrrsImageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dibrrsResponseStatus :: Lens.Lens' DeleteImageBuilderResponse Core.Int
dibrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dibrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
