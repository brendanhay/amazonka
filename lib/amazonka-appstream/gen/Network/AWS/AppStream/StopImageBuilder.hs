{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StopImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified image builder.
module Network.AWS.AppStream.StopImageBuilder
  ( -- * Creating a request
    StopImageBuilder (..),
    mkStopImageBuilder,

    -- ** Request lenses
    sibfName,

    -- * Destructuring the response
    StopImageBuilderResponse (..),
    mkStopImageBuilderResponse,

    -- ** Response lenses
    sibrfrsImageBuilder,
    sibrfrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopImageBuilder' smart constructor.
newtype StopImageBuilder = StopImageBuilder'
  { -- | The name of the image builder.
    name :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopImageBuilder' value with any optional fields omitted.
mkStopImageBuilder ::
  -- | 'name'
  Types.String ->
  StopImageBuilder
mkStopImageBuilder name = StopImageBuilder' {name}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibfName :: Lens.Lens' StopImageBuilder Types.String
sibfName = Lens.field @"name"
{-# DEPRECATED sibfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON StopImageBuilder where
  toJSON StopImageBuilder {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StopImageBuilder where
  type Rs StopImageBuilder = StopImageBuilderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.StopImageBuilder")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopImageBuilderResponse'
            Core.<$> (x Core..:? "ImageBuilder") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopImageBuilderResponse' smart constructor.
data StopImageBuilderResponse = StopImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Core.Maybe Types.ImageBuilder,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StopImageBuilderResponse' value with any optional fields omitted.
mkStopImageBuilderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopImageBuilderResponse
mkStopImageBuilderResponse responseStatus =
  StopImageBuilderResponse'
    { imageBuilder = Core.Nothing,
      responseStatus
    }

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrfrsImageBuilder :: Lens.Lens' StopImageBuilderResponse (Core.Maybe Types.ImageBuilder)
sibrfrsImageBuilder = Lens.field @"imageBuilder"
{-# DEPRECATED sibrfrsImageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrfrsResponseStatus :: Lens.Lens' StopImageBuilderResponse Core.Int
sibrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sibrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
