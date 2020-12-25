{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.StartImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified image builder.
module Network.AWS.AppStream.StartImageBuilder
  ( -- * Creating a request
    StartImageBuilder (..),
    mkStartImageBuilder,

    -- ** Request lenses
    sibName,
    sibAppstreamAgentVersion,

    -- * Destructuring the response
    StartImageBuilderResponse (..),
    mkStartImageBuilderResponse,

    -- ** Response lenses
    sibrrsImageBuilder,
    sibrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartImageBuilder' smart constructor.
data StartImageBuilder = StartImageBuilder'
  { -- | The name of the image builder.
    name :: Types.String,
    -- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
    appstreamAgentVersion :: Core.Maybe Types.AppstreamAgentVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartImageBuilder' value with any optional fields omitted.
mkStartImageBuilder ::
  -- | 'name'
  Types.String ->
  StartImageBuilder
mkStartImageBuilder name =
  StartImageBuilder' {name, appstreamAgentVersion = Core.Nothing}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibName :: Lens.Lens' StartImageBuilder Types.String
sibName = Lens.field @"name"
{-# DEPRECATED sibName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibAppstreamAgentVersion :: Lens.Lens' StartImageBuilder (Core.Maybe Types.AppstreamAgentVersion)
sibAppstreamAgentVersion = Lens.field @"appstreamAgentVersion"
{-# DEPRECATED sibAppstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead." #-}

instance Core.FromJSON StartImageBuilder where
  toJSON StartImageBuilder {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("AppstreamAgentVersion" Core..=) Core.<$> appstreamAgentVersion
          ]
      )

instance Core.AWSRequest StartImageBuilder where
  type Rs StartImageBuilder = StartImageBuilderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.StartImageBuilder")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImageBuilderResponse'
            Core.<$> (x Core..:? "ImageBuilder") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartImageBuilderResponse' smart constructor.
data StartImageBuilderResponse = StartImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Core.Maybe Types.ImageBuilder,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartImageBuilderResponse' value with any optional fields omitted.
mkStartImageBuilderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartImageBuilderResponse
mkStartImageBuilderResponse responseStatus =
  StartImageBuilderResponse'
    { imageBuilder = Core.Nothing,
      responseStatus
    }

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrrsImageBuilder :: Lens.Lens' StartImageBuilderResponse (Core.Maybe Types.ImageBuilder)
sibrrsImageBuilder = Lens.field @"imageBuilder"
{-# DEPRECATED sibrrsImageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sibrrsResponseStatus :: Lens.Lens' StartImageBuilderResponse Core.Int
sibrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sibrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
