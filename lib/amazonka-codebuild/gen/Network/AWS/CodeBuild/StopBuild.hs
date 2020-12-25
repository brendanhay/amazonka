{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.StopBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop running a build.
module Network.AWS.CodeBuild.StopBuild
  ( -- * Creating a request
    StopBuild (..),
    mkStopBuild,

    -- ** Request lenses
    sbId,

    -- * Destructuring the response
    StopBuildResponse (..),
    mkStopBuildResponse,

    -- ** Response lenses
    sbrfrsBuild,
    sbrfrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopBuild' smart constructor.
newtype StopBuild = StopBuild'
  { -- | The ID of the build.
    id :: Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopBuild' value with any optional fields omitted.
mkStopBuild ::
  -- | 'id'
  Types.NonEmptyString ->
  StopBuild
mkStopBuild id = StopBuild' {id}

-- | The ID of the build.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbId :: Lens.Lens' StopBuild Types.NonEmptyString
sbId = Lens.field @"id"
{-# DEPRECATED sbId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON StopBuild where
  toJSON StopBuild {..} =
    Core.object (Core.catMaybes [Core.Just ("id" Core..= id)])

instance Core.AWSRequest StopBuild where
  type Rs StopBuild = StopBuildResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.StopBuild")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopBuildResponse'
            Core.<$> (x Core..:? "build") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopBuildResponse' smart constructor.
data StopBuildResponse = StopBuildResponse'
  { -- | Information about the build.
    build :: Core.Maybe Types.Build,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StopBuildResponse' value with any optional fields omitted.
mkStopBuildResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopBuildResponse
mkStopBuildResponse responseStatus =
  StopBuildResponse' {build = Core.Nothing, responseStatus}

-- | Information about the build.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrfrsBuild :: Lens.Lens' StopBuildResponse (Core.Maybe Types.Build)
sbrfrsBuild = Lens.field @"build"
{-# DEPRECATED sbrfrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrfrsResponseStatus :: Lens.Lens' StopBuildResponse Core.Int
sbrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sbrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
