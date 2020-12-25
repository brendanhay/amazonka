{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata in a build resource, including the build name and version. To update the metadata, specify the build ID to update and provide the new values. If successful, a build object containing the updated metadata is returned.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html Upload a Custom Server Build>
-- __Related operations__
--
--     * 'CreateBuild'
--
--
--     * 'ListBuilds'
--
--
--     * 'DescribeBuild'
--
--
--     * 'UpdateBuild'
--
--
--     * 'DeleteBuild'
module Network.AWS.GameLift.UpdateBuild
  ( -- * Creating a request
    UpdateBuild (..),
    mkUpdateBuild,

    -- ** Request lenses
    ubBuildId,
    ubName,
    ubVersion,

    -- * Destructuring the response
    UpdateBuildResponse (..),
    mkUpdateBuildResponse,

    -- ** Response lenses
    ubrrsBuild,
    ubrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateBuild' smart constructor.
data UpdateBuild = UpdateBuild'
  { -- | A unique identifier for a build to update. You can use either the build ID or ARN value.
    buildId :: Types.BuildIdOrArn,
    -- | A descriptive label that is associated with a build. Build names do not need to be unique.
    name :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Version information that is associated with a build or script. Version strings do not need to be unique.
    version :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBuild' value with any optional fields omitted.
mkUpdateBuild ::
  -- | 'buildId'
  Types.BuildIdOrArn ->
  UpdateBuild
mkUpdateBuild buildId =
  UpdateBuild'
    { buildId,
      name = Core.Nothing,
      version = Core.Nothing
    }

-- | A unique identifier for a build to update. You can use either the build ID or ARN value.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubBuildId :: Lens.Lens' UpdateBuild Types.BuildIdOrArn
ubBuildId = Lens.field @"buildId"
{-# DEPRECATED ubBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

-- | A descriptive label that is associated with a build. Build names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubName :: Lens.Lens' UpdateBuild (Core.Maybe Types.NonZeroAndMaxString)
ubName = Lens.field @"name"
{-# DEPRECATED ubName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Version information that is associated with a build or script. Version strings do not need to be unique.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubVersion :: Lens.Lens' UpdateBuild (Core.Maybe Types.NonZeroAndMaxString)
ubVersion = Lens.field @"version"
{-# DEPRECATED ubVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON UpdateBuild where
  toJSON UpdateBuild {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BuildId" Core..= buildId),
            ("Name" Core..=) Core.<$> name,
            ("Version" Core..=) Core.<$> version
          ]
      )

instance Core.AWSRequest UpdateBuild where
  type Rs UpdateBuild = UpdateBuildResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.UpdateBuild")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBuildResponse'
            Core.<$> (x Core..:? "Build") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateBuildResponse' smart constructor.
data UpdateBuildResponse = UpdateBuildResponse'
  { -- | The updated build resource.
    build :: Core.Maybe Types.Build,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateBuildResponse' value with any optional fields omitted.
mkUpdateBuildResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateBuildResponse
mkUpdateBuildResponse responseStatus =
  UpdateBuildResponse' {build = Core.Nothing, responseStatus}

-- | The updated build resource.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsBuild :: Lens.Lens' UpdateBuildResponse (Core.Maybe Types.Build)
ubrrsBuild = Lens.field @"build"
{-# DEPRECATED ubrrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrrsResponseStatus :: Lens.Lens' UpdateBuildResponse Core.Int
ubrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ubrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
